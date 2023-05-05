import { useEffect, useRef, useState } from "react"
import { useCodeMirror } from "@uiw/react-codemirror"
import { oneDark } from "@codemirror/theme-one-dark"
import { StreamLanguage } from "@codemirror/language"
import { lcbpv } from "../language/mllike"

import billPrompts from "../data/billPrompt"
import "../../MiniML/_build/default/bin/main.bc.js"
import { EditorView } from "codemirror"
import { StateField, StateEffect } from "@codemirror/state"
import { Decoration } from "@codemirror/view"
import { Model } from "https://cdn.jsdelivr.net/npm/minizinc/dist/minizinc.mjs"

const serverAvailability = () => {
  const res = new Set()
  res.add("MiniML -> Autobill")
  res.add("Equation -> Soluce")
  res.add("Equation -> Soluce (with Chuffed)")
  return res
}
function App() {
  const selectNode = useRef(null)
  const modeNode = useRef(null)
  const [server, setServer] = useState(false)
  const [code, setCode] = useState(billPrompts.playground)
  const [mode, setMode] = useState("MiniML -> MiniML_AST")
  const [types, setTypes] = useState("")
  const [print, setPrint] = useState("")
  const [ctrlPressed, setCtrl] = useState(false)
  const [dispatchSpec, setDispatchSpec] = useState(null)
  const editor = useRef(null)
  const addLineHighlight = StateEffect.define()

  const lineHighlightMark = Decoration.line({
    attributes: {
      style: "background-color: #ff00004f; cursor: pointer",
      "data-error": types,
    },
    class: "line-error",
  })

  const availableAtServer = serverAvailability()

  const lineHighlightField = StateField.define({
    create() {
      return Decoration.none
    },
    update(lines, tr) {
      lines = lines.map(tr.changes)
      if (dispatchSpec !== null) {
        let [s, e] = dispatchSpec.effects
        for(let i = s.value; i<= e.value; i++){
          lines = lines.update({
            add: [lineHighlightMark.range(i)],
          })
        }
      } else {
        for (let e of tr.effects) {
          if (e.is(addLineHighlight)) {
            lines = lines.update({ add: [lineHighlightMark.range(e.value)] })
          }
        }
      }
      return lines
    },
    provide: (f) => EditorView.decorations.from(f),
  })

  const { view } = useCodeMirror({
    container: editor.current,
    value: code,
    onChange: (val, _) => {
      setCode(val)
      setDispatchSpec(null)
    },
    height: "100%",
    maxWidth: "60vw",
    theme: oneDark,
    extensions: [
      StreamLanguage.define(lcbpv),
      EditorView.lineWrapping,
      lineHighlightField,
    ],
    indentWithTab: true,
    className: "editor",
    basicSetup: {
      syntaxHighlighting: true,
    },
  })
  function highlight(l) {
    let docPositionStart = view.state.doc.line(l.beginning.line).from
    let docPositionEnd = view.state.doc.line(l.end.line).from
    setDispatchSpec({
      effects: [
        addLineHighlight.of(docPositionStart),
        addLineHighlight.of(docPositionEnd),
      ],
    })
  }
  function handleSelect() {
    let val = selectNode.current?.value
    setCode(billPrompts[val])
  }
  function handleKeyDown(e) {
    if (e.key == "Meta") {
      setCtrl(true)
    } else {
      if (ctrlPressed && e.code == "Enter") {
        e.stopPropagation()
        evalCode(e)
      }
    }
  }
  function handleKeyUp(e) {
    if (e.key == "Meta") {
      setCtrl(false)
    }
  }
  const evalCode = async (e) => {
    e.preventDefault()
    try {
      let evaluation = {
        resultat: "",
        erreur: "",
      }
      switch (mode) {
        case "Equation -> Soluce":
          if (server) {
            const data = await fetch("http://localhost:3002/api/minizinc/gecode", {
              method: "POST",
              headers: {
                "Content-Type": "application/json; charset = UTF-8",
              },
              body: JSON.stringify({ code: code }),
            })
            evaluation = await data.json()
          } else {
            const model = new Model()
            model.addFile("playground.mzn", code)
            const solve = await model.solve({
              options: {
                solver: "gecode",
              },
            })
            console.log(solve)
            evaluation.resultat = solve.solution.output.default
          }
          break
          case "Equation -> Soluce (with Chuffed)":
            if (server) {
              const data = await fetch("http://localhost:3002/api/minizinc/chuffed", {
                method: "POST",
                headers: {
                  "Content-Type": "application/json; charset = UTF-8",
                },
                body: JSON.stringify({ code: code }),
              })
              evaluation = await data.json()
            }
            break
        case "MiniML -> MiniML_AST":
          evaluation = ml.ast(code)
          break
        case "MiniML -> LCBPV_AST":
          evaluation = ml.translate(code)
          break
        case "MiniML -> Autobill":
          if (server) {
            const data = await fetch("http://localhost:3002/api/run-code", {
              method: "POST",
              headers: {
                "Content-Type": "application/json; charset = UTF-8",
              },
              body: JSON.stringify({ code: code }),
            })
            evaluation = await data.json()
          } else {
            evaluation = ml.machine(code)
          }
          break
        case "MiniML -> Equation":
          evaluation = ml.mltoequation(code)
          break
        case "MiniML -> Autobill Typé":
          evaluation = ml.parse(code)
          break
      }
      setDispatchSpec(null)
      setPrint(evaluation.resultat !== "" ? evaluation.resultat : "")
      setTypes(evaluation.erreur !== "" ? evaluation.erreur : "")
    } catch (error) {
      setPrint("")
      if(availableAtServer.has(mode) && server){
        setTypes('Error: ' + error)
      } else{
        if(error[2].c){
          try{
            const err = JSON.parse(error[2].c)
            console.log(err)
            setTypes((err.phase ?? "") + ": " + err.info)
            if(err.loc!=false) {
              highlight(err.loc)
            }
          } catch (e){
            console.log(e)
            setTypes(error[2].c)
          }
        } else{
          setTypes(error[1].c)
        }

      }
    }
  }

  return (
    <>
      <header className="container">
        <nav>
          <div className="branding">
            <img src="/logo.png" alt="" />
            <h1>Autobill</h1>
          </div>
        </nav>
      </header>
      <main onKeyDown={(e) => handleKeyDown(e)} onKeyUp={(e) => handleKeyUp(e)}>
        <section>
          <div ref={editor} className="editor" />
          <footer>
            <div
              className="mode"
              style={{
                display: availableAtServer.has(mode) ? "flex" : "none",
              }}
            >
              <span>Server</span>
              <div className="switch__container">
                <input
                  onChange={() => setServer(!server)}
                  id="switch-shadow"
                  className="switch switch--shadow"
                  type="checkbox"
                />
                <label for="switch-shadow"></label>
              </div>
            </div>
            <div className="mode">
              <span>Mode</span>
              <select
                ref={modeNode}
                name=""
                id=""
                onChange={(e) => setMode(modeNode.current?.value)}
              >
                <option value={"MiniML -> MiniML_AST"}>
                  {"MiniML -> MiniML_AST"}
                </option>
                <option value={"MiniML -> LCBPV_AST"}>
                  {"MiniML -> LCBPV_AST"}
                </option>
                <option value={"MiniML -> Autobill"}>
                  {"MiniML -> Autobill"}
                </option>
                <option value={"MiniML -> Autobill Typé"}>
                  {"MiniML -> Autobill Typé"}
                </option>
                <option value={"MiniML -> Equation"}>
                  {"MiniML -> Equation"}
                </option>
                <option value={"Equation -> Soluce"}>
                  {"Equation -> Soluce"}
                </option>
                <option value={"Equation -> Soluce (with Chuffed)"}>
                  {"Equation -> Soluce (with Chuffed)"}
                </option>
              </select>
            </div>
            <div className="mode">
              <span>Programme</span>
              <select
                ref={selectNode}
                onChange={(e) => handleSelect()}
                name=""
                id=""
              >
                {Object.keys(billPrompts).map((elem, i) => (
                  <option key={i} value={elem}>
                    {elem}
                  </option>
                ))}
              </select>
            </div>
            <button onClick={(e) => evalCode(e)}>
              <span>Run</span>
              <span>⌘⏎</span>
            </button>
          </footer>
        </section>
        <section>
          <aside style={{ marginBottom: "1.5rem" }}>
            <span className="output">Output</span>
          </aside>
          <div style={{ paddingLeft: "1.1rem" }}>
            {print ? <pre className="print">{print}</pre> : ""}
            {types ? <span className="types">{types}</span> : ""}
          </div>
        </section>
      </main>
    </>
  )
}

export default App
