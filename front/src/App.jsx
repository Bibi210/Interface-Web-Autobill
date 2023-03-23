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

function App() {
  const selectNode = useRef(null)
  const modeNode = useRef(null)
  const [code, setCode] = useState(billPrompts.lists)
  const [mode, setMode] = useState("LCBPV -> Equation")
  const [types, setTypes] = useState("")
  const [print, setPrint] = useState("")
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

  const lineHighlightField = StateField.define({
    create() {
      return Decoration.none
    },
    update(lines, tr) {
      lines = lines.map(tr.changes)
      if (dispatchSpec !== null) {
        let e = dispatchSpec.effects
        lines = lines.update({ add: [lineHighlightMark.range(e.value)] })
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
    let docPosition = view.state.doc.line(l).from
    setDispatchSpec({ effects: addLineHighlight.of(docPosition) })
  }
  function handleSelect() {
    let val = selectNode.current?.value
    setCode(billPrompts[val])
  }
  const evalCode = async (e) => {
    e.preventDefault()
    try {
      let evaluation = {
        resultat: "",
        erreur: ""
      }
      switch (mode) {
        case "Equation -> Soluce":
          const model = new Model()
          model.addFile("playground.mzn", code)
          const solve = await model.solve({
            options: {
              solver: "gecode",
            },
          })
          evaluation.resultat = solve.solution ? JSON.stringify(solve.solution.output.json) : "Insatisfiable"
          break
        case "LCBPV -> Equation":
          evaluation = ml.parse(code)
          break
        case "MiniML -> MiniML_AST":
          evaluation = ml.ast(code)
          break
        case "MiniML -> LCBPV_AST":
          evaluation = ml.translate(code)
          break
        case "MiniML -> Equation":
          evaluation = ml.mltoequation(code)
          break
        case "MiniML -> Interpretation":
          evaluation = ml.mlinterpretation(code)
          break
      }
      setDispatchSpec(null)
      setPrint(evaluation.resultat !== "" ? evaluation.resultat : "")
      setTypes(evaluation.erreur !== "" ? evaluation.erreur : "")
    } catch (error) {
      console.log(error)
      setPrint("")
      const err = JSON.parse(error[2].c)
      setTypes(err.text)
      highlight(err.line)
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
      <main>
        <section>
          <div ref={editor} />
          <footer>
            <div className="mode">
              <span>Mode</span>
              <select
                ref={modeNode}
                name=""
                id=""
                onChange={(e) => setMode(modeNode.current?.value)}
              >
                <option value={"LCBPV -> Equation"}>
                  {"LCPBV -> Equation"}
                </option>
                <option value={"MiniML -> MiniML_AST"}>
                  {"MiniML -> MiniML_AST"}
                </option>
                <option value={"MiniML -> LCBPV_AST"}>
                  {"MiniML -> LCBPV_AST"}
                </option>
                <option value={"MiniML -> Equation"}>
                  {"MiniML -> Equation"}
                </option>
                <option value={"MiniML -> Interpretation"}>
                  {"MiniML -> Interpretation"}
                </option>
                <option value={"Equation -> Soluce"}>
                  {"Equation -> Soluce"}
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
          {print ? <pre className="print">{print}</pre> : ""}
          {types ? <span className="types">{types}</span> : ""}
        </section>
      </main>
    </>
  )
}

export default App
