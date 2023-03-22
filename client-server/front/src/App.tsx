//@ts-ignore
import { useEffect, useRef, useState } from "react"
import { useCodeMirror } from "@uiw/react-codemirror"
import { oneDark } from "@codemirror/theme-one-dark"
import { StreamLanguage } from "@codemirror/language"
import { lcbpv } from "../language/mllike"

import billPrompts from "../data/billPrompt"
import "../../../MiniML_V2/_build/default/bin/main.bc.js"
import { EditorView } from "codemirror"
import { StateField, StateEffect } from "@codemirror/state"
import { Decoration } from "@codemirror/view"

interface TopLevelResult {
  types : string
  resultat: string
  erreurs: string
}
function App() {
  const selectNode = useRef<HTMLSelectElement>(null)
  const modeNode = useRef(null)

  const [code, setCode] = useState(billPrompts.lists)
  const [mode, setMode] = useState("LCBPV -> Equation")
  const [result, setResult] = useState(null);
  const [error, setError] = useState('');
  const [response, setResponse] = useState('');
  const [types, setTypes] = useState('')
  const [print, setPrint] = useState('')
  const [dispatchSpec, setDispatchSpec] = useState(null)
  const editor = useRef(null);
  let res = ""
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

  function handleSelect(){
    let val = selectNode.current?.value
    setCode(billPrompts[val])
  }

  function handleClick() {
    switch(mode){
      case "LCBPV -> Equation":{
        console.log("ready to send");
        fetch('http://localhost:3002/api/run-code', {
          method: 'POST',
          headers: {
            'Content-Type': 'application/json; charset = UTF-8'
          },
          body: JSON.stringify({ code: code }) 
        })
          .then(res => res.json())
          .then(data => {
            if (data.result) {
              const res = data.result.resultat;
              setResponse(res);
              setError("");
              console.log(res);
            } else {
              //console.error(data.error);
              setError(data.error);
              //console.log("error message:", data.error);
              console.log(error);
              setResponse("");
            }
          })
          .catch(error => {
            console.log("error detected");
            //console.error(error);
            if (error.json) {
              error.json().then((errorMessage: { error: string }) => {
                //console.log(errorMessage.error);
                setError(errorMessage.error);
                setResponse("")
              });
            }
          })
          .finally(() => {
            console.log("received result");
          });
        
          break
        }

        case "Minizinc":{
          console.log("ready to send");
          fetch('http://localhost:3002/api/minizinc', {
            method: 'POST',
            headers: {
              'Content-Type': 'application/json; charset = UTF-8'
            },
            body: JSON.stringify({ code: code }) 
          })
          .then(res => res.json())
          
          
          .then((data) => {
            let res = data.result;
            console.log(res);
            setResponse(res);
            setError('');
            
          })
          .catch((error) => {
            console.error('Error:', error);
            setResponse("unsatifable");

          });
          break
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
                <option value={"Minizinc"}>
                  {"Minizinc"}
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
            <button onClick={() => handleClick()}>
              <span>Run</span>
              <span>⌘⏎</span>
            </button>
          </footer>
        </section>
        <section>
          <aside>
            <span className="output">Output</span>
          </aside>
          {
            response!=='' ? 
            <pre className="print">
              {response}
            </pre>
            : ''
          }
          {
            error ? 
            <pre className="error">
              {error}
            </pre>
            : ''
          }
          {
            types!== '' ? 
            <pre className="types" >
              {res}
            </pre>
            : ''
          }
        </section>
      </main>
    </>
  )
}

export default App
