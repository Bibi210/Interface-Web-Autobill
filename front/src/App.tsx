//@ts-ignore
import { useRef, useState } from "react"
import CodeMirror, { useCodeMirror } from "@uiw/react-codemirror"
import { oneDark } from "@codemirror/theme-one-dark"
import { StreamLanguage } from "@codemirror/language"
import { lcbpv } from "../language/mllike"

import billPrompts from "../data/billPrompt"
import "../ocaml/main"
import { EditorView } from "codemirror"

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
  const [types, setTypes] = useState('')
  const [print, setPrint] = useState('')
  const editor = useRef(null);
  let res = ""
  const { state, setState, view } = useCodeMirror({
    container: editor.current,
    value: code,
    onChange: (val, _) => setCode(val),
    height: "100%",
    maxWidth:"60vw",
    theme: oneDark,
    extensions:[StreamLanguage.define(lcbpv), EditorView.lineWrapping],
    indentWithTab: true,
    className: "editor",
    basicSetup:{
      syntaxHighlighting: true,
    }
  });

  function handleSelect(){
    let val = selectNode.current?.value
    setCode(billPrompts[val])
  }

  const [response, setResponse] = useState('');
  function handleClick() {
    switch(mode){
      case "LCBPV -> Equation":{
        console.log("ready to send");
        fetch('http://localhost:3001/api/run-code', {
          method: 'POST',
          headers: {
            'Content-Type': 'application/json; charset = UTF-8'
          },
          body: JSON.stringify({ code: code }) 
        })
          .then(res => res.json())
          .then(data => {
            res = data.result.resultat;
            setResponse(res)
            console.log(res)
          })
          .catch(error => {console.log("error detected");
            console.error(error)});
          console.log("recieved result");
          break
        }
        case "Minizinc":{
          console.log("ready to send");
          fetch('http://localhost:3001/api/minizinc', {
            method: 'POST',
            headers: {
              'Content-Type': 'application/json; charset = UTF-8'
            },
            body: JSON.stringify({ code: code }) 
          })
          .then(res => res.json())
          
          .then(data => {
            res = data.result;
            setResponse(res)
            console.log(res)
          })
          .catch(error => {console.log("error detected");
            console.error(error)});
          console.log("recieved result");
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
            types ? 
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
