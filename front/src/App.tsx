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
  const [code, setCode] = useState(billPrompts.lists)

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
  const evalCode = () => {
    const evaluation = ml.parse(code) as TopLevelResult
    console.log(evaluation)
    setPrint(evaluation.resultat)
    // setTypes(evaluation.types)
    // setPrint(evaluation.resultat == "" ? evaluation.erreurs : evaluation.resultat)
  }
  const [response, setResponse] = useState('');
  function handleClick() {
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
            <select ref={selectNode} onChange={(e) => handleSelect()} name="" id="">
              {
                Object.keys(billPrompts).map((elem,i) => (
                  <option key={i} value={elem}>{elem}</option>
                ))
              }
            </select>
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
