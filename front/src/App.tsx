import { useRef, useState } from "react"
import CodeMirror, { useCodeMirror } from "@uiw/react-codemirror"
import { oneDark } from "@codemirror/theme-one-dark"
import { StreamLanguage } from "@codemirror/language"
import { lcbpv } from "../language/mllike"

import initial from "../data/initialPrompt"
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
  const [code, setCode] = useState(initial)

  const [types, setTypes] = useState('')
  const [print, setPrint] = useState('')
  const editor = useRef(null);
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
    if(val!="Ocaml"){
      val = val as string
      setCode(billPrompts[val])
    } else{
      setCode(initial)
    }
  }
  const evalCode = () => {
    const codeToEvaluate = code + "\n ;;"
    const evaluation = ml.eval(codeToEvaluate) as TopLevelResult
    setTypes(evaluation.types)
    setPrint(evaluation.resultat == "" ? evaluation.erreurs : evaluation.resultat)
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
              <option value="Ocaml" defaultChecked>Exemple Ocaml</option>
              {
                Object.keys(billPrompts).map((elem,i) => (
                  <option key={i} value={elem}>{elem}</option>
                ))
              }
            </select>
            <button onClick={() => evalCode()}>
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
            print ? 
            <pre className="print">
              {print}
            </pre>
            : ''
          }
          {
            types ? 
            <pre className="types" >
              {types}
            </pre>
            : ''
          }
        </section>
      </main>
    </>
  )
}

export default App
