import { useRef, useState } from "react"
import { useCodeMirror } from "@uiw/react-codemirror"
import { oneDark } from "@codemirror/theme-one-dark"
import { StreamLanguage } from "@codemirror/language"
import { lcbpv } from "../language/mllike"

import billPrompts from "../data/billPrompt"
import "../../MiniML_V2/_build/default/bin/main.bc.js"
import { EditorView } from "codemirror"

/* interface TopLevelResult {
  types : string
  resultat: string
  erreurs: string
} */
function App() {
  const selectNode = useRef(null)
  const [code, setCode] = useState(billPrompts.lists)

  const [types, setTypes] = useState('lists')
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
    setCode(billPrompts[val])
    setTypes(val)
  }
  const evalCode = () => {
    let evaluation
    if(types==='lists'){
      evaluation = ml.parse(code)
    } else{
      evaluation = ml.ast(code)
    }
    setPrint(evaluation.resultat)
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

        </section>
      </main>
    </>
  )
}

export default App
