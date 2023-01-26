import { useState } from "react"
import CodeMirror from "@uiw/react-codemirror"
import { oneDark } from "@codemirror/theme-one-dark"
import { StreamLanguage } from "@codemirror/language"
import { oCaml } from "@codemirror/legacy-modes/mode/mllike"
import initial from "../data/initialPrompt"
import "../ocaml/main"

interface TopLevelResult {
  types : string
  resultat: string
}
function App() {
  const [code, setCode] = useState(initial)
  const [types, setTypes] = useState('')
  const [print, setPrint] = useState('')
  const evalCode = () => {
    const codeToEvaluate = code + "\n ;;"
    const evaluation = ml.eval(codeToEvaluate) as TopLevelResult
    setTypes(evaluation.types)
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
          <CodeMirror
            onChange={(val, _) => setCode(val)}
            value={initial}
            height="100%"
            theme={oneDark}
            extensions={[StreamLanguage.define(oCaml)]}
            indentWithTab={true}
            className="editor"
          />
          <footer>
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
