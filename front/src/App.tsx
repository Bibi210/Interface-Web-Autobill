import { useState } from "react"
import CodeMirror from "@uiw/react-codemirror"
import { oneDark } from "@codemirror/theme-one-dark"
import { StreamLanguage } from "@codemirror/language"
import { oCaml } from "@codemirror/legacy-modes/mode/mllike"
import initial from "../data/initialPrompt"
import "../ocaml/main.bc"
function App() {
  const [code, setCode] = useState(initial)
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
            <button onClick={() => console.log(myMathLib.add(3, 4))}>
              <span>Run</span>
              <span>⌘⏎</span>
            </button>
          </footer>
        </section>
        <section>
          <aside>
            <span className="output">Output</span>
          </aside>
        </section>
      </main>
    </>
  )
}

export default App
