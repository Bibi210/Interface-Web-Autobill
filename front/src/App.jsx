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
  const modeNode = useRef(null)
  const [code, setCode] = useState(billPrompts.lists)

  const [mode, setMode] = useState("LCBPV -> Equation")
  const [types, setTypes] = useState("")
  const [print, setPrint] = useState("")
  const editor = useRef(null)
  const { state, setState, view } = useCodeMirror({
    container: editor.current,
    value: code,
    onChange: (val, _) => setCode(val),
    height: "100%",
    maxWidth: "60vw",
    theme: oneDark,
    extensions: [StreamLanguage.define(lcbpv), EditorView.lineWrapping],
    indentWithTab: true,
    className: "editor",
    basicSetup: {
      syntaxHighlighting: true,
    },
  })
  function handleSelect() {
    let val = selectNode.current?.value
    setCode(billPrompts[val])
  }
  const evalCode = () => {
    try {
      let evaluation
      switch (mode) {
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
          evaluation = ml.interprete(code)
          break
      }
      console.log(evaluation)
      setPrint(evaluation.resultat !== "" ? evaluation.resultat : "")
      setTypes(evaluation.erreur !== "" ? evaluation.erreur : "")
    } catch (error) {
      setTypes(error[2].c)
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
            <button onClick={() => evalCode()}>
              <span>Run</span>
              <span>??????</span>
            </button>
          </footer>
        </section>
        <section>
          <aside>
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
