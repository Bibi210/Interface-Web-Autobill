import React, {
  ChangeEvent,
  createRef,
  useEffect,
  useRef,
  useState,
} from "react"
const CodeEditor = () => {
  const [col, setCol] = useState(1)
  const [lineToFocus, setLineToFocus] = useState(1)
  const [text, setText] = useState([{ indent: 1, content: "Hello World" }])
  const lineRefs = useRef<Array<HTMLSpanElement | null>>([])
  const handleCursorPosition = (isLeft: boolean | null) => {
    let offset = window.getSelection()?.focusOffset ?? 0
    if(isLeft!=null && isLeft){
      const range = window.getSelection()?.rangeCount ?? 0
      offset = window.getSelection()?.anchorOffset ?? 0
      if(col>1){
        setCol(offset - range + 1)
      }
      return
    }
    if(isLeft!=null && !isLeft){
        if(col<=text[lineToFocus].content.length){
          setCol(offset + 2)
        }
        return
    }
    else{
      setCol(offset + 1)
    }
  }

  const handleCursorMovement = (e: KeyboardEvent) => {
    if (e.isComposing || e.key === "ArrowLeft") {
      handleCursorPosition(true)
      return
    }
    if (e.isComposing || e.key === "ArrowRight") {
      handleCursorPosition(false)
      return
    }
    if (e.isComposing || e.key === "Enter") {
      e.preventDefault()
      console.log(lineToFocus)
      const old = [...text]
      const first_part = old.slice(0, lineToFocus + 1)
      first_part.push({ indent: 1, content: "" })
      setText(first_part.concat(old.slice(lineToFocus + 1, old.length)))
      setCol(1)
      return
    }
    if (e.isComposing || (e.key === "ArrowUp" && lineToFocus > 0)) {
      e.preventDefault()
      lineRefs.current[lineToFocus]?.blur()
      lineRefs.current[lineToFocus - 1]?.focus()
      setLineToFocus(lineToFocus - 1)
      return
    }
    if (
      e.isComposing ||
      (e.key === "ArrowDown" && lineToFocus < text.length - 1)
    ) {
      e.preventDefault()
      lineRefs.current[lineToFocus]?.blur()
      lineRefs.current[lineToFocus + 1]?.focus()
      setLineToFocus(lineToFocus + 1)
      return
    }
    if (e.isComposing || (e.key === "Backspace" && col == 0)) {
      e.preventDefault()
      const newText = [...text]
      newText.filter((t, i) => i !== lineToFocus)
      setText(newText)
      lineRefs.current[lineToFocus]?.blur()
      lineRefs.current[lineToFocus - 1]?.focus()
      setLineToFocus(lineToFocus - 1)
      return
    } else {
      e.preventDefault()
      const newText = [...text]
      let first_part = newText[lineToFocus].content.slice(0, col+1)
      const second_part = newText[lineToFocus].content.slice(
        col + 1,
        newText[lineToFocus].content.length
      )
      if (e.isComposing || e.key === "Backspace") {
        first_part.slice(0, first_part.length - 1)
      } else {
        if (String.fromCharCode(e.keyCode).match(/(\w|\s)/g)) {
          first_part = first_part + e.key
        }
      }
      newText[lineToFocus].content = first_part + second_part
      setText(newText)
    }
  }
  const handleNewFocus = (i: number) => {
    lineRefs.current[i]?.classList.add("focused")
    setLineToFocus(i)
  }
  const handleChange = (i: number, e: ChangeEvent) => {
    console.log(e)
  }
  useEffect(() => {
    if (lineRefs.current.length != text.length) {
      lineRefs.current = lineRefs.current.slice(0, text.length + 1)
      lineRefs.current[lineToFocus]?.blur()
      lineRefs.current[lineToFocus + 1]?.focus()
    }
  }, [text])
  return (
    <div className="editor">
      <div>
        Ln {lineToFocus + 1}, Col {col}
      </div>
      {text.map((t, i) => {
        return (
          <div
            key={i}
            className={"codeLine"}
            onKeyDownCapture={(e) => handleCursorMovement(e)}
            onFocusCapture={() => handleNewFocus(i)}
            onBlurCapture={() =>
              lineRefs.current[i]?.classList.remove("focused")
            }
            onClick={() => handleCursorPosition(null)}
            onChange={(e) => console.log(window.getSelection())}
          >
            <span className={"lineNumber"}>{i + 1}</span>
            {Array(t.indent)
              .fill()
              .map((a, i) => {
                return <span key={i} className="indentBlock"></span>
              })}
            <span
              ref={(el) => (lineRefs.current[i] = el)}
              style={{ width: "100%" }}
              suppressContentEditableWarning={true}
              contentEditable={true}
              onChange={(e) => handleChange(i, e)}
            >
              {t.content}
            </span>
          </div>
        )
      })}
    </div>
  )
}

export default CodeEditor
