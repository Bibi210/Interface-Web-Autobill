import React, { createRef, useEffect, useRef, useState } from "react";
const CodeEditor = () => {
  const [lineToFocus, setLineToFocus] = useState(-1);
  const [text, setText] = useState([
    { index: 1, indent: 1, content: "Hello World" },
  ]);
  const lineRefs = useRef<Array<HTMLDivElement | null>>([])
  const handleNewLine = (e: KeyboardEventInit) => {
    if (e.isComposing || e.key === "Enter") {
      const old = [...text];
      old.push({ index: text.length + 1, indent: 1, content: "" });
      setText(old);
    }
  };
  useEffect(() => {
    console.log("Zyzz")
    lineRefs.current = lineRefs.current.slice(0, text.length +1 )
  }, [text])
  return (
    <div className="editor">
      {text.map((t, i) => {
        return (
          <div
            ref={el => lineRefs.current[i] = el}
            className={"codeLine"}
            onKeyDownCapture={(e) => handleNewLine(e)}
            onFocusCapture={() => lineRefs.current[i]?.classList.add('focused')}
            onBlurCapture={() => lineRefs.current[i]?.classList.remove('focused')}
            onClick={() => console.log(lineRefs.current)}
          >
          <span className={"lineNumber"}>{t.index}</span>
          {Array(t.indent)
            .fill()
            .map((a) => {
              return <span key={a} className="indentBlock"></span>;
            })}
          <span contentEditable={true}>
            {t.content}
          </span>
        </div>
        );
      })}
    </div>
  );
};

export default CodeEditor;
