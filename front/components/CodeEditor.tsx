import React, {
  ChangeEvent,
  createRef,
  useEffect,
  useRef,
  useState,
} from "react"
import CodeMirror from '@uiw/react-codemirror';
import { oneDark } from '@codemirror/theme-one-dark'
import { StreamLanguage } from '@codemirror/language';
import { oCaml }from '@codemirror/legacy-modes/mode/mllike'
const CodeEditor = () => {
  return(
  <>
  <CodeMirror
      value="console.log('hello world!');"
      height="100%"
      theme={oneDark}
      extensions={[StreamLanguage.define(oCaml)]}
      indentWithTab={true}
      className='editor'
    />
  </>
  )
}

export default CodeEditor
