import {
  autocompletion,
  closeBrackets,
  closeBracketsKeymap,
  completionKeymap,
  defaultKeymap,
  highlightSelectionMatches,
  history,
  historyKeymap,
  lintKeymap,
  searchKeymap
} from "./chunk-SIH2XR4N.js";
import {
  bracketMatching,
  defaultHighlightStyle,
  foldGutter,
  foldKeymap,
  indentOnInput,
  syntaxHighlighting
} from "./chunk-NXIPFAN2.js";
import {
  EditorView,
  crosshairCursor,
  drawSelection,
  dropCursor,
  highlightActiveLine,
  highlightActiveLineGutter,
  highlightSpecialChars,
  keymap,
  lineNumbers,
  rectangularSelection
} from "./chunk-2WOFAUPF.js";
import {
  EditorState
} from "./chunk-N62YFMSC.js";
import "./chunk-DFKQJ226.js";

// node_modules/codemirror/dist/index.js
var basicSetup = (() => [
  lineNumbers(),
  highlightActiveLineGutter(),
  highlightSpecialChars(),
  history(),
  foldGutter(),
  drawSelection(),
  dropCursor(),
  EditorState.allowMultipleSelections.of(true),
  indentOnInput(),
  syntaxHighlighting(defaultHighlightStyle, { fallback: true }),
  bracketMatching(),
  closeBrackets(),
  autocompletion(),
  rectangularSelection(),
  crosshairCursor(),
  highlightActiveLine(),
  highlightSelectionMatches(),
  keymap.of([
    ...closeBracketsKeymap,
    ...defaultKeymap,
    ...searchKeymap,
    ...historyKeymap,
    ...foldKeymap,
    ...completionKeymap,
    ...lintKeymap
  ])
])();
var minimalSetup = (() => [
  highlightSpecialChars(),
  history(),
  drawSelection(),
  syntaxHighlighting(defaultHighlightStyle, { fallback: true }),
  keymap.of([
    ...defaultKeymap,
    ...historyKeymap
  ])
])();
export {
  EditorView,
  basicSetup,
  minimalSetup
};
//# sourceMappingURL=codemirror.js.map
