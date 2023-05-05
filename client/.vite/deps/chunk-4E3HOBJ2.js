import {
  HighlightStyle,
  syntaxHighlighting,
  tags
} from "./chunk-NXIPFAN2.js";
import {
  EditorView
} from "./chunk-2WOFAUPF.js";

// node_modules/@codemirror/theme-one-dark/dist/index.js
var chalky = "#e5c07b";
var coral = "#e06c75";
var cyan = "#56b6c2";
var invalid = "#ffffff";
var ivory = "#abb2bf";
var stone = "#7d8799";
var malibu = "#61afef";
var sage = "#98c379";
var whiskey = "#d19a66";
var violet = "#c678dd";
var darkBackground = "#21252b";
var highlightBackground = "#2c313a";
var background = "#282c34";
var tooltipBackground = "#353a42";
var selection = "#3E4451";
var cursor = "#528bff";
var color = {
  chalky,
  coral,
  cyan,
  invalid,
  ivory,
  stone,
  malibu,
  sage,
  whiskey,
  violet,
  darkBackground,
  highlightBackground,
  background,
  tooltipBackground,
  selection,
  cursor
};
var oneDarkTheme = EditorView.theme({
  "&": {
    color: ivory,
    backgroundColor: background
  },
  ".cm-content": {
    caretColor: cursor
  },
  ".cm-cursor, .cm-dropCursor": { borderLeftColor: cursor },
  "&.cm-focused > .cm-scroller > .cm-selectionLayer .cm-selectionBackground, .cm-selectionBackground, .cm-content ::selection": { backgroundColor: selection },
  ".cm-panels": { backgroundColor: darkBackground, color: ivory },
  ".cm-panels.cm-panels-top": { borderBottom: "2px solid black" },
  ".cm-panels.cm-panels-bottom": { borderTop: "2px solid black" },
  ".cm-searchMatch": {
    backgroundColor: "#72a1ff59",
    outline: "1px solid #457dff"
  },
  ".cm-searchMatch.cm-searchMatch-selected": {
    backgroundColor: "#6199ff2f"
  },
  ".cm-activeLine": { backgroundColor: "#6699ff0b" },
  ".cm-selectionMatch": { backgroundColor: "#aafe661a" },
  "&.cm-focused .cm-matchingBracket, &.cm-focused .cm-nonmatchingBracket": {
    backgroundColor: "#bad0f847"
  },
  ".cm-gutters": {
    backgroundColor: background,
    color: stone,
    border: "none"
  },
  ".cm-activeLineGutter": {
    backgroundColor: highlightBackground
  },
  ".cm-foldPlaceholder": {
    backgroundColor: "transparent",
    border: "none",
    color: "#ddd"
  },
  ".cm-tooltip": {
    border: "none",
    backgroundColor: tooltipBackground
  },
  ".cm-tooltip .cm-tooltip-arrow:before": {
    borderTopColor: "transparent",
    borderBottomColor: "transparent"
  },
  ".cm-tooltip .cm-tooltip-arrow:after": {
    borderTopColor: tooltipBackground,
    borderBottomColor: tooltipBackground
  },
  ".cm-tooltip-autocomplete": {
    "& > ul > li[aria-selected]": {
      backgroundColor: highlightBackground,
      color: ivory
    }
  }
}, { dark: true });
var oneDarkHighlightStyle = HighlightStyle.define([
  {
    tag: tags.keyword,
    color: violet
  },
  {
    tag: [tags.name, tags.deleted, tags.character, tags.propertyName, tags.macroName],
    color: coral
  },
  {
    tag: [tags.function(tags.variableName), tags.labelName],
    color: malibu
  },
  {
    tag: [tags.color, tags.constant(tags.name), tags.standard(tags.name)],
    color: whiskey
  },
  {
    tag: [tags.definition(tags.name), tags.separator],
    color: ivory
  },
  {
    tag: [tags.typeName, tags.className, tags.number, tags.changed, tags.annotation, tags.modifier, tags.self, tags.namespace],
    color: chalky
  },
  {
    tag: [tags.operator, tags.operatorKeyword, tags.url, tags.escape, tags.regexp, tags.link, tags.special(tags.string)],
    color: cyan
  },
  {
    tag: [tags.meta, tags.comment],
    color: stone
  },
  {
    tag: tags.strong,
    fontWeight: "bold"
  },
  {
    tag: tags.emphasis,
    fontStyle: "italic"
  },
  {
    tag: tags.strikethrough,
    textDecoration: "line-through"
  },
  {
    tag: tags.link,
    color: stone,
    textDecoration: "underline"
  },
  {
    tag: tags.heading,
    fontWeight: "bold",
    color: coral
  },
  {
    tag: [tags.atom, tags.bool, tags.special(tags.variableName)],
    color: whiskey
  },
  {
    tag: [tags.processingInstruction, tags.string, tags.inserted],
    color: sage
  },
  {
    tag: tags.invalid,
    color: invalid
  }
]);
var oneDark = [oneDarkTheme, syntaxHighlighting(oneDarkHighlightStyle)];

export {
  color,
  oneDarkTheme,
  oneDarkHighlightStyle,
  oneDark
};
//# sourceMappingURL=chunk-4E3HOBJ2.js.map
