function mLlike(parserConfig) {
  var words = parserConfig.extraWords;
  var hintWords = [];
  for (var k in words) { hintWords.push(k); }

  function tokenBase(stream, state) {
    var ch = stream.next();

    if (ch === '"') {
      state.tokenize = tokenString;
      return state.tokenize(stream, state);
    }
    if (ch === '{') {
      if (stream.eat('|')) {
        state.longString = true;
        state.tokenize = tokenLongString;
        return state.tokenize(stream, state);
      }
    }
    if (ch === '(') {
      if (stream.match(/^\*(?!\))/)) {
        state.commentLevel++;
        state.tokenize = tokenComment;
        return state.tokenize(stream, state);
      }
    }
    if (ch === '~' || ch === '?') {
      stream.eatWhile(/\w/);
      return 'variableName.special';
    }
    if (ch === '`') {
      stream.eatWhile(/\w/);
      return 'quote';
    }
    if (ch === '/' && parserConfig.slashComments && stream.eat('/')) {
      stream.skipToEnd();
      return 'comment';
    }
    if (/\d/.test(ch)) {
      if (ch === '0' && stream.eat(/[bB]/)) {
        stream.eatWhile(/[01]/);
      } if (ch === '0' && stream.eat(/[xX]/)) {
        stream.eatWhile(/[0-9a-fA-F]/)
      } if (ch === '0' && stream.eat(/[oO]/)) {
        stream.eatWhile(/[0-7]/);
      } else {
        stream.eatWhile(/[\d_]/);
        if (stream.eat('.')) {
          stream.eatWhile(/[\d]/);
        }
        if (stream.eat(/[eE]/)) {
          stream.eatWhile(/[\d\-+]/);
        }
      }
      return 'number';
    }
    if ( /[+\-*&%=<>!?|@\.~:]/.test(ch)) {
      return 'operator';
    }
    if (/[\w\xa1-\uffff]/.test(ch)) {
      stream.eatWhile(/[\w\xa1-\uffff]/);
      var cur = stream.current();
      return words.hasOwnProperty(cur) ? words[cur] : 'variable';
    }
    return null
  }

  function tokenString(stream, state) {
    var next, end = false, escaped = false;
    while ((next = stream.next()) != null) {
      if (next === '"' && !escaped) {
        end = true;
        break;
      }
      escaped = !escaped && next === '\\';
    }
    if (end && !escaped) {
      state.tokenize = tokenBase;
    }
    return 'string';
  };

  function tokenComment(stream, state) {
    var prev, next;
    while(state.commentLevel > 0 && (next = stream.next()) != null) {
      if (prev === '/' && next === '/') state.commentLevel++;
      if (prev === '*' && next === ')') state.commentLevel--;
      prev = next;
    }
    if (state.commentLevel <= 0) {
      state.tokenize = tokenBase;
    }
    return 'comment';
  }

  function tokenLongString(stream, state) {
    var prev, next;
    while (state.longString && (next = stream.next()) != null) {
      if (prev === '|' && next === '}') state.longString = false;
      prev = next;
    }
    if (!state.longString) {
      state.tokenize = tokenBase;
    }
    return 'string';
  }

  return {
    startState: function() {return {tokenize: tokenBase, commentLevel: 0, longString: false};},
    token: function(stream, state) {
      if (stream.eatSpace()) return null;
      return state.tokenize(stream, state);
    },

    languageData: {
      autocomplete: hintWords,
      commentTokens: {
        line: parserConfig.slashComments ? "//" : undefined,
        block: {open: "(*", close: "*)"}
      }
    }
  };
};

export const miniML = mLlike({
  name: "bill",
  extraWords: {
    'else': 'keyword',
    'fun': 'keyword',
    'if': 'keyword',
    'in': 'keyword',
    'let': 'keyword',
    'of': 'keyword',
    'rec': 'keyword',
    'then': 'keyword',
    'type': 'keyword',
    'with': 'keyword',
    'and': 'keyword',
    'or': 'keyword',
    'not': 'keyword',
    
    // builtins
    'true': 'builtin',
    'false': 'builtin',

    // Pervasives builtins
    '||': 'builtin',
    '&&': 'builtin',
    
    // types
    'int' : 'type', 
    'bool': 'type', 
    'unit' : 'type'
  }
});