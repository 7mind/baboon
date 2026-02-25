import * as monaco from "monaco-editor";

export const BABOON_LANGUAGE_ID = "baboon";

const monarchLanguage: monaco.languages.IMonarchLanguage = {
  defaultToken: "",
  tokenPostfix: ".baboon",

  keywords: [
    "model",
    "version",
    "include",
    "import",
    "pragma",
    "data",
    "struct",
    "adt",
    "enum",
    "choice",
    "contract",
    "foreign",
    "service",
    "ns",
    "root",
    "with",
    "is",
    "was",
    "derived",
    "without",
  ],

  builtinTypes: [
    "bit",
    "i08",
    "i16",
    "i32",
    "i64",
    "u08",
    "u16",
    "u32",
    "u64",
    "f32",
    "f64",
    "f128",
    "str",
    "uid",
    "tsu",
    "tso",
    "map",
    "opt",
    "lst",
    "set",
  ],

  operators: ["+", "-", "^", "=", ":"],

  symbols: /[=><!~?:&|+\-*/^%]+/,

  escapes: /\\(?:[btnfr"'\\]|u[0-9A-Fa-f]{4}|[0-9]{1,3})/,

  tokenizer: {
    root: [
      // Block comments (must come before line comments)
      [/\/\*\*(?!\/)/, "comment.doc", "@docComment"],
      [/\/\*/, "comment", "@blockComment"],

      // Line comments
      [/\/\/.*$/, "comment"],

      // Strings
      [/"/, "string", "@string"],

      // Structural inheritance operators at start of line
      [/^\s*[+\-^]/, "keyword"],

      // Numeric literals (for enum values)
      [/-?[0-9]+/, "number"],

      // Identifiers and keywords
      [
        /[A-Za-z_][A-Za-z_0-9]*/,
        {
          cases: {
            "@keywords": "keyword",
            "@builtinTypes": "type",
            "@default": "identifier",
          },
        },
      ],

      // Dotted identifiers (package names)
      [/\./, "delimiter"],

      // Brackets
      [/[{}()[\]]/, "@brackets"],

      // Operators
      [/[=:]/, "delimiter"],

      // Whitespace
      [/\s+/, "white"],
    ],

    string: [
      [/@escapes/, "string.escape"],
      [/[^"\\]+/, "string"],
      [/"/, "string", "@pop"],
    ],

    blockComment: [
      [/\/\*/, "comment", "@push"],
      [/\*\//, "comment", "@pop"],
      [/./, "comment"],
    ],

    docComment: [
      [/\/\*/, "comment.doc", "@push"],
      [/\*\//, "comment.doc", "@pop"],
      [/\[\[/, "string.link"],
      [/\]\]/, "string.link"],
      [/./, "comment.doc"],
    ],
  },
};

const languageConfiguration: monaco.languages.LanguageConfiguration = {
  comments: {
    lineComment: "//",
    blockComment: ["/*", "*/"],
  },
  brackets: [
    ["{", "}"],
    ["[", "]"],
    ["(", ")"],
  ],
  autoClosingPairs: [
    { open: "{", close: "}" },
    { open: "[", close: "]" },
    { open: "(", close: ")" },
    { open: '"', close: '"' },
  ],
  surroundingPairs: [
    { open: "{", close: "}" },
    { open: "[", close: "]" },
    { open: "(", close: ")" },
    { open: '"', close: '"' },
  ],
};

export function registerBaboonLanguage(): void {
  monaco.languages.register({
    id: BABOON_LANGUAGE_ID,
    extensions: [".baboon", ".bmo"],
    aliases: ["Baboon", "baboon"],
  });
  monaco.languages.setMonarchTokensProvider(BABOON_LANGUAGE_ID, monarchLanguage);
  monaco.languages.setLanguageConfiguration(
    BABOON_LANGUAGE_ID,
    languageConfiguration
  );
}
