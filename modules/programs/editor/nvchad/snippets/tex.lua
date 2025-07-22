-- tex.lua: LuaSnip snippets for LaTeX

local ls = require("luasnip")
local s = ls.snippet
local t = ls.text_node
local i = ls.insert_node
local fmt = require("luasnip.extras.fmt").fmt

return {

  -- Basic environment template
  s("beg", fmt([[
    \begin{{{}}}
      {}
    \end{{{}}}
  ]], { i(1, "environment"), i(2), rep(1) })),

  -- Figure environment
  s("fig", fmt([[
    \begin{{figure}}[htbp]
      \centering
      \includegraphics[width=\linewidth]{{{}}}
      \caption{{{}}}
      \label{{fig:{}}}
    \end{{figure}}
  ]], { i(1, "filename.png"), i(2, "Caption here"), i(3, "label") })),

  -- Table environment
  s("tab", fmt([[
    \begin{{table}}[htbp]
      \centering
      \begin{{tabular}}{{{}}}
        \toprule
        {} \\
        \midrule
        {} \\
        \bottomrule
      \end{{tabular}}
      \caption{{{}}}
      \label{{tab:{}}}
    \end{{table}}
  ]], {
    i(1, "c c"),
    i(2, "Header1 & Header2"),
    i(3, "Value1 & Value2"),
    i(4, "Table caption"),
    i(5, "label")
  })),

  -- Math environment
  s("eq", fmt([[
    \begin{{equation}}
      {}
    \end{{equation}}
  ]], { i(1, "E = mc^2") })),

  -- Itemize environment
  s("item", fmt([[
    \begin{{itemize}}
      \item {}
      \item {}
    \end{{itemize}}
  ]], { i(1), i(2) })),

  -- Enumerate environment
  s("enum", fmt([[
    \begin{{enumerate}}
      \item {}
      \item {}
    \end{{enumerate}}
  ]], { i(1), i(2) })),

  -- Section
  s("sec", fmt([[ \section{{{}}} ]], { i(1, "Section Title") })),

  -- Subsection
  s("ssec", fmt([[ \subsection{{{}}} ]], { i(1, "Subsection Title") })),

  -- Label
  s("lbl", fmt([[ \label{{{}}} ]], { i(1, "label-name") })),

}
