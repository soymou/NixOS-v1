local ls = require("luasnip")
local s = ls.snippet
local t = ls.text_node
local i = ls.insert_node
local fmt = require("luasnip.extras.fmt").fmt
local rep = require("luasnip.extras").rep

-- Context helpers (requires vimtex)
local in_mathzone = function()
  return vim.fn["vimtex#syntax#in_mathzone"]() == 1
end

local in_textzone = function()
  return not in_mathzone()
end

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

  -- Display math environment
  s({ trig = "dm$", regTrig = true, snippetType = "autosnippet" }, fmt([[
\[
  {}
\]
]], { i(1) })),

  -- Inline math
  s({ trig = "mk", snippetType = "autosnippet" }, fmt("\\( {} \\)", { i(1) })),

  -- \mathbb{}
  s({ trig = "mbb", snippetType = "autosnippet" }, fmt("\\mathbb{{{}}}", { i(1) })),

  -- \mathcal{}
  s({ trig = "mcal", snippetType = "autosnippet" }, fmt("\\mathcal{{{}}}", { i(1) })),

  -- Bold (context-aware)
  s({ trig = "bf", snippetType = "autosnippet", condition = in_textzone },
    fmt("\\textbf{{{}}}", { i(1) })),

  s({ trig = "bf", snippetType = "autosnippet", condition = in_mathzone },
    fmt("\\mathbf{{{}}}", { i(1) })),

  -- Italic (context-aware)
  s({ trig = "itf", snippetType = "autosnippet", condition = in_textzone },
    fmt("\\textit{{{}}}", { i(1) })),

  s({ trig = "itf", snippetType = "autosnippet", condition = in_mathzone },
    fmt("\\mathit{{{}}}", { i(1) })),
}
