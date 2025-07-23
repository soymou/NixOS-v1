local ls = require("luasnip")
local s = ls.snippet
local t = ls.text_node
local i = ls.insert_node
local fmt = require("luasnip.extras.fmt").fmt
local rep = require("luasnip.extras").rep

-- context helpers (requires vimtex)
local in_mathzone = function()
  return vim.fn["vimtex#syntax#in_mathzone"]() == 1
end

local in_textzone = function()
  return not in_mathzone()
end

return {
  -- basic environment template
  s("beg", fmt([[
\begin{{{}}}
  {}
\end{{{}}}
]], { i(1, "environment"), i(2), rep(1) })),

  -- figure environment
  s("fig", fmt([[
\begin{{figure}}[htbp]
  \centering
  \includegraphics[width=\linewidth]{{{}}}
  \caption{{{}}}
  \label{{fig:{}}}
\end{{figure}}
]], {
    i(1, "filename.png"),
    i(2, "caption here"),
    i(3, "label"),
  })),

  -- table environment
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
    i(2, "header1 & header2"),
    i(3, "data1 & data2"),
    i(4, "table caption"),
    i(5, "label"),
  })),

  -- math environment
  s("eq", fmt([[
\begin{{equation}}
  {}
\end{{equation}}
]], { i(1, "e = mc^2") })),

  -- itemize
  s("item", fmt([[
\begin{{itemize}}
  \item {}
  \item {}
\end{{itemize}}
]], { i(1), i(2) })),

  -- enumerate
  s("enum", fmt([[
\begin{{enumerate}}
  \item {}
  \item {}
\end{{enumerate}}
]], { i(1), i(2) })),

  -- section
  s("sec", fmt([[\section{{{}}}]], { i(1, "section title") })),

  -- subsection
  s("ssec", fmt([[\subsection{{{}}}]], { i(1, "subsection title") })),

  -- label
  s("lbl", fmt([[\label{{{}}}]], { i(1, "label-name") })),

  -- display math
  s({ trig = "^dm$", regtrig = true, snippettype = "autosnippet" }, fmt([[
\[
  {}
\]
]], { i(1) })),

  -- inline math
  s({ trig = "^mk$", regtrig = true, snippettype = "autosnippet" }, fmt("\\( {} \\)", { i(1) })),

  -- \mathbb{}
  s({ trig = "^mbb$", regtrig = true, snippettype = "autosnippet" }, fmt("\\mathbb{{{}}}", { i(1) })),

  -- \mathcal{}
  s({ trig = "^mcal$", regtrig = true, snippettype = "autosnippet" }, fmt("\\mathcal{{{}}}", { i(1) })),

  -- bold (context-aware)
  s({ trig = "^bf$", regtrig = true, snippettype = "autosnippet", condition = in_textzone },
    fmt("\\textbf{{{}}}", { i(1) })),

  s({ trig = "^bf$", regtrig = true, snippettype = "autosnippet", condition = in_mathzone },
    fmt("\\mathbf{{{}}}", { i(1) })),

  -- italic (context-aware)
  s({ trig = "^if$", regtrig = true, snippettype = "autosnippet", condition = in_textzone },
    fmt("\\textit{{{}}}", { i(1) })),

  s({ trig = "^if$", regtrig = true, snippettype = "autosnippet", condition = in_mathzone },
    fmt("\\mathit{{{}}}", { i(1) })),

  -- tcolorboxes
  s("boxdef", fmt([[
\begin{{tcolorbox}}[
  colback=cyan!15,
  colframe=cyan!60,
  fonttitle=\sffamily\bfseries,
  title=\textsc{{{}}}
]
{}
\end{{tcolorbox}}
]], { i(1, "definition"), i(2) })),

  s("boxthe", fmt([[
\begin{{tcolorbox}}[
  colback=yellow!15,
  colframe=yellow!60,
  fonttitle=\sffamily\bfseries,
  title=\textsc{{{}}}
]
{}
\end{{tcolorbox}}
]], { i(1, "theorem"), i(2) })),

  s("boxobs", fmt([[
\begin{{tcolorbox}}[
  colback=gray!10,
  colframe=gray!50,
  fonttitle=\sffamily\bfseries,
  title=\textsc{{{}}}
]
{}
\end{{tcolorbox}}
]], { i(1, "observation"), i(2) })),

  s("boxlem", fmt([[
\begin{{tcolorbox}}[
  colback=green!15,
  colframe=green!60,
  fonttitle=\sffamily\bfseries,
  title=\textsc{{{}}}
]
{}
\end{{tcolorbox}}
]], { i(1, "lemma"), i(2) })),

  s("boxcor", fmt([[
\begin{{tcolorbox}}[
  colback=orange!15,
  colframe=orange!60,
  fonttitle=\sffamily\bfseries,
  title=\textsc{{{}}}
]
{}
\end{{tcolorbox}}
]], { i(1, "corollary"), i(2) })),

  s("boxex", fmt([[
\begin{{tcolorbox}}[
  colback=violet!15,
  colframe=violet!60,
  fonttitle=\sffamily\bfseries,
  title=\textsc{{{}}}
]
{}
\end{{tcolorbox}}
]], { i(1, "example"), i(2) })),
}

