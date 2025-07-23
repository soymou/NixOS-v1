
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
\begin{figure}[htbp]
  \centering
  \includegraphics[width=\linewidth]{{{}}}
  \caption{{{}}}
  \label{{fig:{}}}
\end{figure}
]], { i(1, "filename.png"), i(2, "Caption here"), i(3, "label") })),

  -- Table environment
  s("tab", fmt([[
\begin{table}[htbp]
  \centering
  \begin{tabular}{ {} }
    \toprule
    {} \\
    \midrule
    {} \\
    \bottomrule
  \end{tabular}
  \caption{{{}}}
  \label{{tab:{}}}
\end{table}
]], {
    i(1, "c c"),
    i(2, "Header1 & Header2"),
    i(3, "Value1 & Value2"),
    i(4, "Table caption"),
    i(5, "label")
  })),

  -- Math environment
  s("eq", fmt([[
\begin{equation}
  {}
\end{equation}
]], { i(1, "E = mc^2") })),

  -- Itemize environment
  s("item", fmt([[
\begin{itemize}
  \item {}
  \item {}
\end{itemize}
]], { i(1), i(2) })),

  -- Enumerate environment
  s("enum", fmt([[
\begin{enumerate}
  \item {}
  \item {}
\end{enumerate}
]], { i(1), i(2) })),

  -- Section
  s("sec", fmt([[\section{{{}}}]], { i(1, "Section Title") })),

  -- Subsection
  s("ssec", fmt([[\subsection{{{}}}]], { i(1, "Subsection Title") })),

  -- Label
  s("lbl", fmt([[\label{{{}}}]], { i(1, "label-name") })),

  -- Display math environment
  s({ trig = "^dm$", regTrig = true, snippetType = "autosnippet" }, fmt([[
\[
  {}
\]
]], { i(1) })),

  -- Inline math
  s({ trig = "^mk$", regTrig = true, snippetType = "autosnippet" }, fmt("\\( {} \\)", { i(1) })),

  -- \mathbb{}
  s({ trig = "^mbb$", regTrig = true, snippetType = "autosnippet" }, fmt("\\mathbb{{{}}}", { i(1) })),

  -- \mathcal{}
  s({ trig = "^mcal$", regTrig = true, snippetType = "autosnippet" }, fmt("\\mathcal{{{}}}", { i(1) })),

  -- Bold (context-aware)
  s({ trig = "^bf$", regTrig = true, snippetType = "autosnippet", condition = in_textzone },
    fmt("\\textbf{{{}}}", { i(1) })),

  s({ trig = "^bf$", regTrig = true, snippetType = "autosnippet", condition = in_mathzone },
    fmt("\\mathbf{{{}}}", { i(1) })),

  -- Italic (context-aware)
  s({ trig = "^if$", regTrig = true, snippetType = "autosnippet", condition = in_textzone },
    fmt("\\textit{{{}}}", { i(1) })),

  s({ trig = "^if$", regTrig = true, snippetType = "autosnippet", condition = in_mathzone },
    fmt("\\mathit{{{}}}", { i(1) })),

  -- Colored tcolorbox environments

  s("boxdef", fmt([[
\begin{{tcolorbox}}[
  colback=cyan!15,
  colframe=cyan!60,
  fonttitle=\sffamily\bfseries,
  title=\textsc{{{}}}
]
{}
\end{{tcolorbox}}
]], { i(1, "Definition"), i(2) })),

  s("boxthe", fmt([[
\begin{{tcolorbox}}[
  colback=yellow!15,
  colframe=yellow!60,
  fonttitle=\sffamily\bfseries,
  title=\textsc{{{}}}
]
{}
\end{{tcolorbox}}
]], { i(1, "Theorem"), i(2) })),

  s("boxobs", fmt([[
\begin{{tcolorbox}}[
  colback=gray!10,
  colframe=gray!50,
  fonttitle=\sffamily\bfseries,
  title=\textsc{{{}}}
]
{}
\end{{tcolorbox}}
]], { i(1, "Observation"), i(2) })),

  s("boxlem", fmt([[
\begin{{tcolorbox}}[
  colback=green!15,
  colframe=green!60,
  fonttitle=\sffamily\bfseries,
  title=\textsc{{{}}}
]
{}
\end{{tcolorbox}}
]], { i(1, "Lemma"), i(2) })),

  s("boxcor", fmt([[
\begin{{tcolorbox}}[
  colback=orange!15,
  colframe=orange!60,
  fonttitle=\sffamily\bfseries,
  title=\textsc{{{}}}
]
{}
\end{{tcolorbox}}
]], { i(1, "Corollary"), i(2) })),

  s("boxex", fmt([[
\begin{{tcolorbox}}[
  colback=violet!15,
  colframe=violet!60,
  fonttitle=\sffamily\bfseries,
  title=\textsc{{{}}}
]
{}
\end{{tcolorbox}}
]], { i(1, "Example"), i(2) })),
}

