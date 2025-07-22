local ls = require("luasnip")
local s = ls.snippet
local t = ls.text_node
local i = ls.insert_node
local fmt = require("luasnip.extras.fmt").fmt
local rep = require("luasnip.extras").rep

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

  -- Display math environment (autoexpand)
  s({ trig = "dm", wordTrig = true, snippetType = "autosnippet" }, fmt([[
  \[
    {}
  \]
  ]], { i(1) })),

  s({ trig = "mk", wordTrig = true, snippetType = "autosnippet" }, fmt([[
  /( {} /)
  ]], { i(1) })),

   -- \mathbb{} snippet
   s({ trig = "mbb", wordTrig = true, snippetType = "autosnippet" }, fmt([[
   \mathbb{{{}}}
   ]], { i(1) })),

   -- \mathcal{} snippet
   s({ trig = "mcal", wordTrig = true, snippetType = "autosnippet" }, fmt([[
   \mathcal{{{}}}
   ]], { i(1) })),

   -- Normal text bold
   s({ trig = "bf", wordTrig = true, snippetType = "autosnippet" }, fmt([[
   \textbf{{{}}}
   ]], { i(1) })),

   -- Normal text italic
   s({ trig = "if", wordTrig = true, snippetType = "autosnippet" }, fmt([[
   \textit{{{}}}
   ]], { i(1) })),

   -- Math mode bold
   s({ trig = "mbf", wordTrig = true, snippetType = "autosnippet" }, fmt([[
   \mathbf{{{}}}
   ]], { i(1) })),

   -- Math mode italic
   s({ trig = "mif", wordTrig = true, snippetType = "autosnippet" }, fmt([[
   \mathit{{{}}}
   ]], { i(1) })),
}
