module PandocFilters.RevealLineHighlightingFilter where

import Text.Pandoc.JSON
import Text.Pandoc.Parsing

import qualified Data.Text as T

import qualified Debug.Trace as DT

convertToReveal :: Block -> Block
convertToReveal b@(CodeBlock (i, classes, namevals) contents) =
    let codeBlock = T.concat ["<code class=\"", T.concat $ map (T.append "hljs ") classes, "\" ", T.concat $ map nameValToText namevals, ">"]
        block = RawBlock (Format "html") (T.concat ["<pre>", codeBlock, contents, "</code></pre>"])
    in DT.trace ("block=" ++ show block) block
    where
        nameValToText (name, val) = T.concat [
                case name of
                    "h-lines" -> "data-line-numbers"
                    name'     -> name',
                "=",
                "\"", T.filter (/= ' ') val, "\""
            ]
convertToReveal b = b