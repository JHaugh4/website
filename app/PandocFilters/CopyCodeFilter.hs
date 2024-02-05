{-# LANGUAGE OverloadedStrings #-}

module PandocFilters.CopyCodeFilter where

import Text.Pandoc.JSON

import Control.Monad.State.Strict

import Data.Text (Text, pack, append)

import Debug.Trace

{-
This function transforms every code block and adds a header above
with the name of the language and a button to copy the code in the
code block. It accomplishes this task by generating an auto-incremented
index to attach to the button as well as the code block as ids. The javascript
code in copy-code.js then uses these ids to grab the data from the code block
and put it into the clipboard.

It also adds in the images and text for when you copy and after you copy.
-}
addCopyButton :: Block -> State Int Block
addCopyButton b@(CodeBlock (i, classes, namevals) contents)
    | null classes = return b
    | otherwise = do
        newId <- get
        put (newId + 1)
        let hide = filter (== "hide") classes
        return $
            Div ("", "code-container" : hide, []) 
                [ Div ("", ["code-header"], [])
                    [ Plain 
                        [ Span ("", ["language-label"], []) [Str $ head classes]
                        , Link 
                            ("", ["copy-button"] , [("data-copy-target", pack $ show newId)]) 
                            [ Image ("copyIconId", ["copy-icon"], []) [] ("/images/copy-icon.png", "")
                            , Span ("copyCodeId", ["copy-code"], []) [Str "copy code"]
                            , Image ("copiedIconId", ["copied-icon"], []) [] ("/images/copied-icon.png", "")
                            , Span ("copiedId", ["copied"], []) [Str "copied!"]
                            ]
                            ("javascript:void(0);", "")
                        ]
                    ]
                , CodeBlock (pack $ show newId, resolveClasses classes, namevals) contents
                ]
addCopyButton x = return x

-- When I do code for cabal repl commands I am adopting the
-- convention of marking it with the "repl" class. However,
-- this means it wont't get formatted by "syntax.css" thus,
-- I replace the class with "haskell" to get it highlighted like haskell.
resolveClasses :: [Text] -> [Text]
resolveClasses ts
    | "repl" `elem` ts = "haskell" : filter (/= "repl") ts
    | otherwise        = ts

-- main :: IO ()
-- main = toJSONFilter addCopyButton
