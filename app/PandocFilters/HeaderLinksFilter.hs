{-# LANGUAGE OverloadedStrings #-}

module PandocFilters.HeaderLinksFilter where

import Text.Pandoc.JSON

import Control.Monad.State.Strict

import Data.Text (Text, pack, append)

import Debug.Trace

addHeaderLink :: Block -> Block
addHeaderLink h@(Header level attr@(i, classes, namevals) contents) =
    Header level attr [ Link attr contents (append "#" i, "")]
    -- Div ("", [], [])
    --     [ h
    --     , Plain 
    --         [ Link 
    --             ("", ["header-link"], []) 
    --             [ Str "#" ] 
    --             ("javascript:void(0);", "")
    --         ]
    --     ]
addHeaderLink b = b