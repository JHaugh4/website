{-# LANGUAGE OverloadedStrings #-}

module PandocFilters.HeaderLinksFilter where

import Text.Pandoc.JSON

import Data.Text (append)

addHeaderLink :: Block -> Block
addHeaderLink h@(Header level attr@(i, classes, namevals) contents) =
    Header level attr [ Link attr contents (append "#" i, "")]
addHeaderLink b = b