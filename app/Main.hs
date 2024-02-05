{-# LANGUAGE OverloadedStrings #-}

module Main where

import Hakyll
import Hakyll.Web.Pandoc

import qualified Skylighting.Types as ST
import qualified Data.Map as Map

import Text.Pandoc
import Text.Pandoc.Writers (writeRevealJs)
import Text.Pandoc.Highlighting (Style, breezeDark, styleToCss)
import Text.Pandoc.Options      (ReaderOptions (..), WriterOptions (..))

import Debug.Trace

import PandocFilters.CopyCodeFilter
import PandocFilters.HeaderLinksFilter
import PandocFilters.RevealLineHighlightingFilter

import Text.Pandoc.Walk (walk, walkM)

import Control.Monad.State.Strict

import Data.Text (Text)
import qualified Data.Text as T

config :: Configuration
config = defaultConfiguration
  { destinationDirectory = "docs"
  }

main :: IO ()
main = hakyllWith config $ do
    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    create ["css/syntax.css"] $ do
        route idRoute
        compile $ do
            makeItem $ styleToCss pandocCodeStyle

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match (fromList ["about.md", "contact.markdown"]) $ do
        route   $ setExtension "html"
        compile $ blogCompilerWithStyleTransform
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    match "posts/*" $ do
        route $ setExtension "html"
        compile $ blogCompilerWithStyleTransform
            >>= loadAndApplyTemplate "templates/blog-post.html" dateCtx
            >>= loadAndApplyTemplate "templates/default.html" dateCtx
            >>= relativizeUrls

    match "tutorials/*" $ do
        route $ setExtension "html"
        compile $ blogCompilerWithStyleTransform
            >>= loadAndApplyTemplate "templates/tutorial.html" defaultContext
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    match "slides/*" $ do
        route $ setExtension "html"
        compile $ slidesCompilerWithTransform
            >>= loadAndApplyTemplate "templates/slides.html" slidesContext
            >>= relativizeUrls

    create ["blog-post-archive.html"] $ do
        route idRoute
        compile $ do
            blogPosts <- recentFirst =<< loadAll "blog-posts/*"
            let archiveCtx =
                    listField "blog-posts" dateCtx (return blogPosts) <>
                    constField "title" "Blog Post Archive"            <>
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/blog-post-archive.html" archiveCtx
                >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                >>= relativizeUrls

    create ["tutorial-archive.html"] $ do
        route idRoute
        compile $ do
            tutorials <- chronological =<< loadAll "tutorials/*"
            let archiveCtx =
                    listField "tutorials" dateCtx (return tutorials) <>
                    constField "title" "Tutorial Archive"            <>
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/tutorial-archive.html" archiveCtx
                >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                >>= relativizeUrls

    match "index.html" $ do
        route idRoute
        compile $ do
            tutorials <- chronological =<< loadAll "tutorials/*"
            blogPosts <- take 5 <$> (recentFirst =<< loadAll "blog-posts/*")
            let indexCtx =
                    listField "tutorials" dateCtx (return tutorials) <>
                    listField "blog-posts" dateCtx (return blogPosts)       <>
                    defaultContext

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls

    match "templates/*" $ compile templateBodyCompiler

    match "js/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "revealjs/**" $ do
        route idRoute
        compile copyFileCompiler

--------------------------------------------------------------------------------
dateCtx :: Context String
dateCtx =
    dateField "date" "%B %e, %Y" <>
    defaultContext

--------------------------------------------------------------------------------
-- Blog compiler

blogCompilerWithStyleTransform :: Compiler (Item String)
blogCompilerWithStyleTransform =
  pandocCompilerWithTransform
    defaultHakyllReaderOptions
    defaultHakyllWriterOptions
      { writerHighlightStyle   = Just pandocCodeStyle
      }
    (\b -> evalState (walkM (addCopyButton . addHeaderLink) b) 0)



--------------------------------------------------------------------------------
-- Slides compiler

slidesContext :: Context String
slidesContext =
    constField "author" "Joseph Haugh" <>
    constField "institute" "University of New Mexico" <>
    defaultContext

slidesCompilerWithTransform :: Compiler (Item String)
slidesCompilerWithTransform = 
  pandocRevealCompilerWithTransform
    defaultHakyllReaderOptions
    defaultHakyllWriterOptions
    (walk convertToReveal)

--------------------------------------------------------------------------------
-- | Write a document (as reveal HTML) using pandoc, with the supplied options
writePandocRevealWith :: WriterOptions  -- ^ Writer options for pandoc
                      -> Item Pandoc    -- ^ Document to write
                      -> Item String    -- ^ Resulting HTML
writePandocRevealWith wopt (Item itemi doc) =
    case runPure $ writeRevealJs wopt doc of
        Left err    -> error $ "Hakyll.Web.Pandoc.writePandocRevealWith: " ++ show err
        Right item' -> Item itemi $ T.unpack item'

--------------------------------------------------------------------------------
-- | Render the resource using pandoc
renderPandocRevealWithTransform :: ReaderOptions 
                                -> WriterOptions
                                -> (Pandoc -> Pandoc) 
                                -> Item String 
                                -> Compiler (Item String)
renderPandocRevealWithTransform ropt wopt f item =
    writePandocRevealWith wopt <$> (fmap . fmap) f (readPandocWith ropt item)

--------------------------------------------------------------------------------
-- | A version of 'pandocCompiler' which allows you to specify your own pandoc
-- options
pandocRevealCompilerWithTransform :: ReaderOptions 
                                  -> WriterOptions 
                                  -> (Pandoc -> Pandoc)
                                  -> Compiler (Item String)
pandocRevealCompilerWithTransform ropt wopt f =
    getResourceBody >>= renderPandocRevealWithTransform ropt wopt f

--------------------------------------------------------------------------------
-- Styles

pandocCodeStyle :: Style
pandocCodeStyle = solarizedDark

color :: Int -> Maybe ST.Color
color = ST.toColor

base03 = 0x002b36
base02 = 0x073642
base01 = 0x586e75
base00 = 0x657b83
base0 = 0x839496
base1 = 0x93a1a1
base2 = 0xeee8d5
base3 = 0xfdf6e3
yellow = 0xb58900
orange = 0xcb4b16
red = 0xdc322f
magenta = 0xd33682
violet = 0x6c71c4
blue = 0x268bd2
cyan = 0x2aa198
green = 0x859900

solarizedDark :: Style
solarizedDark = ST.Style {
    ST.backgroundColor = color base03
  , ST.defaultColor = color base1 -- Handles a lot such as 'a' in type sigs
  , ST.lineNumberColor = Nothing
  , ST.lineNumberBackgroundColor = Nothing
  , ST.tokenStyles = Map.fromList
    [ (ST.KeywordTok, ST.defStyle{ ST.tokenColor = color blue })
    , (ST.DataTypeTok, ST.defStyle{ ST.tokenColor = color base1 })
    , (ST.DecValTok, ST.defStyle{ ST.tokenColor = color magenta })
    , (ST.BaseNTok, ST.defStyle{ ST.tokenColor = color magenta })
    , (ST.FloatTok, ST.defStyle{ ST.tokenColor = color magenta })
    , (ST.CharTok, ST.defStyle{ ST.tokenColor = color cyan })
    , (ST.StringTok, ST.defStyle{ ST.tokenColor = color cyan })
    , (ST.CommentTok, ST.defStyle{ ST.tokenColor = color base01 })
    , (ST.OtherTok, ST.defStyle{ ST.tokenColor = color blue }) -- Handles function names
    , (ST.AlertTok, ST.defStyle{ ST.tokenColor = color orange })
    , (ST.FunctionTok, ST.defStyle{ ST.tokenColor = color blue })
    , (ST.ErrorTok, ST.defStyle{ ST.tokenColor = color red })
    , (ST.WarningTok, ST.defStyle{ ST.tokenColor = color yellow, ST.tokenBold = True })
    , (ST.ConstantTok, ST.defStyle{ ST.tokenColor = color magenta, ST.tokenBold = True })
    , (ST.SpecialCharTok, ST.defStyle{ ST.tokenColor = color orange })
    , (ST.VerbatimStringTok, ST.defStyle{ ST.tokenColor = color cyan })
    , (ST.SpecialStringTok, ST.defStyle{ ST.tokenColor = color cyan })
    , (ST.ImportTok, ST.defStyle)
    , (ST.VariableTok, ST.defStyle)
    , (ST.ControlFlowTok, ST.defStyle{ ST.tokenColor = color green })
    , (ST.OperatorTok, ST.defStyle{ ST.tokenColor = color green })
    , (ST.BuiltInTok, ST.defStyle)
    , (ST.ExtensionTok, ST.defStyle)
    , (ST.PreprocessorTok, ST.defStyle{ ST.tokenColor = color base1, ST.tokenBold = True })
    , (ST.AttributeTok, ST.defStyle)
    , (ST.DocumentationTok, ST.defStyle{ ST.tokenColor = color base1 })
    , (ST.AnnotationTok, ST.defStyle{ ST.tokenColor = color base1, ST.tokenBold = True })
    , (ST.CommentVarTok, ST.defStyle{ ST.tokenColor = color base2, ST.tokenBold = True })
    , (ST.InformationTok, ST.defStyle{ ST.tokenColor = color base1, ST.tokenBold = True })
    ]
  }