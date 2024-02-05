{-# LANGUAGE OverloadedStrings #-}

module PandocFilters.QuestionFilter where

import Text.Pandoc.JSON
import Text.Pandoc.Parsing
import Text.Pandoc.Builder

import Data.Text (Text)
import qualified Data.Text as T

import qualified Debug.Trace as DT

{-
This pandoc filter transforms every code block
with the "question" class into an element with the
question and ordered list of the answers. Javascript
will be injected to back the "Submit" button to check
their answer.
-}
-- questionFilter :: Block -> Block
-- questionFilter b@(CodeBlock (i, classes, namevals) contents)
--     | "question" `elem` classes = 
--         case DT.trace (show contents) $ runParser parseQuestion () "" contents of
--             Left err -> error $ show err
--             Right q -> Div ("", ["question-container"], []) [questionToHtml q]
--     | otherwise = b
-- questionFilter b = b

-- {-
-- This function parses the content of a "question" code block
-- -}
-- type QuestionText = Text

-- data Answer = Answer
--     { answerText :: Text
--     , correctAnswer :: Bool
--     } deriving (Eq, Show)

-- data Question = Question QuestionText [Answer]
--     deriving (Eq, Show)

-- type QuestionParser = Parsec Text ()

-- parseQuestion :: QuestionParser Question
-- parseQuestion = do
--     q <- parseQuestionText
--     as <- many parseAnswer
--     pure $ Question q as
--     where
--         oneLine = manyTillChar anyChar ((newline *> pure ()) <|> eof)
--         parseQuestionText = do
--             string "Q:"
--             spaces
--             oneLine
--         parseAnswer = (try parseWrongAnswer) <|> parseCorrectAnswer
--         parseWrongAnswer = do
--             string "A:"
--             spaces
--             a <- oneLine
--             DT.trace (T.unpack a) $
--                 pure $ Answer a False
--         parseCorrectAnswer = do
--             string "A*:"
--             spaces
--             a <- oneLine
--             DT.trace (T.unpack a) $
--                 pure $ Answer a True

-- {-
-- This function takes a question and turns it into an HTML element
-- -}
-- questionToHtml :: Question -> Block
-- questionToHtml (Question q as) = 
--     Div ("", ["question"], [ ("correct-answer=", ca) ]) [Para [Str q], answersToHtml as]
--     where
--         ca = case filter correctAnswer as of
--             [] -> ""
--             (answer:_) -> answerText answer

-- {-
-- This function takes a list of answers and turns it into an HTML element
-- -}
-- answersToHtml :: [Answer] -> Block
-- answersToHtml as = 
--     Div ("", ["answers"], []) bs
--     where
--         answerToHtml (Answer a True) = div plain $ str a
--         listAttrs = (1, LowerAlpha, OneParen)
--         as' = fmap answerToHtml as
--         (Pandoc _ bs) = doc $ orderedListWith listAttrs as'