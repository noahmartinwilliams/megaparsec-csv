-- |
-- Module      : Text.Megaparsec.CSV
-- Description : Parsec CSV files with optional escape characters using megaparsec
-- Copyright   : [2025] Noah Martin Williams
-- License     : BSD3
--
-- Maintainer  : Noah Martin Williams <noahmartinwilliams@gmail.com>
-- Stability   : experimental
-- Portability : portable
--
-- This module contains the csv function.
module Text.Megaparsec.CSV(csv, CSVParser, csvT, CSVParserT) where

import Control.Monad
import Data.Text
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer
import Text.Megaparsec.Internal

-- * The String parser
-- | The parsing type for CSV files using string type.
-- 
type CSVParser = Parsec Void String

escapedChar :: Char -> CSVParser Char
escapedChar c = do
    void $ single c
    c' <- anySingle 
    return c'

removeCR :: String -> String
removeCR ret = do
    let l = Prelude.length ret
    if l /= 0
    then
        if (ret !! (l - 1)) == '\r' 
        then
            (Prelude.take (l - 1) ret)
        else
            ret
    else
        ""

eol' :: CSVParser ()
eol' = ((single '\n' >> return ()) <|> eof)

csvEntry :: Char -> Maybe Char -> CSVParser String
csvEntry sep (Just esc) = do
    ret <- many ((escapedChar esc) <|> (noneOf (sep : "\n")))
    void $ optional (single sep)
    return (removeCR ret)
csvEntry sep Nothing = do
    ret <- many (noneOf (sep : "\n")) 
    void $ optional (single sep)
    return (removeCR ret)

csvLine :: Char -> Maybe Char -> CSVParser [String]
csvLine sep escape = do
    ret <- manyTill (csvEntry sep escape) (lookAhead eol')
    void $ eol'
    return ret

-- | The CSV parser. The first argument is the seperator and the second argument is the optional escape character. 
--
csv :: Char -> Maybe Char -> CSVParser [[String]]
csv seperator escape = manyTill (csvLine seperator escape) eof

-- * The Text Parser
-- | The parsing type for CSV files using text type.
-- 
type CSVParserT = Parsec Void Text

removeCRT :: Text -> Text
removeCRT ret = do
    let l = Data.Text.length ret
    if l /= 0
    then
        if (ret `Data.Text.index` (l - 1)) == '\r' 
        then
            (Data.Text.take (l - 1) ret)
        else
            ret
    else
        Data.Text.empty

escapedCharT :: Char -> CSVParserT Char
escapedCharT c = do
    void $ single c
    c' <- anySingle 
    return c'

eolT' :: CSVParserT ()
eolT' = ((single '\n' >> return ()) <|> eof)

csvEntryT :: Char -> Maybe Char -> CSVParserT Text
csvEntryT sep (Just esc) = do
    ret <- many ((escapedCharT esc) <|> (noneOf (sep : "\n")))
    void $ optional (single sep)
    return (removeCRT (Data.Text.pack ret))
csvEntryT sep Nothing = do
    ret <- many (noneOf (sep : "\n")) 
    void $ optional (single sep)
    return (removeCRT (Data.Text.pack ret))

csvLineT :: Char -> Maybe Char -> CSVParserT [Text]
csvLineT sep escape = do
    ret <- manyTill (csvEntryT sep escape) (lookAhead eolT')
    void $ eolT'
    return ret

-- | The CSV parser for text type. The first argument is the seperator and the second argument is the optional escape character. 
--
csvT :: Char -> Maybe Char -> CSVParserT [[Text]]
csvT seperator escape = manyTill (csvLineT seperator escape) eof

