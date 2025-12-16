module Text.Megaparsec.CSV where

import Control.Monad
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer
import Text.Megaparsec.Internal


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

eoe :: Char -> CSVParser ()
eoe c = ((single c >> return ()) <|> (single '\n' >> return ()) <|> eof)

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

csv :: Char -> Maybe Char -> CSVParser [[String]]
csv seperator escape = manyTill (csvLine seperator escape) eof
