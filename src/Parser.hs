{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Parser where
import Control.Applicative
import Control.Monad
import Control.Monad.State
import Data.Char

newtype BaseParser m s a = Parser {
    unparse :: StateT s m a
} deriving (Functor, Applicative, Alternative, Monad, MonadPlus)

type Parser a = BaseParser Maybe String a

runParser :: Parser a -> String -> Maybe a
runParser p = fmap fst
    . mfilter (null . snd)
    . runStateT (unparse p)

anyChar :: Parser Char
anyChar = Parser (StateT p) where
   p [] = empty
   p (h : t) = pure (h, t)

char :: Char -> Parser Char
char c = mfilter (== c) anyChar

digit :: Parser Char
digit = mfilter isDigit anyChar

bool :: Parser Bool
bool = True <$ (string "True")  <|>
       False <$ (string "False")

string :: String -> Parser String
string =  traverse char

