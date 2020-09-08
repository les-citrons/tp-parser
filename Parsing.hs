
module Parsing where

import Data.Char
import Control.Applicative

newtype Parser a = P (String -> [(a, String)])

parse :: Parser a -> String -> [(a, String)]
parse (P p) inp = p inp

item = P (\inp -> case inp of 
                  []     -> []
                  (x:xs) -> [(x,xs)])

instance Functor Parser where
        fmap g p  = P (\inp -> case parse p inp of
                               [] -> []
                               xs -> map (\(x,s) -> (g x,s)) xs)

instance Applicative Parser where
        -- pure :: a -> Parser a
        pure x = P (\inp -> [(x,inp)])
        
        -- (<*>) :: Parser (a -> b) -> Parser a -> Parser b
        pf <*> px = P (\inp -> case parse pf inp of 
                               [] -> []
                               xs -> concat $ 
                                     map (\(f,s) -> parse (fmap f px) s) xs)

instance Alternative Parser where
        -- empty :: Parser a
        empty = P (\inp -> [])

        -- (<|>) :: Parser a -> Parser a -> Parser a
        p <|> q = P (\inp -> case parse p inp of
                             [] -> parse q inp
                             xs -> xs)

(<+>) :: Parser a -> Parser a -> Parser a
p <+> q = P (\inp -> parse p inp ++ parse q inp)

instance Monad Parser where
        p >>= f = P (\inp -> case parse p inp of
                             [] -> []
                             xs -> concat $ map (\(x,s) -> parse (f x) s) xs)

sat :: (Char -> Bool) -> Parser Char
sat p = do x <- item
           if p x then return x else empty

notP :: Parser a -> Parser ()
notP p = P (\inp -> case parse p inp of
                    [] -> [((),inp)]
                    _  -> [])

char x = sat (== x)

digit = sat isDigit
lower = sat isLower
upper = sat isUpper
letter = sat isAlpha
alphanum = sat isAlphaNum 

nat :: Parser Integer
nat = do xs <- some digit
         return (read xs)

int :: Parser Integer
int = do char '-'
         n <- nat
         return (-n)
      <|> nat

string :: String -> Parser String
string []     = return [] 
string (x:xs) = do char x
                   string xs
                   return (x:xs)

strings :: [String] -> Parser String
strings = foldr1 (<|>) . map string

space :: Parser ()
space = do many (sat isSpace)
           return ()

token :: Parser a -> Parser a
token p = do space
             x <- p
             space
             return x

symbol = token . string

