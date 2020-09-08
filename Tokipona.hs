module Tokipona (
        Nimi, NP, Prep, Modifier, Preverb,
        VP, Predicate, Statement, Toki,
        eval
) where

import Parsing
import Control.Applicative

type Nimi = String
data NP = NP Nimi [Modifier] deriving Show
data Prep = Prep Nimi NP | PPAla Nimi NP deriving Show
data Modifier = Single Nimi | Group NP | Pr Prep deriving Show
data Preverb = Preverb Nimi VP | PVAla Nimi VP deriving Show
data VP = VP NP | Question VP | Pvb Preverb deriving Show
data Predicate = Predicate VP [NP] [Prep] deriving Show
data Statement = Li [NP] [Predicate] | O [NP] Predicate deriving Show
data Toki = La1 NP Toki | La2 Prep Toki | La3 Statement Toki | S Statement |
            Taso Statement | AnuSeme Statement deriving Show


consonant = char 'p' <|> char 't' <|> char 'k' <|> char 's' <|>
            char 'w' <|> char 'l' <|> char 'j' <|> char 'n' <|>
            char 'm'
vowel = char 'a' <|> char 'e' <|> char 'i' <|> char 'o' <|> char 'u'
syllable = do c <- consonant 
              v <- vowel
              return [c,v] 
           <|> 
           do n <- char 'n'
              return [n]
initSyllable = do x <- vowel
                  return [x]
               <|> syllable

particle = symbol "en" <|> symbol "li" <|> symbol "e" <|>
           symbol "la" <|> symbol "anu" <|> symbol "pi"

content :: Parser Nimi
content = token (do notP particle
                    x <- initSyllable
                    xs <- many syllable
                    return (concat (x:xs)))

prepNimi = symbol "tawa" <|> symbol "tan" <|> symbol "kepeken" <|>
           symbol "lon" <|> symbol "sama" 

prep = do w <- prepNimi
          (do symbol "ala"
              p <- np
              return (PPAla w p)
           <+>
           do p <- np
              return (Prep w p))

modifier :: Parser Modifier
modifier = (do p <- prep 
               return (Pr p)
            <+> 
            do w <- content
               return (Single w))
           <|> 
           do symbol "pi"
              n <- np
              case n of
                   (NP _ []) -> empty
                   (NP _ _ ) -> return (Group n)

np :: Parser NP
np = do w <- content
        ms <- many modifier
        return (NP w ms)

preverbNimi = symbol "kama" <|> symbol "ken" <|> symbol "wile" <|>
              symbol "lukin" <|> symbol "oko" <|> symbol "alasa" <|>
              symbol "wile" <|> symbol "sona" <|> symbol "awen" <|>
              symbol "pini" 

preverb :: Parser Preverb
preverb = do p <- preverbNimi
             symbol "ala"
             v <- vp
             return (PVAla p v)
          <+>
          do p <- preverbNimi
             v <- vp
             return (Preverb p v)

wordsOf :: Modifier -> [Nimi]
wordsOf (Single s)        = [s]
wordsOf (Group (NP x xs)) = x : (concat $ map wordsOf xs)
wordsOf (Pr (Prep _ n))   = wordsOf (Group n)

verb :: Parser VP
verb = -- V ala V constructions can take two forms:
       -- X MS ala X MS where X's modifiers are repeated on each side or
       -- X ala X MS where X's modifiers appear only on the right side
       do ws <- some (do notP (symbol "ala"); content)
          symbol "ala"
          n@(NP x ms) <- np
          let xs = concat $ map wordsOf ms
          case ws of
               [w] -> if (w == x) then return (Question (VP n)) else empty
               _   -> if (ws == (x:xs)) then return (Question (VP n)) else empty
       <+>
       do n <- np
          return (VP n)

vp = -- Questions with preverbs/prepositions take the form:
     -- P ala P NP where only the prev/prep itself is on the left side
     do pn <- preverbNimi
        symbol "ala"
        p <- preverb
        case p of
             (Preverb s _) -> if (s == pn) 
                              then return (Question (Pvb p))
                              else empty
             _           -> empty
     <+>
     do pn <- prepNimi
        symbol "ala"
        p <- prep
        case p of
             (Prep s _) -> if (s == pn)
                           then return (Question (VP (NP "" [Pr p])))
                           else empty
             _          -> empty
     <+>
     do p <- preverb
        return (Pvb p)
     <+>
     do p <- prep
        return (VP (NP "" [Pr p]))
     <+> verb

predicateNoLi :: Parser Predicate
predicateNoLi = do v <- vp              
                   os <- many (do symbol "e"; np)
                   preps <- many prep
                   return (Predicate v os preps)

predicate = do symbol "li"; predicateNoLi

statement :: Parser Statement
statement = do s <- (symbol "mi" <|> symbol "sina")
               p <- predicateNoLi
               ps <- many predicate
               return (Li [(NP s [])] (p:ps))
            <+>
            do n <- np
               ns <- many (do symbol "en"; np)
               (do ps <- some predicate
                   return (Li (n:ns) ps)
                <|>
                do symbol "o"
                   p <- predicateNoLi
                   return (O (n:ns) p))

sentence :: Parser Toki
sentence = do n <- np
              symbol "la"
              t <- sentence
              return (La1 n t)
           <|>
           do p <- prep
              symbol "la"
              t <- sentence
              return (La2 p t)
           <|> 
           do s <- statement
              symbol "la"
              t <- sentence
              return (La3 s t)
           <|>
           do s <- statement
              symbol "anu"
              symbol "seme"
              return (AnuSeme s)
           <|>
           do symbol "taso"
              s <- statement
              return (Taso s)
           <|>
           do s <- statement
              return (S s)

sentences :: Parser [Toki]
sentences = do s <- sentence
               ss <- many (do punct; sentence)
               return (s:ss)
            where punct = symbol "."<|>symbol "!"<|>symbol "?"<|>symbol ":"

eval :: String -> (Maybe [[Toki]], [String])
eval s = case p of
              [] -> (Nothing, [])
              xs -> if (all (== "") (map snd p)) 
                    then (Just (map fst p), []) 
                    else (Nothing, map snd p)
         where p = parse sentences s
