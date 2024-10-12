{-

$if = "if"
$then = "then"
$else = "else"

$num = "{num}"
$eq = "=="

$ = if expr then expr else expr

$expr = num | num eq num

-}

import System.Random
import Data.List
import Data.Char
import Control.Monad

type T = String
type NT = String
type Gen = String

data Grammar = Grammar Production [Definition] [Rule]

data Definition = Definition T Gen

data Rule = Rule NT [Production]

data Production = Production [Symbol]
    deriving Eq

data Symbol = Terminal String | Nonterminal String
    deriving Eq





-- My first intuition was to skip tokenization (Tsoding did something similar with JSON
-- parsing, but I didn't want to just copy that approach), but it failed miserably, so
-- now I'll try the usual approach
data GToken = SymbolId String
            | TerminalDef String
            | Goal
            | ProdEq
            | ProdOr
            | TerminalStr String
            | Invalid | EOF
getToken :: String -> (String, GToken)
getToken [] = ("", EOF)
getToken (' ':xs) = getToken xs
getToken ('\t':xs) = getToken xs
getToken ('\n':xs) = getToken xs
getToken ('\r':xs) = getToken xs
getToken ('$':x:xs)
    | isAlpha x = let (rest, s) = readAlpha xs in
                  (rest, TerminalDef (x:s))
    | otherwise = (x:xs, Goal)
getToken ('=':xs) = (xs, ProdEq)
getToken ('|':xs) = (xs, ProdOr)
getToken ('"':xs) = (rest, TerminalStr cont)
    where (rest, cont) = readUntilStr xs
getToken (x:xs)
    | isAlpha x = let (rest, s) = readAlpha xs in
                  (rest, SymbolId (x:s))
    | otherwise = (x:xs, Goal)

readAlpha :: String -> (String, String)
readAlpha (x:xs) = if isAlpha x then (rest, x:id) else (x:xs, [])
    where (rest, id) = readAlpha xs

readUntilStr :: String -> (String, String)
readUntilStr ('"':xs) = (xs, "")
readUntilStr (x:xs) = (rest, x:contents)
    where (rest, contents) = readUntilStr xs






generate :: Grammar -> IO [String]
generate g@(Grammar goal _ _) = expandProduction g goal

expandProduction :: Grammar -> Production -> IO [String]
expandProduction g (Production symbols) = (sequenceA $ map (expandSymbol g) symbols) >>= (return . join)

expandSymbol :: Grammar -> Symbol -> IO [String]
expandSymbol (Grammar _ defs _) (Terminal t) = singleton <$> generateSymbol gen
    -- HACK: ehhh
    where (Just (Definition _ gen)) = find (\(Definition d _) -> d == t) defs

-- TODO: Implement
generateSymbol :: Gen -> IO String
generateSymbol gen = pure gen
