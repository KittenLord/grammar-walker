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
import System.IO
import System.IO.Unsafe
import Data.List
import Data.Char
import Data.Maybe
import Control.Monad

type T = String
type NT = String
type Gen = String

data Grammar = Grammar Production [Definition] [Rule]
    deriving (Show)

data Definition = Definition T Gen
    deriving (Show)

data Rule = Rule NT [Production]
    deriving (Show)

data Production = Production [Symbol]
    deriving (Eq, Show)

data Symbol = Terminal String | Nonterminal String
    deriving (Eq, Show)



instance Semigroup Grammar where
    (<>) (Grammar prod defs rules) (Grammar prod' defs' rules') = Grammar p (defs ++ defs') (rules ++ rules')
        where p = if prod' == Production [] then prod else prod'
instance Monoid Grammar where mempty = Grammar (Production []) [] []




-- My first intuition was to skip tokenization (Tsoding did something similar with JSON
-- parsing, but I didn't want to just copy that approach), but it failed miserably, so
-- now I'll try the usual approach
data GToken = SymbolId String
            | TerminalDef String
            -- | Goal
            | ProdEq
            | ProdOr
            | TerminalStr String
            | Invalid | EOF
    deriving (Show, Eq)
getToken :: String -> (String, GToken)
getToken [] = ("", EOF)
getToken (' ':xs) = getToken xs
getToken ('\t':xs) = getToken xs
getToken ('\n':xs) = getToken xs
getToken ('\r':xs) = getToken xs
getToken ('$':x:xs)
    | isAlpha x = let (rest, s) = readAlpha xs in
                  (rest, TerminalDef (x:s))
    | otherwise = (x:xs, TerminalDef "#Goal")
getToken ('=':xs) = (xs, ProdEq)
getToken ('|':xs) = (xs, ProdOr)
getToken ('"':xs) = (rest, TerminalStr cont)
    where (rest, cont) = readUntilStr xs
getToken (x:xs)
    | isAlpha x = let (rest, s) = readAlpha xs in
                  (rest, SymbolId (x:s))
    | otherwise = (x:xs, Invalid)

readAlpha :: String -> (String, String)
readAlpha (x:xs) = if isAlpha x then (rest, x:id) else (x:xs, [])
    where (rest, id) = readAlpha xs

readUntilStr :: String -> (String, String)
readUntilStr ('"':xs) = (xs, "")
readUntilStr (x:xs) = (rest, x:contents)
    where (rest, contents) = readUntilStr xs

getTokenStream :: String -> [GToken]
getTokenStream input = token : getTokenStream rest
    where (rest, token) = getToken input



parseGrammar :: Grammar -> [GToken] -> Maybe Grammar
parseGrammar g (EOF:_) = Just g
parseGrammar g ((TerminalDef t):(ProdEq):(TerminalStr def):rest) = parseGrammar (ng <> g) rest
    where ng = Grammar (Production []) [Definition t def] []
parseGrammar g ((TerminalDef t):(ProdEq):rest) = parseGrammar (ng <> g) nrest
    where (nrest, consumed) = consumeSymbols g rest
          prods = map Production consumed
          rule = Rule t prods
          ng = if t == "#Goal" then Grammar (prods !! 0) [] [] else Grammar (Production []) [] [rule]

parseGrammar _ _ = Nothing

consumeSymbols :: Grammar -> [GToken] -> ([GToken], [[Symbol]])
consumeSymbols g tokens = res
    where go :: Grammar -> [[Symbol]] -> [GToken] -> ([GToken], [[Symbol]])
          go g acc t@(ProdOr:rest) = go g acc rest
          go g acc t@((SymbolId _):_) = go g (acc ++ [nacc]) rest
              where (rest, nacc) = go' g [] t
          go g acc t = (t, acc)

          go' :: Grammar -> [Symbol] -> [GToken] -> ([GToken], [Symbol])
          go' g acc t@((SymbolId sname):rest) = go' g (acc ++ [symbol]) rest
              where symbol = findSymbolInGrammar g sname
          go' g acc t = (t, acc)

          res = go g [] tokens

findSymbolInGrammar :: Grammar -> String -> Symbol
findSymbolInGrammar (Grammar _ defs _) sname = if isNothing def then Nonterminal sname else Terminal sname
    where def = find (\(Definition t _) -> t == sname) defs






generate :: Grammar -> IO [String]
generate g@(Grammar goal _ _) = expandProduction g goal

expandProduction :: Grammar -> Production -> IO [String]
expandProduction g (Production symbols) = (sequenceA $ map (expandSymbol g) symbols) >>= (return . join)

expandSymbol :: Grammar -> Symbol -> IO [String]
expandSymbol (Grammar _ defs _) (Terminal t) = singleton <$> generateSymbol gen
    -- HACK: ehhh
    where (Just (Definition _ gen)) = find (\(Definition d _) -> d == t) defs
expandSymbol g@(Grammar _ _ rules) (Nonterminal t) = do
    let (Just (Rule r prods)) = find (\(Rule r _) -> r == t) rules
    r <- randomIO
    let prod = prods !! (r `mod` length prods)
    expandProduction g prod
          

-- TODO: Implement
generateSymbol :: Gen -> IO String
generateSymbol gen = pure gen




main :: IO ()
main = do
    handle <- openFile "example.txt" ReadMode
    grammarText <- hGetContents handle
    let (Just grammar) = parseGrammar mempty (getTokenStream grammarText)
    result <- generate grammar
    print result
