import System.Random
import Data.List
import Control.Monad

type T = String
type NT = String
type Gen = String

data Grammar = Grammar Production [Definition] [Rule]

data Definition = Definition T Gen

data Rule = Rule NT [Production]

data Production = Production [Symbol]

data Symbol = Terminal String | Nonterminal String


expandProduction :: Grammar -> Production -> IO [String]
expandProduction g (Production symbols) = (sequenceA $ map (expandSymbol g) symbols) >>= (return . join)

expandSymbol :: Grammar -> Symbol -> IO [String]
expandSymbol (Grammar _ defs _) (Terminal t) = singleton <$> generateSymbol gen
    where (Just (Definition _ gen)) = find (\(Definition d _) -> d == t) defs

-- TODO: Implement
generateSymbol :: Gen -> IO String
generateSymbol gen = pure gen
