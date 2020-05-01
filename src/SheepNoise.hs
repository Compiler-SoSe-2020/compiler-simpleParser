module SheepNoise
    ( sheepNoise
    , scanner
    , SheepNoise(..)
    , Token(..)
    ) where

import           Text.ParserCombinators.Parsec

-- Die Sprache der Schafe
data SheepNoise = SN
  deriving (Show, Eq)

-- Die Worte von Schafen und Hütehund
data Token = Baa
           | Wuff
  deriving (Show, Eq)

-- | Scanner, der den Zeichenstrom in Token zerlegt, oder den restlichen
-- Zeichenstrom zurückgibt. `Either` ist ein Datentyp, der entweder einen Wert
-- `Left s` oder `Right [toks]` hat
scanner :: String -> Either String [Token]

-- nichts zu lesen -> leere Tokenliste
scanner [] = Right []

-- Match auf "baa"
scanner ('b':'a':'a':rest) =
  case scannedRest of
    Left  _            -> scannedRest
    Right scannedRest' -> Right (Baa : scannedRest')
  where scannedRest = scanner rest

-- Match auf "wuff"
scanner ('w':'u':'f':'f':rest) =
  case scannedRest of
    Left  _            -> scannedRest
    Right scannedRest' -> Right (Wuff : scannedRest')
  where scannedRest = scanner rest

-- Fehler
scanner stream = Left stream


-- | Einfacher Parser für die Sprache der Schafe
--
-- SN -> baa SN
--    |  baa
--
-- Rückgabe ist entweder ein Wert von Typ SheepNoise oder ein Fehler, `Nothing`
-- wenn kein Token verfügbar ist oder `Just x` mit dem unerwarteten Token `x`.
--
sheepNoise :: [Token] -> Either (Maybe Token) SheepNoise
sheepNoise (Baa:rest) = case rest of
                          (_:_) -> sheepNoise rest
                          []    -> Right SN
sheepNoise [] = Left Nothing
sheepNoise (Wuff:_) = Left (Just Wuff)
