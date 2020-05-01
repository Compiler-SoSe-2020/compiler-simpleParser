module Main where

import           SheepNoise (SheepNoise (..), scanner, sheepNoise)

import           System.IO (BufferMode (NoBuffering), hSetBuffering, stdout)

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    putStr "Bitte Satz eingeben: "
    word <- getLine
    putStr "===> "
    let scanned = scanner word
    case scanned of
      Left s       -> do
        putStrLn ("kann nicht gescanned werden wegen \"" ++ s ++ "\"")
        main
      Right tokens -> do
        case sheepNoise tokens of
          Left Nothing   -> putStrLn "keine Tokens"
          Left (Just tk) -> putStrLn ("unerwartetes Token: " ++ show tk
                                      ++ " in Tokenliste " ++ show tokens)
          Right SN       -> putStrLn (
            "Satz der Sprache \"SheepNoise\" erkannt mit Tokens " ++ show tokens)
        main
