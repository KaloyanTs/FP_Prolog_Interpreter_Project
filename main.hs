{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use when" #-}
import Checkers
import Conversions
import Datatypes
import Identities
import Resolutions
import Tools
import Unification

consult :: String -> (Bool, [String])
consult contents = (truth, if truth then [] else filter (\x -> not (isFact x || isRule x || isComment x)) (extractData contents))
  where
    truth = all (\x -> isFact x || isRule x || isComment x) (extractData contents)

interpreteCode :: [String] -> Database
interpreteCode c = (rules, facts)
  where
    rules = map toRule (filter isRule c)
    facts = map toFact (filter isFact c)

showQRs :: [QueryResult] -> IO ()
showQRs [] = do
  putStrLn "false."
showQRs [qr] = do
  showQR qr
showQRs (x@(EndQR _) : xs) = do
  showQR x
showQRs (x : xs) = do
  showQR x
  response <- getLine
  if (not . null) response
    then return ()
    else showQRs xs

showQR :: QueryResult -> IO ()
showQR (EndQR True) = do
  putStrLn "true."
showQR (EndQR False) = do
  putStrLn "false."
showQR (MakeQR (var, r) (EndQR _)) = do
  putStrLn $ showVariable var ++ " = " ++ showReplacement r ++ "."
showQR (MakeQR (var, r) qr) = do
  putStrLn $ showVariable var ++ " = " ++ showReplacement r ++ "."
  showQR qr

check :: String -> Database -> IO ()
check input database = do
  if input == "quit"
    then return ()
    else
      if not (isFact input) && not (isRule input) && not (isEquality input)
        then do
          putStrLn "You are allowed to input only facts, queries and equalities!"
          userInteract database
        else do
          showQRs $ interpreteInput input database
          userInteract database

userInteract :: Database -> IO ()
userInteract database = do
  putStr "> "
  factInput <- getLine
  let fact = removeWhiteSpacesAroundComma factInput
  check fact database

workWithFile :: String -> IO ()
workWithFile path = do
  contents <- readFile ("prolog/" ++ path)
  let truth = consult contents
  putStrLn $ if fst truth then "true." else "false.\n" ++ unlines (snd truth)
  let realCode =
        [ removeWhiteSpacesAroundComma x
          | x <- lines contents,
            (not . isComment) x,
            (not . null) x
        ]
  let interpretedCode = interpreteCode realCode
  userInteract interpretedCode

loop :: IO ()
loop = do
  putStr "Which file to consult from the directory \"prolog/\"?\n> "
  file <- getLine
  workWithFile file
  putStr "Consult another file? ( y | [n] )\n> "
  response <- getLine
  if (not . null) response && head response == 'y' then loop else return ()

-- todo handle error on file reading
-- todo childof(X,Y) doesn't finish

main :: IO ()
main = do
  loop
  putStrLn "Closing..."
  response <- getLine
  return ()