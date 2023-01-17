import Prolog.Datatypes
import Prolog.Checkers
import Prolog.Conversions
import Prolog.Unification
import Prolog.Resolution
import Tools

import Control.Exception

answer :: String -> IO()
answer str = do
  putStr "\t"
  putStrLn str

showQRs :: [QueryResult] -> IO ()
showQRs [] = do
  answer "false."
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
  answer "true."
showQR (EndQR False) = do
  answer "false."
showQR (MakeQR (var, r) (EndQR _)) = do
  answer $ showVariable var ++ " = " ++ showReplacement r ++ "."
showQR (MakeQR (var, r) qr) = do
  answer $ showVariable var ++ " = " ++ showReplacement r ++ "."
  showQR qr

check :: String -> Database -> IO ()
check input database = do
  if input == "quit"
    then return ()
    else
      if not (isFact input) && not (isRule input) && not (isEquality input) && not (isSequence input)
        then do
          putStrLn "You are allowed to input only facts, queries and equalities!"
          userInteract database
        else do
          showQRs $ interpreteInput input database
          userInteract database

userInteract :: Database -> IO ()
userInteract database = do
  factInput <- getLine
  let fact = removeWhiteSpacesAroundComma factInput
  check fact database

workWithFile :: String -> IO ()
workWithFile path = do
  contents <- try (readFile path) :: IO (Either SomeException String) 
  case contents of 
     Left ex   -> answer "No such file... " 
     Right val -> do
        let modify = removeWhiteSpacesAroundComma val
        let truth = consult modify
        answer $ if fst truth then "true." else "false.\n" ++ unlines (snd truth)
        let realCode =
              [ removeWhiteSpacesAroundComma x
                | x <- lines modify,
                  (not . isComment) x,
                  (not . null) x
              ]
        let interpretedCode = interpreteCode realCode
        userInteract interpretedCode

anotherFile :: IO()
anotherFile = do
  putStrLn "Consult another file? ( y | [n] )"
  response <- getLine
  if (not . null) response && head response == 'y' then loop else return ()

loop :: IO ()
loop = do
  putStrLn "Which file to consult?"
  file <- getLine
  workWithFile file
  anotherFile

main :: IO ()
main = do
  loop
  putStrLn "Closing..."
  response <- getLine
  return ()