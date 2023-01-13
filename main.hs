import Prolog
import Tools

import Control.Exception

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
  contents <- try (readFile ("prolog/" ++ path)) :: IO (Either SomeException String) 
  case contents of 
     Left ex   -> putStrLn "No such file... " 
     Right val -> do
        let modify = removeWhiteSpacesAroundComma val
        let truth = consult modify
        putStrLn $ if fst truth then "true." else "false.\n" ++ unlines (snd truth)
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
  putStr "Consult another file? ( y | [n] )\n> "
  response <- getLine
  if (not . null) response && head response == 'y' then loop else return ()

loop :: IO ()
loop = do
  putStr "Which file to consult from the directory \"prolog/\"?\n> "
  file <- getLine
  workWithFile file
  anotherFile

-- todo childof(X,Y) doesn't finish
-- todo halts when reversed variables from left to right
-- todo maybe append "0" to variables in the file
-- todo using 2 functions
        -- todo one when reading from file and one regular

main :: IO ()
main = do
  loop
  putStrLn "Closing..."
  response <- getLine
  return ()