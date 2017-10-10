module Actions where

import System.Process
import System.Directory
import Types
import System.IO
import Data.Yaml
import Rainbow
import Data.List
import System.Random
import System.Random.Shuffle

newNag :: IO Nag
newNag = do
  putStrLn "Okay, we're up to create a new nag"
  putStrLn "First, let's get its tags. Separate them by space."
  tags <- words <$> getLine
  putStrLn "Okay, I got em"
  putStr "Now describe your nag in your favourite editor"
  hFlush stdout >> getLine
  action <- editor "/tmp/nag"
  putStr "Okay, I created a nag. It's default status is \"Active\"."
  return $ Nag action tags Active

editor :: FilePath -> IO String
editor fp = do
  ed <- runCommand $ "$EDITOR " ++ fp
  waitForProcess ed
  edit <- readFile fp
  removeFile fp
  return edit

readNags :: IO [ Nag ]
readNags = do
  home <- getHomeDirectory
  maybeNags <- withCurrentDirectory home $ decodeFileEither ".nagger.yaml"
  case maybeNags of
    Left e -> do
      hPutStrLn stderr "No .nagger.yaml file present. No nags :("
      return []
    Right x -> return x

writeNags :: [ Nag ] -> IO ()
writeNags nags = do
  home <- getHomeDirectory
  let sorted = sortOn status nags
  withCurrentDirectory home $ encodeFile ".nagger.yaml" nags

addNag :: IO ()
addNag = do
  nags <- readNags
  nag <- newNag
  writeNags $ nag:nags
  putStrLn "Your nag was saved. Thank you for your patience."

showOneNag :: Nag -> IO Nag
showOneNag nag = do
  putChunkLn $ bold $ chunk "[e]dit | [t]ags | [a]ctive | [c]omplete | [q]ueue | [x]cancel" & fore black
  putChunkLn $ bold $ chunk "-------------------------------------------------------------" & fore black
  putStrLn $ dropTails $ action nag
  putChunkLn $ bold $ chunk ( unwords $ tags nag ) & fore yellow
  putChunkLn $ bold $ chunk ( show $ status nag ) & fore red
  hFlush stdout >> hSetBuffering stdin NoBuffering
  todo <- getChar
  hSetBuffering stdin LineBuffering
  case todo of
    'e' -> do
      writeFile "/tmp/nag" $ action nag
      newAction <- editor "/tmp/nag"
      putStrLn "But be still patient: I will save only on clean exit"
      return $ nag { action = newAction }
    't' -> do
      putStrLn "Okay, enter new tags (old ones will be discarded)"
      newTags <- hFlush stdout >> getLine
      putStrLn "But be still patient: I will save only on clean exit"
      return $ nag { tags = words newTags }
    'a' -> do
      putStrLn "Nag status set to Active"
      return $ nag { status = Active }
    'c' -> do
      putStrLn "Nag status set to Complete"
      return $ nag { status = Complete }
    'q' -> do
      putStrLn "Nag status set to Queued"
      return $ nag { status = Queued }
    'x' -> do
      putStrLn "Nag status set to Cancelled"
      return $ nag { status = Cancelled }
    ___ -> return nag
  where
  dropTails :: String -> String
  dropTails s | s /= "" && last s == '\n' = dropTails $ init s
              | otherwise = s

showTags :: IO ()
showTags = do
  allNags <- readNags
  mapM_ putStrLn $ nub $ allNags >>= tags

showNags :: [ Status ] -> IO ()
showNags sts = do
  allNags <- readNags
  goodNags <- shuf $ filter (\nag -> status nag `elem` sts) allNags
  let badNags = allNags \\ goodNags
  newNags <- mapM showOneNag goodNags
  writeNags $ newNags ++ badNags

shuf :: [ a ] -> IO [ a ]
shuf list = do
  g <- newStdGen
  return $ shuffle' list ( length list ) g

showNagsByTags :: [ String ] -> IO ()
showNagsByTags tgs = do
  allNags <- readNags
  goodNags <- shuf $ filter (\nag -> tags nag `intersect` tgs /= [] ) allNags
  let badNags = allNags \\ goodNags
  newNags <- mapM showOneNag goodNags
  writeNags $ badNags ++ newNags
