module Day7 where

import qualified Control.Monad.State.Lazy as S
import Data.List (delete, find, isPrefixOf, sort)
import Data.List.Split (splitOn)
import Paths_aoc2022 (getDataFileName)

day7 :: IO ()
day7 = do
  inputLines <- lines <$> (getDataFileName "input7.txt" >>= readFile)
  let rootDir = processCommands $ parseInput inputLines
  let dirSizes = map totalSize $ allDirs rootDir
  print $ sum $ filter (<= 100000) $ dirSizes -- 1749646
  let unusedSpace = 70000000 - totalSize rootDir
  let spaceToFree = max 0 $ 30000000 - unusedSpace
  print $ find (>= spaceToFree) $ sort dirSizes -- 1498966

---------- FSItem (Filesystem description) ----------

data FSItem
  = Dir String [FSItem] -- name, entries
  | File String Int -- name, size
  deriving (Eq, Show)

fsItemName :: FSItem -> String
fsItemName (File name _) = name
fsItemName (Dir name _) = name

allDirs :: FSItem -> [FSItem]
allDirs (File {}) = []
allDirs d@(Dir _ items) = d : concatMap allDirs items

totalSize :: FSItem -> Int
totalSize (File _ size) = size
totalSize (Dir _ items) = sum $ totalSize <$> items

-----------------------------------------------------

---------- Parsing input into simple commands ----------

parseInput :: [String] -> [Command]
parseInput [] = []
parseInput (line : lines) =
  case splitOn " " line of
    ["$", "cd", whr] -> Cd whr : parseInput lines
    ["$", "ls"] ->
      let (itemLines, lines') = break ("$" `isPrefixOf`) lines
       in Ls (parseFsItem <$> itemLines) : parseInput lines'
    _ -> error "invalid input line"
  where
    parseFsItem itemLine =
      case splitOn " " itemLine of
        ["dir", dirName] -> Dir dirName []
        [size, name] -> File name (read size)
        _ -> error "invalid input line"

data Command = Cd String | Ls [FSItem] deriving (Eq, Show)

--------------------------------------------------------

---------- Based on commands, figure out the state of the file system ----------

processCommands :: [Command] -> FSItem
processCommands cmds = msRootDir $ snd $ S.runState (processCommandsM cmds) initialMachineState
  where
    initialMachineState = MachineState {msCwd = [], msRootDir = Dir "/" []}

data MachineState = MachineState
  { msCwd :: [String],
    msRootDir :: FSItem
  }

processCommandsM :: [Command] -> S.State MachineState ()
processCommandsM [] = return ()
processCommandsM ((Cd whr) : cmds) = do
  case whr of
    "/" -> S.modify (\s -> s {msCwd = []})
    ".." -> S.modify (\s -> s {msCwd = init (msCwd s)})
    dirName -> S.modify (\s -> s {msCwd = msCwd s ++ [dirName]})
  processCommandsM cmds
processCommandsM ((Ls fsItems) : cmds) = do
  mapM_ addFsItemIfDoesntExist fsItems
  processCommandsM cmds

addFsItemIfDoesntExist :: FSItem -> S.State MachineState ()
addFsItemIfDoesntExist fsItem = do
  cwd <- msCwd <$> S.get
  rootDir <- msRootDir <$> S.get
  let rootDir' = addFsItemIfDoesntExist' fsItem cwd rootDir
  S.modify (\s -> s {msRootDir = rootDir'})
  where
    addFsItemIfDoesntExist' :: FSItem -> [String] -> FSItem -> FSItem
    addFsItemIfDoesntExist' _ _ (File {}) = error "Can't add item to a file!"
    addFsItemIfDoesntExist' newItem [] (Dir dirName dirItems) =
      let dirItems' =
            if any ((fsItemName newItem ==) . fsItemName) dirItems
              then dirItems
              else newItem : dirItems
       in Dir dirName dirItems'
    addFsItemIfDoesntExist' newItem (cwdName : cwdRestOfPath) (Dir dirName dirItems) =
      let (cwdDir, otherDirItems) = case find ((cwdName ==) . fsItemName) dirItems of
            Just d -> (d, delete d dirItems)
            Nothing -> (Dir cwdName [], dirItems)
          cwdDir' = addFsItemIfDoesntExist' newItem cwdRestOfPath cwdDir
          dirItems' = cwdDir' : otherDirItems
       in Dir dirName dirItems'

---------------------------------------------------------------------------------
