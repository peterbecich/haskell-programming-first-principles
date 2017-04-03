module CountEntries where

import System.Directory (doesDirectoryExist, getDirectoryContents)
import System.FilePath ((</>))

import Control.Monad (forM, liftM, when, forM_)
import Control.Monad.Trans (liftIO)
import Control.Monad.Writer (WriterT, tell, runWriterT)

listDirectory :: FilePath -> IO [String]
listDirectory = liftM (filter notDots) . getDirectoryContents
  where notDots pth = pth /= "." && pth /= ".." && notHidden pth

notHidden :: FilePath -> Bool
notHidden (x:xs)
  | x /= '.' = True
  | otherwise = False
notHidden [] = False


-- http://book.realworldhaskell.org/read/monad-transformers.html

countEntries :: FilePath -> IO [(FilePath, Int)]
countEntries path = do
  contents <- listDirectory path
  rest <- forM contents $ (\name -> do
                       let newName = path </> name
                       isDir <- doesDirectoryExist newName
                       if isDir
                         then countEntries newName
                         else return []
                          )
  return $ (path, length contents) : concat rest

pth = "/home/peterbecich/haskell/haskell-programming-first-principles/src"
pth2 = "/home/peterbecich/haskell/haskell-programming-first-principles"
pth3 = "/Users/peterbecich/haskell/haskell-programming-first-principles"
pth4 = "/Users/peterbecich/haskell/haskell-programming-first-principles/src"

countEntries2 :: FilePath -> WriterT [(FilePath, Int)] IO ()
countEntries2 path = do
  contents <- liftIO $ listDirectory path
  _ <- tell [(path, length contents)]
  forM_ contents $ (\name -> do
                       let newName = path </> name
                       isDir <- liftIO $ doesDirectoryExist $ newName
                       when isDir $ countEntries2 newName
                   )

countEntries2' :: FilePath -> IO ( (), [(FilePath, Int)])
countEntries2' path = runWriterT $ countEntries2 path


