import System.IO
import System.FilePath
import System.Environment
import System.Directory
import System.Process
import System.Exit

__robocopy src dst file =
  copyFile (src </> file) (dst </> file)

robocopy src dst file = do
  putStr $ "Syncing " ++ (src </> file) ++ " --> " ++ (dst </> file) ++ " ... "
  hFlush stdout
  (exitCode, stdout, stderr) <- readProcessWithExitCode "robocopy.exe" [src, dst, file] ""
  if not (exitOk exitCode) then error( "robocopy failed with " ++ show exitCode ) else do
    putStrLn "DONE"
  where
    exitOk ExitSuccess = True
    exitOk (ExitFailure 1) = True
    exitOk _ = False

rename src dst = do
  renameFile src dst
  putStrLn $ "Moved " ++ src ++ " --> " ++ dst

assertDirectory path = doesDirectoryExist path >>= test where
  test True = return ()
  test _ = error $ "Directory " ++ path ++ " does not exist."

assertFile path = doesFileExist path >>= test where
  test True = return ()
  test _ = error $ "File " ++ path ++ " does not exist."
  
copy srcPath dstPath = do
  assertDirectory srcD
  assertFile srcPath
  dstIsDir <- doesDirectoryExist dstPath
  case dstIsDir of
    True -> robocopy srcD dstPath srcF
    _ -> robocopy srcD dstD srcF >> rename (dstD </> srcF) (dstD </> dstF)
  where
    (srcD, srcF) = splitFileName srcPath
    (dstD, dstF) = splitFileName dstPath
    
main = do
  args <- getArgs
  case args of
    [ srcF, dstF ] ->
      copy srcF dstF
    _ -> error "usage: robosync <src file> <dest file>"

  

