This module implements Lehel's virtual file system functionality over the real file system.

> module Lehel.Core.FileSystems.RealFileSystem (
>                                               realFileSystemItem
>                                              )
> where

We have to import the VFS module which we'll use:

> import Lehel.Core.VFS

To handle file modes we need bitwise operators:

> import Data.Bits

And we will also using file path and directory related modules:

> import System.FilePath
> import System.Directory
> import System.Posix.Files
> import System.Process

Our main exported function creates a VFS Item value from a simple file system path:

> realFileSystemItem :: FilePath -> Item
> realFileSystemItem path = Item { 
>                             itemUniqueName = "lehel:rfs://" ++ normalizedPath,
>                             itemName = takeFileName normalizedPath,
>                             itemFullPath = normalizedPath,
>                             itemChange = changeImpl,
>                             itemChildren = childrenImpl,
>                             itemExecute = executeImpl,
>                             itemIsDirectory = isDirectoryImpl,
>                             itemIsExecutable = isExecutableImpl
>                           }
>     where
>       normalizedPath = normalise path
>       changeImpl newPath = do newPath' <- canonicalizePath $ normalise $ path </> newPath
>                               exists <- doesDirectoryExist newPath'
>                               if exists then return $ Just $ realFileSystemItem newPath' else return Nothing
>
>       childrenImpl = do paths <- getDirectoryContents normalizedPath
>                         return $ map (\p -> realFileSystemItem (path </> p)) paths
>
>       executeImpl params wdir = do let cp = CreateProcess {
>                                               cmdspec = RawCommand normalizedPath params,
>                                               cwd = Just wdir,
>                                               std_in = Inherit,
>                                               std_out = Inherit,
>                                               std_err = Inherit,
>                                               env = Nothing,
>                                               close_fds = False
>                                             }
>                                    (_,_,_,pid) <- createProcess cp
>                                    waitForProcess pid
>                                    return ()
>
>
>       isDirectoryImpl = do status <- getFileStatus normalizedPath
>                            return $ isDirectory status
>
>       isExecutableImpl = do status <- getFileStatus normalizedPath
>                             let mode = fileMode status                             
>                             return $ (mode .&. 0o111) > 0
