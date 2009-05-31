This module implements Lehel's virtual file system functionality over the real file system.

> module Lehel.Core.FileSystems.RealFileSystem (
>                                               realFileSystemItem
>                                              )
> where

We have to import the VFS module which we'll use:

> import Lehel.Core.VFS

To handle file modes we need bitwise operators:

> import Data.Bits
> import Numeric

And we will also using file path and directory related modules:

> import System.IO
> import System.FilePath
> import System.Directory
> import System.Posix.Files

Our main exported function creates a VFS Item value from a simple file system path:

> realFileSystemItem :: FilePath -> Item
> realFileSystemItem path = Item { 
>                             itemUniqueName = "lehel:rfs://" ++ normalizedPath,
>                             itemName = takeFileName normalizedPath,
>                             itemFullPath = normalizedPath,
>                             itemChange = changeImpl,
>                             itemChildren = childrenImpl,
>                             itemIsDirectory = isDirectoryImpl,
>                             itemIsExecutable = isExecutableImpl
>                           }
>     where
>       normalizedPath = normalise path
>       changeImpl newPath = do newPath' <- canonicalizePath $ normalise $ path </> newPath
>                               exists <- doesDirectoryExist newPath'
>                               case exists of
>                                   True ->  return $ Just $ realFileSystemItem newPath'
>                                   False -> return Nothing
>
>       childrenImpl = do paths <- getDirectoryContents normalizedPath
>                         return $ map (\p -> realFileSystemItem (path </> p)) paths
>
>       isDirectoryImpl = do status <- getFileStatus normalizedPath
>                            return $ isDirectory status
>
>       isExecutableImpl = do status <- getFileStatus normalizedPath
>                             let mode = fileMode status                             
>                             return $ (mode .&. 0o111) > 0
