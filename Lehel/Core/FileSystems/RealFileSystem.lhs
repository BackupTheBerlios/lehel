This module implements Lehel's virtual file system functionality over the real file system.

> module Lehel.Core.FileSystems.RealFileSystem (
>                                               realFileSystemItem
>                                              )
> where

We have to import the VFS module which we'll use:

> import Lehel.Core.VFS

And we will also using file path and directory related modules:

> import System.IO
> import System.FilePath
> import System.Directory

Our main exported function creates a VFS Item value from a simple file system path:

> realFileSystemItem :: FilePath -> Item
> realFileSystemItem path = Item { 
>                             itemUniqueName = "lehel:rfs://" ++ normalizedPath,
>                             itemName = takeFileName normalizedPath,
>                             itemFullPath = normalizedPath,
>                             itemChange = changeImpl
>                           }
>     where
>       normalizedPath = normalise path
>       changeImpl newPath = do newPath' <- canonicalizePath $ normalise $ path </> newPath
>                               exists <- doesDirectoryExist newPath'
>                               case exists of
>                                   True ->  return $ Just $ realFileSystemItem newPath'
>                                   False -> return Nothing
