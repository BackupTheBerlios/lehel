
> {-# OPTIONS_GHC -XDeriveDataTypeable  #-}

This module contains the data types requied for handling any kind of file system-like 
sources such as real file systems, archive files, FTP/SSH links, etc.

> module Lehel.Core.VFS (
>                         Item(..)
>                       )
> where

> import Data.Typeable
  
The basic item of a file system is called Item:

> data Item = Item { 
>                    itemUniqueName :: String,
>                    itemName :: String,
>                    itemFullPath :: String,
>                    itemChange :: String -> IO (Maybe Item),
>                    itemChildren :: IO [Item],
>                    itemExecute :: [String] -> FilePath -> IO (),
>                    itemIsDirectory :: IO Bool,
>                    itemIsExecutable :: IO Bool
>                  } deriving Typeable

It is currently implemented as a record. Probably it will be a type class later, but it 
is currently not the focus of the development, so it is quite minimalistic.

We have to specifiy Item as an instance of Show, to better support
debugging in other areas:

> instance Show Item where
>     show = itemName

The equality can be checked using the unique name of the items:

> instance Eq Item where
>     i1 == i2 = (itemUniqueName i1) == (itemUniqueName i2)

The standard ordering is done based on the item's name:

> instance Ord Item where
>     compare i1 i2 = compare (itemName i1) (itemName i2)

