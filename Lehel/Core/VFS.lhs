
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
>                    itemChange :: String -> IO (Maybe Item)
>                  } deriving Typeable

We have to specifiy Item as an instance of Show, to better support
debugging in other areas:

> instance Show Item where
>     show item = show $ itemName item

> instance Eq Item where
>     i1 == i2 = (itemUniqueName i1) == (itemUniqueName i2)

