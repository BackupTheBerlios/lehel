> {-# OPTIONS_GHC -XNamedFieldPuns #-}

This module contains the main function of the command line frontend version.

> module Main
> where

We'll have to initialize our state:

> import Control.Monad.State

The initial state will point to the system's current directory:

> import System.Directory (getCurrentDirectory)
> import System.IO.Error

We need to handle sets when working with action results:

> import Data.Set

We import the Core's appropriate modules:

> import Lehel.Core.REPL
> import Lehel.Core.Actions
> import Lehel.Core.Frontend
> import Lehel.Core.State
> import Lehel.Core.VFS
> import Lehel.Core.FileSystems.RealFileSystem

The following type has no meaning yet, it is only here 
to be able to create an instance of FrontEnd

> data FrontEndState = FES

The FrontEnd interface implementation, very simple yet:

> instance FrontEnd FrontEndState where
>     showResult _ InvalidCall = putStrLn "!!! invalid call"
>     showResult _ ExitRequest = putStrLn $ "Exit request"
>     showResult _ (Error str) = putStrLn $ "!!! ERROR: " ++ str
>     showResult _ (ResultString str) = putStrLn str
>     showResult _ (ResultItems hints items) = mapM_ (\a -> (showItem hints a) >>= putStrLn)items
>     showResult _ ResultSuccess = return ()

And a constructor function for the front end state:

> createFrontEnd = return $ FES

The following function converts an Item to a string, used by showResult:

> showItem :: Set ItemsHint -> Item -> IO String
> showItem hints (Item { itemName, itemFullPath, itemIsDirectory, itemIsExecutable }) =
>     do
>       let name = if (ShowItemsFullPath `member` hints) then itemFullPath else itemName
>       isD <- try itemIsDirectory
>       isX <- try itemIsExecutable
>       case (isD, isX) of
>         (Left err1, _)            -> return $ "    " ++ name ++ " -> " ++ ioeGetErrorString err1
>         (_, Left err2)            -> return $ "    " ++ name ++ " -> " ++ ioeGetErrorString err2
>         (Right True, Right _)     -> return $ "[D] " ++ name
>         (Right False, Right True) -> return $ "[*] " ++ name
>         (Right False, Right False)-> return $ "    " ++ name


And the main function:

> main :: IO ()
> main = do frontend <- createFrontEnd        
>           curdir <- getCurrentDirectory
>           st <- initialState (realFileSystemItem curdir)
>           evalStateT (lehelREPL frontend) st

