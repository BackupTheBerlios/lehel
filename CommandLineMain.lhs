> {-# OPTIONS_GHC -XNamedFieldPuns #-}

This module contains the main function of the command line frontend version.

> module Main
> where

We'll have to initialize our state:

> import Control.Monad.State

The initial state will point to the system's current directory:

> import System.Directory (getCurrentDirectory)

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
>     showResult _ (ResultItems items) = mapM_ (\a -> (return (showItem a)) >>= putStrLn) items
>     showResult _ ResultSuccess = return ()

And a constructor function for the front end state:

> createFrontEnd = return $ FES

The following function converts an Item to a string, used by showResult:

> showItem :: Item -> String
> showItem (Item { itemName }) = itemName


And the main function:

> main :: IO ()
> main = do frontend <- createFrontEnd        
>           curdir <- getCurrentDirectory
>           st <- initialState (realFileSystemItem curdir)
>           evalStateT (lehelREPL frontend) st

