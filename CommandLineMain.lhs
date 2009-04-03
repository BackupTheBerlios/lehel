This module contains the main function of the command line frontend version.

> module Main
> where

We'll have to initialize our state:

> import Control.Monad.State

We import the Core's appropriate modules:

> import Lehel.Core.REPL
> import Lehel.Core.Actions
> import Lehel.Core.Frontend
> import Lehel.Core.State

The following type has no meaning yet, it is only here 
to be able to create an instance of FrontEnd

> data FrontEndState = FES

The FrontEnd interface implementation, very simple yet:

> instance FrontEnd FrontEndState where
>     showResult _ InvalidCall = putStrLn "!!! invalid call"
>     showResult _ ExitRequest = putStrLn $ "Exit request"
>     showResult _ (Error str) = putStrLn $ "!!! ERROR: " ++ str
>     showResult _ (ResultString str) = putStrLn str

And a constructor function for the front end state:

> createFrontEnd = return $ FES

> main :: IO ()
> main = do frontend <- createFrontEnd        
>           st <- initialState
>           evalStateT (lehelREPL frontend) st

