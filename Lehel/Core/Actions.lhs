This module collects every action-related type and function.

> module Lehel.Core.Actions (
>                            ActionResult(..),
>                            exit,
>                            print
>                           )
> where

We have to hide some Prelude functions. It is important that these functions
must be hidden in the dynamically evaluated code as well (see Eval.lhs):

> import Prelude hiding (print)

Every action must result a value of the type ActionResult:

> data ActionResult = InvalidCall
>                   | ExitRequest
>                   | ResultString String
>                   | Error String
>                     deriving (Eq, Show)

Some very simple actions are defined here:

The following action signals an exit request:

> exit :: IO (ActionResult)
> exit = return $ ExitRequest

And the following one returns a string to the frontend which will be displayed
to the user:

> print :: String -> IO (ActionResult)
> print s = return $ (ResultString s)
