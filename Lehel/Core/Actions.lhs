This module collects every action-related type and function.

> module Lehel.Core.Actions (
>                            ActionResult(..),
>                            exit
>                           )
> where

Every action must result a value of the type ActionResult:

> data ActionResult = ExitRequest
>                   | Error String
>                     deriving (Eq, Show)

Some very simple actions are defined here:

> exit :: IO (ActionResult)
> exit = return $ ExitRequest
