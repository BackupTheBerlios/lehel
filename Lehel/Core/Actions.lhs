> {-# OPTIONS_GHC -XDeriveDataTypeable -XTypeSynonymInstances -XFlexibleInstances #-}

This module collects every action-related type and function.

> module Lehel.Core.Actions (
>                            ActionResult(..),
>                            exit,
>                            print,
>                            pwd
>                           )
> where

We have to hide some Prelude functions. It is important that these functions
must be hidden in the dynamically evaluated code as well (see Eval.lhs):

> import Prelude hiding (print)

To be able to use the type-safe dynamic evaluation, the result type will
have to be \emph{typeable}. For this reason we import the appropriate module:

> import Data.Typeable

The actions will run in Lehel's state monad, which is defined in the State module:

> import Lehel.Core.State

Every action must result a value of the type ActionResult:

> data ActionResult = InvalidCall
>                   | ExitRequest
>                   | ResultString String
>                   | Error String
>                     deriving (Eq, Show, Typeable)

As it was stated, this data type must be an instance of \emph{Typeable} to be
able to perform type-safe dynamic evaluation. This is done automatically by deriving.
Note that this needs the -XDeriveDataTypeable compiler flag to be specified.

The monadic type LehelStateWithIO (ActionResult) must be instantiated manually:

> instance Typeable (LehelStateWithIO ActionResult) where
>     typeOf _ = mkTyConApp (mkTyCon "Lehel.Core.LehelState-withIO-Lehel.Core.ActionResult") []

We've used -XTypeSynonymInstances to improve readibility here, and -XFlexibleInstances to be able to specify
ActionResult in the head of the instance declaration.

Some very simple actions are defined here:

The following action signals an exit request:

> exit :: LehelStateWithIO (ActionResult)
> exit = return $ ExitRequest

And the following one returns a string to the frontend which will be displayed
to the user:

> print :: String -> LehelStateWithIO (ActionResult)
> print s = return $ (ResultString s)

We can easily get the current directory of the panels:

> pwd :: LehelStateWithIO (ActionResult)
> pwd = do ps <- getCurrentPanelState
>          return $ ResultString $ psCurrentDir ps
