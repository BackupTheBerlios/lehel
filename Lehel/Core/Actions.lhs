> {-# OPTIONS_GHC -XDeriveDataTypeable -XTypeSynonymInstances -XFlexibleInstances #-}

This module collects every action-related type and function.

> module Lehel.Core.Actions (
>                            ActionResult(..),
>                            exit,
>                            print,
>                            pwd, pwdL, pwdR,
>                            cd, cdL, cdR,
>                            ls, lsL, lsR
>                           )
> where

We have to hide some Prelude functions. It is important that these functions
must be hidden in the dynamically evaluated code as well (see Eval.lhs):

> import Prelude hiding (print)

To handle paths, we import the appropriate module:

> import System.IO hiding (print)
> import System.FilePath
> import System.Directory

And we'll also use liftIO inside action definitions, to perform IO functions:

> import Control.Monad.Trans (liftIO)

To be able to use the type-safe dynamic evaluation, the result type will
have to be \emph{typeable}. For this reason we import the appropriate module:

> import Data.Typeable

The actions will run in Lehel's state monad, which is defined in the State module:

> import Lehel.Core.State

Action results can also contain \emph{Items}, which are the base data structures
for handling file systems in Lehel. It must be imported from the following 
module:

> import Lehel.Core.VFS

Every action must result a value of the type ActionResult:

> data ActionResult = InvalidCall
>                   | ExitRequest
>                   | ResultSuccess
>                   | ResultString String
>                   | ResultItems [Item]
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

Simple actions can be implemented with one common action function and aliases defined
working for the current panel, the left panel or the right panel. To help implementing
these set of actions, we define the following function constructors:

(Probably Template Haskell could be used to automatize this one?)

> singlePanelAction0 fnImpl = (fn, fnL, fnR)
>     where
>       fn = do ps <- getCurrentPanelState
>               (ps', r) <- fnImpl ps
>               case ps' of
>                 Just ps'' -> setCurrentPanelState ps'' >> return r
>                 Nothing -> return r
>       fnL = do ps <- getLeftPanelState
>                (ps', r) <- fnImpl ps
>                case ps' of
>                 Just ps'' -> setLeftPanelState ps'' >> return r
>                 Nothing -> return r                
>       fnR = do ps <- getRightPanelState
>                (ps', r) <- fnImpl ps
>                case ps' of
>                 Just ps'' -> setRightPanelState ps'' >> return r
>                 Nothing -> return r

> singlePanelAction1 fnImpl = (fn, fnL, fnR)
>     where
>       fn x = do ps <- getCurrentPanelState
>                 (ps', r) <- fnImpl ps x
>                 case ps' of
>                  Just ps'' -> setCurrentPanelState ps'' >> return r
>                  Nothing -> return r
>       fnL x = do ps <- getLeftPanelState
>                  (ps', r) <- fnImpl ps x
>                  case ps' of
>                   Just ps'' -> setLeftPanelState ps'' >> return r
>                   Nothing -> return r                
>       fnR x = do ps <- getRightPanelState
>                  (ps', r) <- fnImpl ps x
>                  case ps' of
>                   Just ps'' -> setRightPanelState ps'' >> return r
>                   Nothing -> return r

Some simple basic actions are defined here:

The following action signals an exit request:

> exit :: LehelStateWithIO (ActionResult)
> exit = return $ ExitRequest

And the following one returns a string to the frontend which will be displayed
to the user:

> print :: String -> LehelStateWithIO (ActionResult)
> print s = return $ (ResultString s)

We can easily get the current directory of the panels:

> pwd, pwdL, pwdR :: LehelStateWithIO (ActionResult)
> (pwd, pwdL, pwdR) = singlePanelAction0 pwdImpl
>     where 
>       pwdImpl ps = return (Nothing, ResultString $ itemFullPath $ psCurrentDir ps)

And similarly change them:

> cd, cdL, cdR :: FilePath -> LehelStateWithIO (ActionResult)
> (cd, cdL, cdR) = singlePanelAction1 cdImpl
>     where
>       cdImpl ps newPath = do let oldItem = psCurrentDir ps
>                              newItem <- liftIO $ (itemChange oldItem) newPath
>                              case newItem of
>                                Just newItem' -> return (Just (ps { psCurrentDir = newItem' }), ResultSuccess)
>                                Nothing -> return (Nothing, Error "Directory doesn't exist")

The list command (named ls to be familiar for users) returns the child items (if there is any)
of the active (or given) panel's current item:

> ls, lsL, lsR :: LehelStateWithIO (ActionResult)
> (ls, lsL, lsR) = singlePanelAction0 lsImpl
>     where
>       lsImpl ps = do children <- liftIO $ itemChildren $ psCurrentDir ps
>                      return (Nothing, ResultItems children)
