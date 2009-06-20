> {-# OPTIONS_GHC -XDeriveDataTypeable -XTypeSynonymInstances -XFlexibleInstances #-}

This module collects every action-related type and function.

> module Lehel.Core.Actions (
>                            ActionResult(..),
>                            ItemsHint(..),
>                            exit,
>                            print,
>                            switchL, switchR, toggle,
>                            pwd, pwdL, pwdR, pwdLR,
>                            cd, cdL, cdR,
>                            ls, lsL, lsR,
>                            sort, sortBy,
>                            run, runL, runR,
>                            addFilter
>                           )
> where

We have to hide some Prelude functions. It is important that these functions
must be hidden in the dynamically evaluated code as well (see Eval.lhs):

> import Prelude hiding (print)

We'll use some data structure related functions, but import them qualified as
they could collide with simple actions:

> import qualified Data.List as List
> import qualified Data.Set as Set

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
> import Control.Monad.State

Action results can also contain \emph{Items}, which are the base data structures
for handling file systems in Lehel. It must be imported from the following 
module:

> import Lehel.Core.VFS
> import Lehel.Core.FileSystems.RealFileSystem

Every action must result a value of the type ActionResult:

> data ActionResult = InvalidCall
>                   | ExitRequest
>                   | ResultSuccess
>                   | ResultString String
>                   | ResultItems (Set.Set ItemsHint) [Item]
>                   | Error String
>                     deriving (Eq, Show, Typeable)

The following flags can be used in the frontends to improve the action result display:

> data ItemsHint = NoItemsHint | ShowItemsFullPath deriving (Show, Eq, Ord, Typeable)

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

> singlePanelAction2 fnImpl = (fn, fnL, fnR)
>     where
>       fn x y = do ps <- getCurrentPanelState
>                   (ps', r) <- fnImpl ps x y
>                   case ps' of
>                            Just ps'' -> setCurrentPanelState ps'' >> return r
>                            Nothing -> return r
>       fnL x y = do ps <- getLeftPanelState
>                    (ps', r) <- fnImpl ps x y
>                    case ps' of
>                             Just ps'' -> setLeftPanelState ps'' >> return r
>                             Nothing -> return r                
>       fnR x y = do ps <- getRightPanelState
>                    (ps', r) <- fnImpl ps x y
>                    case ps' of
>                             Just ps'' -> setRightPanelState ps'' >> return r
>                             Nothing -> return r

Some simple basic actions are defined here:

The following action signals an exit request:

> exit :: LehelStateWithIO (ActionResult)
> exit = return $ ExitRequest

And the following one returns a string to the frontend which will be displayed
to the user:

> print :: String -> LehelStateWithIO (ActionResult)
> print s = return $ (ResultString s)

The next three functions are responsible for switching between panels:

> switchL :: LehelStateWithIO (ActionResult)
> switchL = do ls <- get
>              put $ ls { lsCurrentPanel = LeftPanel }
>              return ResultSuccess
> switchR = do ls <- get
>              put $ ls { lsCurrentPanel = RightPanel }
>              return ResultSuccess
> toggle = do ls <- get
>             case (lsCurrentPanel ls) of
>               LeftPanel -> switchR
>               RightPanel-> switchL

We can easily get the current directory of the panels:

> pwd, pwdL, pwdR :: LehelStateWithIO (ActionResult)
> (pwd, pwdL, pwdR) = singlePanelAction0 pwdImpl
>     where 
>       pwdImpl ps = return (Nothing, ResultString $ itemFullPath $ psCurrentDir ps)

The @pwdLR@ helper function returns both working directories:

> pwdLR :: LehelStateWithIO (ActionResult)
> pwdLR = do left <- getLeftPanelState
>            right <- getRightPanelState
>            return $ ResultItems (Set.singleton ShowItemsFullPath) [psCurrentDir left,
>                                                                    psCurrentDir right]

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
>                      return (Nothing, ResultItems (Set.empty) children)

We lift the simple sort function to the level of actions to allow users to manipulate "ls" action's result
in an easy way:

> sort :: LehelStateWithIO (ActionResult) -> LehelStateWithIO (ActionResult)
> sort action = do result <- action
>                  case result of
>                    ResultItems hint items -> return $ ResultItems hint (List.sort items)
>                    Error str -> return $ Error str
>                    _ -> return $ Error "Cannot sort this kind of data"

> sortBy :: (Item -> Item -> Ordering) -> LehelStateWithIO (ActionResult) -> LehelStateWithIO (ActionResult)
> sortBy fn action = do result <- action
>                       case result of
>                                   ResultItems hint items -> return $ ResultItems hint (List.sortBy fn items)
>                                   Error str -> return $ Error str
>                                   _ -> return $ Error "Cannot sort this kind of data"

One of the most important basic actions is run. Although it has a special syntax processed by input filters,
it is a simple Lehel action just like the other ones, taking the path of the executable and a list of parameters.
If the path is relative, it is first looked up in the current directory, then in the system's search path.

> run, runL, runR :: String -> [String] -> LehelStateWithIO (ActionResult)
> (run, runL, runR) = singlePanelAction2 runImpl
>     where
>       runImpl ps exe params = do let cdir = psCurrentDir ps
>                                  children <- liftIO $ itemChildren cdir
>                                  let localExe = List.find (\i -> itemName i == exe) children
>                                  case localExe of
>                                    Just exeItem -> do liftIO $ (itemExecute exeItem) params (itemFullPath cdir)
>                                                       return (Nothing, ResultSuccess)
>                                    Nothing -> do searchedExe <- liftIO $ findExecutable exe
>                                                  case searchedExe of
>                                                    Just foundExe -> do liftIO $ (itemExecute (realFileSystemItem foundExe)) 
>                                                                                    params (itemFullPath cdir)
>                                                                        return (Nothing, ResultSuccess)
>                                                    Nothing -> return (Nothing, Error "Could not find executable")
>   

The following function is a simple wrapper around @registerFilter@, to be available for users:

> addFilter f = (registerFilter f) >> (return ResultSuccess)
