> {-# OPTIONS_GHC -XDeriveDataTypeable -XTypeSynonymInstances #-}

This module collects every action-related type and function.

> module Lehel.Core.Actions (
>                            ActionResult(..),
>                            Action(..),
>                            ActionWrapper(..),
>                            wrapAction,
>                            ItemsHint(..),
>                            ExitAction(..),
>                            PrintAction(..),
>                            SwitchAction(..),
>                            PwdAction(..),
>                            CdAction(..),
>                            LsAction(..),
>                            RunAction(..),
>                            AddFilterAction(..),
>                            sort, sortBy
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

> import System.FilePath
> import System.Directory

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

> instance Typeable1 LehelStateWithIO where
>     typeOf1 _ = mkTyConApp (mkTyCon "Lehel.Core.LehelState-withIO") []

We've used -XTypeSynonymInstances to improve readibility here.

The actions will be represented with a type class that defines how they can be ran and combined
with each other:

> class Action a where
>     runAction :: a -> LehelStateWithIO (ActionResult)

For this typeclass we have to create a concrete wrapper structure, to avoid 
problems when evaluating problems at runtime. This wrapper structure will also
belong to the Action typeclass, so this wrapping has no effect to most of the functions in the 
application:

> data ActionWrapper = WrappedAction { runWrappedAction :: LehelStateWithIO (ActionResult) }
>                      deriving Typeable

> instance Action ActionWrapper where
>     runAction = runWrappedAction

The @wrapAction@ function simple wraps any @Action@ instance to this record:

> wrapAction :: (Action a) => a -> ActionWrapper
> wrapAction a = WrappedAction {
>                  runWrappedAction = runAction a
>                }

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

\subsection{Actions}
In the next section we'll define various actions and their possible relationships,
using the above defined type classes.

\subsubsection{Exit action}
The following data type represents an exit action:

> data ExitAction = Exit deriving Typeable

It is an action, and running it simply returns an exit request:

> instance Action ExitAction where
>     runAction _ = return ExitRequest

\subsubsection{Print action}
The print action returns a string to the frontend which will be displayed
to the user:

> data PrintAction = Print String deriving Typeable

> instance Action PrintAction where
>     runAction (Print str) = return (ResultString str)

\subsubsection{Switching between panels}
The next three functions are responsible for switching between panels:

> data SwitchAction = SwitchL | SwitchR | Toggle deriving Typeable

> instance Action SwitchAction where
>     runAction SwitchL = do lst <- get
>                            put $ lst { lsCurrentPanel = LeftPanel }
>                            return ResultSuccess
>     runAction SwitchR = do lst <- get
>                            put $ lst { lsCurrentPanel = RightPanel }
>                            return ResultSuccess
>     runAction Toggle = do lst <- get
>                           case (lsCurrentPanel lst) of
>                             LeftPanel -> runAction SwitchL
>                             RightPanel-> runAction SwitchR

\subsubsection{Getting the current directory}
We can easily get the current directory of the panels. The command is named as
\emph{print working directory}, to make it more convenient for unix users.

> data PwdAction = Pwd | PwdL | PwdR | PwdLR deriving Typeable

> instance Action PwdAction where
>     runAction PwdLR = do left <- getLeftPanelState
>                          right <- getRightPanelState
>                          return $ ResultItems (Set.singleton ShowItemsFullPath) [psCurrentDir left,
>                                                                                  psCurrentDir right]
>     runAction p = case p of
>                     Pwd -> pwd
>                     PwdL -> pwdL
>                     PwdR -> pwdR                     
>                   where
>                     pwdImpl ps = return (Nothing, ResultString $ itemFullPath $ psCurrentDir ps)
>                     (pwd, pwdL, pwdR) = singlePanelAction0 pwdImpl

\subsubsection{Change directory action}
Changing the current directory is similar to the previous action (@pwd@), but 
it also has a parameter:

> data CdAction = Cd FilePath | CdL FilePath | CdR FilePath deriving Typeable

> instance Action CdAction where
>     runAction p = case p of
>                     Cd path -> cd path
>                     CdL path -> cdL path
>                     CdR path -> cdR path
>                   where
>                     cdImpl ps newPath = do let oldItem = psCurrentDir ps
>                                            newItem <- liftIO (itemChange oldItem newPath)
>                                            case newItem of
>                                              Just newItem' -> return (Just (ps { psCurrentDir = newItem' }), ResultSuccess)
>                                              Nothing -> return (Nothing, Error "Directory doesn't exist")
>                     (cd, cdL, cdR) = singlePanelAction1 cdImpl

\subsubsection{List action}
The list command (named @ls@ to be familiar for users) returns the child items (if there is any)
of the active (or given) panel's current item:

> data LsAction = Ls | LsL | LsR deriving Typeable

> instance Action LsAction where
>     runAction p = case p of
>                     Ls -> ls
>                     LsL-> lsL
>                     LsR-> lsR
>                   where
>                     lsImpl ps = do children <- liftIO $ itemChildren $ psCurrentDir ps
>                                    return (Nothing, ResultItems Set.empty children)
>                     (ls, lsL, lsR) = singlePanelAction0 lsImpl

We lift the simple sort function to the level of actions to allow users to manipulate "ls" action's result
in an easy way:

> sort :: (Action a) => a -> LehelStateWithIO (ActionResult)
> sort action = do result <- runAction action
>                  case result of
>                    ResultItems hint items -> return $ ResultItems hint (List.sort items)
>                    Error str -> return $ Error str
>                    _ -> return $ Error "Cannot sort this kind of data"

> sortBy :: (Action a) => (Item -> Item -> Ordering) -> a -> LehelStateWithIO (ActionResult)
> sortBy fn action = do result <- runAction action
>                       case result of
>                                   ResultItems hint items -> return $ ResultItems hint (List.sortBy fn items)
>                                   Error str -> return $ Error str
>                                   _ -> return $ Error "Cannot sort this kind of data"

\subsubsection{Run program action}
One of the most important basic actions is run. Although it has a special syntax processed by input filters,
it is a simple Lehel action just like the other ones, taking the path of the executable and a list of parameters.
If the path is relative, it is first looked up in the current directory, then in the system's search path.

> data RunAction = Run String [String]
>                | RunL String [String]
>                | RunR String [String] deriving Typeable

> instance Action RunAction where
>     runAction p = case p of
>                     Run exe params -> run exe params
>                     RunL exe params -> runL exe params
>                     RunR exe params -> runR exe params
>         where
>           runImpl ps exe params = do let cdir = psCurrentDir ps
>                                      children <- liftIO $ itemChildren cdir
>                                      let localExe = List.find (\i -> itemName i == exe) children
>                                      case localExe of
>                                        Just exeItem -> do liftIO $ itemExecute exeItem params (itemFullPath cdir)
>                                                           return (Nothing, ResultSuccess)
>                                        Nothing -> do searchedExe <- liftIO $ findExecutable exe
>                                                      case searchedExe of
>                                                        Just foundExe -> do liftIO $ itemExecute (realFileSystemItem foundExe) 
>                                                                                     params (itemFullPath cdir)
>                                                                            return (Nothing, ResultSuccess)
>                                                        Nothing -> return (Nothing, Error "Could not find executable")
>           (run, runL, runR) = singlePanelAction2 runImpl
>   

\subsubsection{Add filter action}
The following action is a simple wrapper around @registerFilter@, to be available for users:

> data AddFilterAction = AddFilter InputFilter deriving Typeable

> instance Action AddFilterAction where
>     runAction (AddFilter f) = registerFilter f >> return ResultSuccess
