> {-# OPTIONS_GHC -XDeriveDataTypeable #-}

The State module contains the definition of Lehel's core state type, and all the necessary
data types and instance declarations related to it.

TODO: Explain why are we hardcoding the number of panels (2) instead of a generic solution

> module Lehel.Core.State (
>                          LehelState(..),
>                          Panel(..),
>                          PanelState(..),
>                          LehelStateT,
>                          LehelStateWithIO,
>                          initialState,
>                          getCurrentPanelState,
>                          getLeftPanelState,
>                          getRightPanelState,
>                          setCurrentPanelState,
>                          setLeftPanelState,
>                          setRightPanelState
>                         )
> where
>   

We'll define a state monad:

> import Control.Monad.State

To be able to use the type-safe dynamic evaluation, the state type and the monads will
have to be \emph{typeable}. For this reason we import the appropriate module:

> import Data.Typeable

We'll use getCurrentDirectory when defining the initial state value:

> import System.Directory (getCurrentDirectory)

The state itself is currently very simple:

> data LehelState = LehelState { lsCurrentPanel :: Panel, 
>                                lsLeft :: PanelState,
>                                lsRight :: PanelState
>                              } deriving (Typeable)

Where the state of current panel can be:

> data Panel = LeftPanel | RightPanel deriving (Typeable, Show, Eq)

And for both panels we have a panel state, which currently only holds
the current directory:

> data PanelState = PanelState { psCurrentDir :: FilePath } deriving (Typeable)

The following function gets the panel state depending on the current panel field:

> lsCurrentPanelState :: LehelState -> PanelState
> lsCurrentPanelState (LehelState { lsCurrentPanel = LeftPanel, lsLeft = l }) = l
> lsCurrentPanelState (LehelState { lsCurrentPanel = RightPanel, lsRight = r}) = r

Using these record types we can define our state monad transformer:

> type LehelStateT m a = StateT LehelState m a 

And it will be often combined with IO monad:

> type LehelStateWithIO a = LehelStateT IO a

TODO: LehelStateWithIO must be Typeable

The initial state is defined by the following IO function:

> initialState :: IO (LehelState)
> initialState = do curdir <- getCurrentDirectory
>                   return $ LehelState { lsCurrentPanel = LeftPanel,
>                                         lsLeft = PanelState { psCurrentDir = curdir },
>                                         lsRight = PanelState { psCurrentDir = curdir }
>                                       }

We define some monadic functions for the state monad to be able to get the state:

> getCurrentPanelState :: (Monad m) => LehelStateT m PanelState
> getCurrentPanelState = do ls <- get
>                           let ps = lsCurrentPanelState ls
>                           return ps

We also have to get the left or right panel's state for current-panel-independent
actions:

> getLeftPanelState :: (Monad m) => LehelStateT m PanelState
> getLeftPanelState = do ls <- get
>                        return (lsLeft ls)
> getRightPanelState :: (Monad m) => LehelStateT m PanelState
> getRightPanelState = do ls <- get
>                         return (lsRight ls)

Another set of monadic functions required to change the panel states:

> setCurrentPanelState :: (Monad m) => PanelState -> LehelStateT m ()
> setCurrentPanelState ps = do ls <- get
>                              case ls of
>                                LehelState { lsCurrentPanel = LeftPanel } -> put $ ls { lsLeft = ps }
>                                LehelState { lsCurrentPanel = RightPanel }-> put $ ls { lsRight = ps }

> setLeftPanelState :: (Monad m) => PanelState -> LehelStateT m ()
> setLeftPanelState ps = do ls <- get
>                           put $ ls { lsLeft = ps }
> setRightPanelState :: (Monad m) => PanelState -> LehelStateT m ()
> setRightPanelState ps = do ls <- get
>                            put $ ls { lsRight = ps }
