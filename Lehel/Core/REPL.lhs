This is the core Read-Eval-Print-Loop, which is based on GNU readline for line editing
and uses the actual frontend to display results of the action.

> module Lehel.Core.REPL (
>                         lehelREPL
>                        ) 
> where

The REPL is using the GNU readline library for getting user input.

> import System.Console.Readline

The Frontend module is used to show result of the evaluted command, and the 
Eval module is used to evaluate the input.

> import Lehel.Core.Eval
> import Lehel.Core.Frontend
> import Lehel.Core.Actions
> import Lehel.Core.InputFilters

The whole REPL runs in Lehel's state monad:

> import Lehel.Core.State

and we'll also use liftIO:

> import Control.Monad.Trans (liftIO)

The main loop of the command interpreter part is the lehelREPL function:

> lehelREPL :: (FrontEnd f) => f -> LehelStateWithIO ()
> lehelREPL frontend = do maybeLine <- liftIO $ readline "> "
>                         case maybeLine of
>                             Nothing -> return ()
>                             Just "" -> lehelREPL frontend
>                             Just line -> do liftIO $ addHistory line
>                                             filteredLine <- filterInput line
>                                             result <- eval filteredLine         
>                                             case result of
>                                                 ExitRequest -> return ()
>                                                 _ -> do liftIO $ showResult frontend result
>                                                         lehelREPL frontend
