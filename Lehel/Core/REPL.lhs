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

The main loop of the command interpreter part is the lehelREPL function:

> lehelREPL :: (FrontEnd f) => f -> IO ()
> lehelREPL frontend = do maybeLine <- readline "> "
>                         case maybeLine of
>                             Nothing -> return ()
>                             Just line -> do addHistory line
>                                             result <- eval line         
>                                             case result of
>                                                 ExitRequest -> return ()
>                                                 _ -> do showResult frontend result
>                                                         lehelREPL frontend
