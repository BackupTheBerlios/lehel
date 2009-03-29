The Eval module contains the functions for evaluating single-line user input
and script files as well.

> module Lehel.Core.Eval (
>                         eval
>                        )
> where

The plugins library is used for runtime evaluation. The alias E is used to avoid
collision with our eval function.

> import qualified System.Eval.Haskell as E

We also have to determine the path of our interface modules, which is possible
with the auto-generated getLibDir function:

> import Paths_Lehel (getLibDir)

The ActionResult type is the result of evaluated actions:

> import Lehel.Core.Actions (ActionResult(..))

The eval function evaluates single actions:

> eval :: String -> IO (ActionResult)
> eval input = do ifacePath <- getLibDir
>                 evalResult <- E.unsafeEval_ input ["Lehel.Core.Actions"] ["-package Lehel", "-dynamic"] [] [ifacePath]
>                 case evalResult of
>                   Left msgs -> return $ Error $ concatMap ((++) "\n") msgs
>                   Right action -> action

