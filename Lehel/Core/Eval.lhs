The Eval module contains the functions for evaluating single-line user input
and script files as well.

> module Lehel.Core.Eval (
>                         eval
>                        )
> where

The plugins library is used for runtime evaluation. We need several modules because
we are implementing a custom eval function.

> import System.Plugins.Make
> import System.Plugins.Load
> import System.Eval.Haskell hiding (eval)
> import System.Eval.Utils
> import System.Directory
> import System.Random
> import System.IO.Unsafe
> import Data.Char

We also have to determine the path of our interface modules, which is possible
with the auto-generated getLibDir function:

> import Paths_Lehel (getLibDir)

The ActionResult type is the result of evaluated actions:

> import Lehel.Core.Actions (ActionResult(..))

The eval function evaluates single actions:

> eval :: String -> IO (ActionResult)
> eval input = do ifacePath <- getLibDir
>                 evalResult <- lehelUnsafeEval_ input ["Lehel.Core.Actions"] ["-package Lehel"] [] [ifacePath]
>                 case evalResult of
>                   Left msgs -> return $ Error $ concatMap ((++) "\n") msgs
>                   Right action -> action

We need to use our own unsafeEval_ implementation, to be able to hide Prelude items
in the generated code:

> lehelUnsafeEval_ :: String           -- ^ code to compile
>                  -> [Import]         -- ^ any imports
>                  -> [String]         -- ^ make flags
>                  -> [FilePath]       -- ^ (package.confs) for load
>                  -> [FilePath]       -- ^ include paths load is to search in
>                  -> IO (Either [String] a)

> lehelUnsafeEval_ src mods args ldflags incs = do
>     pwd  <- getCurrentDirectory
>     tmpf <- mkUniqueWith lehelWrap src mods
>     status <- make tmpf args
>     e_rsrc <- case status of
>         MakeSuccess _ obj  -> do
>            m_v <- load obj (pwd:incs) ldflags symbol
>            case m_v of LoadFailure e      -> return $ Left e
>                        LoadSuccess _ rsrc -> return $ Right rsrc
>         MakeFailure err -> return $ Left err
>     makeCleaner tmpf
>     return e_rsrc

And our custom unsafe wrapper function:

> lehelWrap :: String -> String -> [Import] -> String
> lehelWrap expr nm mods =
>        "module "++nm++ "( resource ) where\n" ++
>        "import Prelude hiding (print)\n" ++
>        concatMap (\m-> "import "++m++"\n") mods ++
>        "resource = let { "++x++" = \n" ++
>        "{-# LINE 1 \"<Plugins.Eval>\" #-}\n" ++ expr ++ ";} in "++x
>    where
>        x = ident ()

> ident () = unsafePerformIO $
>            sequence (take 3 (repeat $ getStdRandom (randomR (97,122)) >>= return . chr))
