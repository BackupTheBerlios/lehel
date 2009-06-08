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
> import Data.Dynamic

We also have to determine the path of our interface modules, which is possible
with the auto-generated getLibDir function:

> import Paths_Lehel (getLibDir)

The ActionResult type is the result of evaluated actions:

> import Lehel.Core.Actions (ActionResult(..))

And the whole eval function runs in Lehel's state monad, defined in:

> import Lehel.Core.State

For which we'll use liftIO as well:

> import Control.Monad.Trans (liftIO)

The eval function evaluates single actions:

> eval :: String -> LehelStateWithIO (ActionResult)
> eval input = do ifacePath <- liftIO $ getLibDir
>                 evalResult <- liftIO $ lehelEval_ input ["Lehel.Core.Actions", "Lehel.Core.InputFilters"] ["-package Lehel"] [] [ifacePath]
>                 case evalResult of
>                   Left msgs -> return $ Error $ concatMap ((++) "\n") msgs
>                   Right (Just action) -> action
>                   Right Nothing -> return $ Error $ "Invalid command"

We need to use our own eval_ and unsafeEval_ implementation, to be able to hide Prelude items
in the generated code (the difference from the plugins package is only the usage of \emph{lehelWrap} and \emph{lehelDynWrap}.

Currently we provide here both the unsafe and the dynamic based functions to ease experiments. Later probably only one of them will be necessary.

The type-safe version:

> lehelEval_ :: Typeable a =>
>               String           -- ^ code to compile
>            -> [Import]         -- ^ any imports
>            -> [String]         -- ^ extra make flags
>            -> [FilePath]       -- ^ (package.confs) for load
>            -> [FilePath]       -- ^ include paths load is to search in
>            -> IO (Either [String] (Maybe a)) -- ^ either errors, or maybe a well typed value

> lehelEval_ src mods args ldflags incs = do
>     pwd                <- getCurrentDirectory
>     tmpf               <- mkUniqueWith lehelDynWrap src mods
>     status             <- make tmpf $ ["-O0", "-fglasgow-exts", "-package plugins"] ++ args
>     m_rsrc <- case status of
>         MakeSuccess _ obj -> do
>            m_v <- dynload obj (pwd:incs) (ldflags) symbol
>            return $ case m_v of LoadFailure e      -> Left e
>                                 LoadSuccess _ rsrc -> Right (Just rsrc)
>         MakeFailure err -> return $ Left err
>     makeCleaner tmpf
>     return m_rsrc

And the unsafe version:

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

The custom wrappers will use the following function to hide some imports from Prelude:

> lehelImportPrelude = "import Prelude hiding (print)\n"

Our custom safe wrapper function:

> lehelDynWrap :: String -> String -> [Import] -> String
> lehelDynWrap expr nm mods =
>         "module "++nm++ "( resource ) where\n" ++
>         lehelImportPrelude ++
>          concatMap (\m-> "import "++m++"\n") mods ++
>         "import Data.Dynamic\n" ++
>         "resource = let { "++x++" = \n" ++
>         "{-# LINE 1 \"<eval>\" #-}\n" ++ expr ++ ";} in toDyn "++x
>     where
>         x = ident ()

And our custom unsafe wrapper function:

> lehelWrap :: String -> String -> [Import] -> String
> lehelWrap expr nm mods =
>        "module "++nm++ "( resource ) where\n" ++
>        lehelImportPrelude ++
>        concatMap (\m-> "import "++m++"\n") mods ++
>        "resource = let { "++x++" = \n" ++
>        "{-# LINE 1 \"<Plugins.Eval>\" #-}\n" ++ expr ++ ";} in "++x
>    where
>        x = ident ()

And an utility function to generate identifiers:

> ident () = unsafePerformIO $
>            sequence (take 3 (repeat $ getStdRandom (randomR (97,122)) >>= return . chr))
