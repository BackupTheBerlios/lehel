This module contains functions for applying input filters, and also
some default input filter functions.

Input filters are simple string manipulators applied on user input inside
the REPL. 

> module Lehel.Core.InputFilters (
>                                InputFilter,
>                                actionWrapperFilter,
>                                filterInput,
>                                simpleCDFilter,
>                                runFilter
>                               )

> where

> import Control.Monad
> import Control.Monad.State (get)
> import Data.Maybe
> import Data.List
> import Text.Regex

The filters will be able to use Lehel's state, so we'll need the state 
module:

> import Lehel.Core.State

The @filterInput@ function uses the state monad's list of registered
input filters to process an input line and return a result string.

The function applies every registered input filter in their registration 
order.

> filterInput :: String -> LehelStateWithIO String
> filterInput input = 
>     do ls <- get
>        let filters = lsFilters ls
>        processInput input filters
>     where
>       processInput i [] = return i
>       processInput i (f:fs) =
>           do i' <- f i
>              processInput (fromMaybe i i') fs

The @actionWrapperFilter@ is an input filter which is executed always, to
wrap any data structure that belongs to the @Action@ typeclass into @ActionWrapper@
records, to ensure that dynamic evaluation works correctly.

> actionWrapperFilter :: InputFilter
> actionWrapperFilter input = return (Just ("wrapAction $ " ++ input))

The @simpleCDFilter@ is an input filter that allows typing cd, cdL and cdR commands
without enclosing the target directory between "". 

> simpleCDFilter :: InputFilter
> simpleCDFilter = return . simpleCDFilter'

It is implemented in a pure function:

> simpleCDFilter' :: String -> Maybe String
> simpleCDFilter' input = 
>     let regex = mkRegex "^(cd[LR]?)\\s+([^\\s]+)$"
>         match = matchRegex regex input
>     in
>       case match of
>         Just [cmd, path] -> Just $ cmd ++ " \"" ++ path ++ "\""
>         _ -> Nothing

The @runFilter@ allows the user to execute commands with a more common syntax, by prefixing it
with a dot (.) character.

> runFilter :: InputFilter
> runFilter = return . runFilter'

It is also implemented in a pure function:

> runFilter' :: String -> Maybe String
> runFilter' ('.':'/':input) = Just $ buildRunCommand input
> runFilter' ('.':input) = Just $ buildRunCommand input
> runFilter' _ = Nothing

The command line is parsed and converted to a run call by the following function:

> buildRunCommand :: String -> String
> buildRunCommand input = 
>     let params = splitInput input in "run " ++ quoted (head params) ++ " " ++ show (tail params)
>     where
>       quoted str = "\"" ++ str ++ "\""
>       splitInput str = reverse $ splitInput' (words str) 0 []
>       splitInput' [] _ w = w
>       splitInput' (('\"':w0):s) 0 w = splitInput' s 1 (w0:w)
>       splitInput' (('\"':w1):s) n (w0:w) = splitInput' s (n+1) ((w0++" " ++w1):w)
>       splitInput' (w0:s) 0 w = splitInput' s 0 (w0:w)
>       splitInput' (w1:s) n (w0:w) =
>           let ec = last w1
>           in if ec == '\"' 
>              then splitInput' s (n-1) ((w0++" "++ take (length w1 - 1) w1):w)
>              else splitInput' s n ((w0 ++ " " ++ w1):w)

