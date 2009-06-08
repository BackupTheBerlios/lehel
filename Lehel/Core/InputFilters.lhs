This module contains functions for applying input filters, and also
some default input filter functions.

Input filters are simple string manipulators applied on user input inside
the REPL. 

> module Lehel.Core.InputFilters (
>                                InputFilter,
>                                filterInput,
>                                simpleCDFilter,
>                               )

> where

> import Control.Monad
> import Control.Monad.State (get)
> import Data.Maybe
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
>        input' <- processInput input filters
>        return input'
>     where
>       processInput i [] = return i
>       processInput i (f:fs) =
>           do i' <- f i
>              processInput (fromMaybe i i') fs


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

