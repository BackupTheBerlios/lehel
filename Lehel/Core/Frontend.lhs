The Frontend module contains functions that works as an interface towards
the frontend to be accessible from the Core.

> module Lehel.Core.Frontend (
>                             FrontEnd(..)
>                            )
> where

The Actions module is needed because some of the interface functions
definitions use its types:

> import Lehel.Core.Actions

The FrontEnd type class contains the basic functions that must be implemented
in every frontend.

> class FrontEnd f where
>     showResult :: f -> ActionResult -> IO ()
