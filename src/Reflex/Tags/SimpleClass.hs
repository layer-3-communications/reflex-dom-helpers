{-|
Module      : Reflex.Tags.Simple
Description : reflex-dom simple tags
Copyright   : (c) Layer 3 Communications, 2016
                  Matthew Parsons, 2016
License     : BSD3
Maintainer  : parsonsmatt@gmail.com
Stability   : experimental
Portability : POSIX

This module is intended to be imported qualified. Allows you to use tags with a prefix.

This module exports all of the HTML tags for the 'el' function.
-}

module Reflex.Tags.SimpleClass where

import qualified Prelude as P

import Reflex.Dom.Widget (el)
import Reflex.Tags.TH

genClass ""
