{-|
Module      : Reflex.Tags.SimplePrime
Description : reflex-dom simple tags
Copyright   : (c) Layer 3 Communications, 2016
                  Matthew Parsons, 2016
License     : BSD3
Maintainer  : parsonsmatt@gmail.com
Stability   : experimental
Portability : POSIX

This module is intended to be imported qualified.

This module exports all of the HTML tags for the 'el\'' function.
-}
module Reflex.Tags.SimplePrime where

import qualified Prelude as P

import Reflex.Dom.Widget (el')
import Reflex.Tags.TH

gen'
