{-|
Module      : Reflex.Tags.DynAttr
Description : reflex-dom attribute tags
Copyright   : (c) Layer 3 Communications, 2016
                  Matthew Parsons, 2016
License     : BSD3
Maintainer  : parsonsmatt@gmail.com
Stability   : experimental
Portability : POSIX

This module is intended to be imported qualified. Allows you to use tags with a prefix.

This module exports all of the HTML tags for the 'elDynAttr' function.
-}
module Reflex.Tags.DynAttr where

import Prelude()
import Reflex.Tags.TH
import Reflex.Dom.Widget

genDynAttr ""
