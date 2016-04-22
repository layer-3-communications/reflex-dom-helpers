{-|
Module      : Reflex.Tags.Attr
Description : reflex-dom attribute tags
Copyright   : (c) Layer 3 Communications, 2016
                  Matthew Parsons, 2016
License     : BSD3
Maintainer  : parsonsmatt@gmail.com
Stability   : experimental
Portability : POSIX

This module is intended to be imported qualified.

This module exports all of the HTML tags for the 'elAttr' function.
-}
module Reflex.Tags.Attr where

import Prelude()
import Reflex.Tags.TH
import Reflex.Dom.Widget

$(gen 'elAttr "")
