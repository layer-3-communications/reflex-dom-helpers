{-|
Module      : Reflex.Tags
Description : reflex-dom tags with function suffixes
Copyright   : (c) Layer 3 Communications, 2016
                  Matthew Parsons, 2016
License     : BSD3
Maintainer  : parsonsmatt@gmail.com
Stability   : experimental
Portability : POSIX

This module exports all of the HTML tags in the various permutations generally
used by reflex-dom with a suffix. The suffix prevents name collisions with
Prelude functions. Following the example of "Lucid", ordinary elements have an
underscore character appended to them. The other elements use the suffix that
you'd normally add to 'el' to make.

Here are a list of equivalences:

* @'el' "p"@ to 'p_'
* @'el'' "p"@ to 'p''
* @'elAttr' "p"@ to 'pAttr'
* @'elAttr'' "p"@ to 'pAttr''
* @'elDynAttr'@ "p"@ to 'pDynAttr'
* @'elDynAttr@' "p"' to 'pDynAttr''
-}

module Reflex.Tags where

import Reflex.Tags.TH

-- | generate all the tags with a suffix
genTagsSuffixed
