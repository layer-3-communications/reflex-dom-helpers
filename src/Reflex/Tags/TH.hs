{-|
Module      : Reflex.Tags.TH
Description : Template Haskell utilities
Copyright   : (c) Layer 3 Communications, 2016
                  Matthew Parsons, 2016
License     : BSD3
Maintainer  : parsonsmatt@gmail.com
Stability   : experimental
Portability : POSIX

This module provide utilities for generating convenience functions for HTML
elements.
-}

module Reflex.Tags.TH where

import Reflex.Dom.Widget
import Control.Monad
import qualified Data.Text as T

import Language.Haskell.TH

-- | A list of all HTML elements.
elements :: [T.Text]
elements =
  [ "a"
  , "abbr"
  , "acronym"
  , "address"
  , "applet"
  , "area"
  , "article"
  , "aside"
  , "audio"
  , "b"
  , "base"
  , "basefont"
  , "bdi"
  , "bdo"
  , "big"
  , "blockquote"
  , "body"
  , "br"
  , "button"
  , "canvas"
  , "caption"
  , "center"
  , "cite"
  , "code"
  , "col"
  , "colgroup"
  , "datalist"
  , "dd"
  , "del"
  , "details"
  , "dfn"
  , "dialog"
  , "dir"
  , "div"
  , "dl"
  , "dt"
  , "em"
  , "embed"
  , "fieldset"
  , "figcaption"
  , "figure"
  , "font"
  , "footer"
  , "form"
  , "frame"
  , "frameset"
  , "h1"
  , "h2"
  , "h3"
  , "h4"
  , "h5"
  , "h6"
  , "head"
  , "header"
  , "hr"
  , "html"
  , "i"
  , "iframe"
  , "img"
  , "input"
  , "ins"
  , "kbd"
  , "keygen"
  , "label"
  , "legend"
  , "li"
  , "link"
  , "main"
  , "map"
  , "mark"
  , "menu"
  , "menuitem"
  , "meta"
  , "meter"
  , "nav"
  , "noframes"
  , "noscript"
  , "object"
  , "ol"
  , "optgroup"
  , "option"
  , "output"
  , "p"
  , "param"
  , "pre"
  , "progress"
  , "q"
  , "rp"
  , "rt"
  , "ruby"
  , "s"
  , "samp"
  , "script"
  , "section"
  , "select"
  , "small"
  , "source"
  , "span"
  , "strike"
  , "strong"
  , "style"
  , "sub"
  , "summary"
  , "sup"
  , "table"
  , "tbody"
  , "td"
  , "textarea"
  , "tfoot"
  , "th"
  , "thead"
  , "time"
  , "title"
  , "tr"
  , "track"
  , "tt"
  , "u"
  , "ul"
  , "var"
  , "video"
  , "wbr"
  ]

-- | Given a name for a function and a suffix, this function will generate
-- a list of declarations. Each declaration will consist of the function applied
-- to each of the HTML elements with the given suffix.
gen :: Name -> T.Text -> DecsQ
gen sym suffix =
    forM elements $ \element -> do
        let name = mkName (T.unpack element ++ T.unpack suffix)
        funD name [clause [] (normalB (appE (varE sym) (stringE (T.unpack element)))) []]

-- | Generate 'el' functions for all of the elements with an @_@ suffix.
gen_ :: DecsQ
gen_ = gen 'el "_"

-- | Generate 'el'' functions for all of the elements with an @'@ suffix.
gen' :: DecsQ
gen' = gen 'el' "'"

-- | Generate 'elAttr' functions for all of the elements with an @Attr@ suffix.
genAttr :: DecsQ
genAttr = gen 'elAttr "Attr"

-- | Generate 'elAttr'' functions for all of the elements with an @Attr'@
-- suffix.
genAttr' :: DecsQ
genAttr' = gen 'elAttr' "Attr'"

-- | Generate 'elDynAttr' functions for all of the elements with a @DynAttr@
-- suffix.
genDynAttr :: DecsQ
genDynAttr = gen 'elDynAttr "DynAttr"

-- | Generate 'elDynAttr'' functions for all of the elements with a @DynAttr'@
-- suffix.
genDynAttr' :: DecsQ
genDynAttr' = gen 'elDynAttr' "DynAttr'"

-- | Generate all of the tags with all of the suffixes.
genTags :: DecsQ
genTags = do
    a <- gen_
    b <- gen'
    c <- genAttr
    d <- genAttr'
    e <- genDynAttr
    f <- genDynAttr'
    return (mconcat [a, b, c, d, e, f])
