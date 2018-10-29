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

{-# LANGUAGE OverloadedStrings #-}

module Reflex.Tags.TH where

import Reflex.Dom.Widget
import Control.Monad

import Language.Haskell.TH
import qualified Data.Text as Text

-- | A list of all HTML elements.
elements :: [String]
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
gen :: Name -> String -> DecsQ
gen sym suffix =
    forM elements $ \element -> do
        let name = mkName (element ++ suffix)
        funD name [clause [] (normalB (appE (varE sym) (stringE element))) []]

-- | Generate 'el' functions for all of the elements with an @_@ suffix.
gen_ :: DecsQ
gen_ = gen '(el . Text.pack) "_"

-- | Generate 'el'' functions for all of the elements with an @'@ suffix.
gen' :: DecsQ
gen' = gen '(el'  . Text.pack) "'"

-- | Generate 'elAttr' functions for all of the elements with an @Attr@ suffix.
genAttr :: DecsQ
genAttr = gen '(elAttr . Text.pack) "Attr"

-- | Generate 'elAttr'' functions for all of the elements with an @Attr'@
-- suffix.
genAttr' :: DecsQ
genAttr' = gen '(elAttr'. Text.pack) "Attr'"

-- | Generate 'elDynAttr' functions for all of the elements with a @DynAttr@
-- suffix.
genDynAttr :: DecsQ
genDynAttr = gen '(elDynAttr . Text.pack) "DynAttr"

-- | Generate 'elDynAttr'' functions for all of the elements with a @DynAttr'@
-- suffix.
genDynAttr' :: DecsQ
genDynAttr' = gen '(elDynAttr' . Text.pack) "DynAttr'"

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
