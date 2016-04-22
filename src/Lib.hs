{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module Lib where

import Reflex.Dom.Widget
import Control.Monad

import Language.Haskell.TH

someFunc :: IO ()
someFunc = putStrLn "someFunc"

simpleEls :: [String]
simpleEls =
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

gen :: Name -> String -> DecsQ
gen sym suffix =
    forM simpleEls $ \element -> do
        let name = mkName (element ++ suffix)
        funD name [clause [] (normalB (appE (varE sym) (stringE element))) []]

gen_ :: DecsQ
gen_ = gen 'el "_"

gen' :: DecsQ
gen' = gen 'el' "'"

genAttr :: DecsQ
genAttr = gen 'elAttr "Attr"

genAttr' :: DecsQ
genAttr' = gen 'elAttr' "Attr'"

genDynAttr :: DecsQ
genDynAttr = gen 'elDynAttr "DynAttr"

genDynAttr' :: DecsQ
genDynAttr' = gen 'elDynAttr' "DynAttr'"

genTags :: DecsQ
genTags = do
    a <- gen_
    b <- gen'
    c <- genAttr
    d <- genAttr'
    e <- genDynAttr
    f <- genDynAttr'
    return (mconcat [a, b, c, d, e, f])
