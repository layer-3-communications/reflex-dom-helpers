{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module Lib where

import Reflex.Dom.Widget
import Control.Monad

import Language.Haskell.TH

someFunc :: IO ()
someFunc = putStrLn "someFunc"

simpleEls :: [Name]
simpleEls = map mkName ["p", "div", "ul", "li"]

genDyn :: DecsQ
genDyn = do
    fns <- forM simpleEls $ \name -> do
        let s = show name
        funD name [clause [] (normalB (appE (varE 'el) (stringE s))) []]
    return fns
