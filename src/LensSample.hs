-- http://hackage.haskell.org/package/lens-tutorial-1.0.3/docs/Control-Lens-Tutorial.html
-- https://hackage.haskell.org/package/microlens-0.4.7.0/docs/Lens-Micro.html
-- https://hackage.haskell.org/package/microlens-th-0.4.1.0/docs/Lens-Micro-TH.html

-- build-depends: microlens, microlens-th

{-# LANGUAGE TemplateHaskell #-}

module LensSample ( 
    runSampleView,
    runSampleOver,
    runSampleSet
) where

import Lens.Micro
import Lens.Micro.TH
import Lens.Micro.Extras

data Point = Point { _x :: Double, _y :: Double } deriving(Show)
data Atom = Atom { _element :: String, _point :: Point } deriving(Show)

makeLenses ''Atom
makeLenses ''Point

runSampleView :: Double
runSampleView = 
    let atom = Atom { _element = "C", _point = Point { _x = 1.0, _y = 2.0 } }
    in view (point . x) atom

runSampleOver :: Atom
runSampleOver = 
    let atom = Atom { _element = "C", _point = Point { _x = 1.0, _y = 2.0 } }
    in over (point . x) (+1) atom

runSampleSet :: Atom
runSampleSet = 
    let atom = Atom { _element = "C", _point = Point { _x = 1.0, _y = 2.0 } }
    in set (point . x) 9 atom