-- http://hackage.haskell.org/package/lens-tutorial-1.0.3/docs/Control-Lens-Tutorial.html
-- https://hackage.haskell.org/package/microlens-0.4.7.0/docs/Lens-Micro.html
-- https://hackage.haskell.org/package/microlens-th-0.4.1.0/docs/Lens-Micro-TH.html

-- https://github.com/ekmett/lens/wiki/Operators

-- build-depends: microlens, microlens-th

{-# LANGUAGE TemplateHaskell #-}

module LensSample where

import Lens.Micro
import Lens.Micro.TH
import Lens.Micro.Extras

data Point = Point { _x :: Double, _y :: Double } deriving(Show)
data Atom = Atom { _element :: String, _point :: Point } deriving(Show)
data Molecule = Molecule { _atoms :: [Atom] } deriving (Show)

makeLenses ''Atom
makeLenses ''Point
makeLenses ''Molecule

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

runSampleAccessor :: Double
runSampleAccessor = 
    let atom = Atom { _element = "C", _point = Point { _x = 1.0, _y = 2.0 } }
    in atom^.point.y

runSampleTraverseOver  :: Molecule
runSampleTraverseOver = 
    let atom1 = Atom { _element = "C", _point = Point { _x = 1.0, _y = 2.0 } }
        atom2 = Atom { _element = "O", _point = Point { _x = 3.0, _y = 4.0 } }
        molecule = Molecule { _atoms = [atom1, atom2] }
    in over (atoms . traverse . point . x) (+ 1) molecule

runSampleTraverseSet  :: Molecule
runSampleTraverseSet = 
    let atom1 = Atom { _element = "C", _point = Point { _x = 1.0, _y = 2.0 } }
        atom2 = Atom { _element = "O", _point = Point { _x = 3.0, _y = 4.0 } }
        molecule = Molecule { _atoms = [atom1, atom2] }
    in set (atoms . traverse . point . x) 9 molecule 
        
runSampleTraverseToListOf1  :: [Atom]
runSampleTraverseToListOf1 = 
    let atom1 = Atom { _element = "C", _point = Point { _x = 1.0, _y = 2.0 } }
        atom2 = Atom { _element = "O", _point = Point { _x = 3.0, _y = 4.0 } }
        molecule = Molecule { _atoms = [atom1, atom2] }
    in toListOf (atoms . traverse ) molecule      

runSampleTraverseToListOf2  :: [Atom]
runSampleTraverseToListOf2 = 
    let atom1 = Atom { _element = "C", _point = Point { _x = 1.0, _y = 2.0 } }
        atom2 = Atom { _element = "O", _point = Point { _x = 3.0, _y = 4.0 } }
        molecule = Molecule { _atoms = [atom1, atom2] }
    in molecule ^.. atoms . traverse        