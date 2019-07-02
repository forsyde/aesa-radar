{-# LANGUAGE PackageImports #-}
module ForSyDe.Atom.MoC.BDF (
  switch, select
  ) where

import "forsyde-atom" ForSyDe.Atom.MoC.SDF as SDF
import "forsyde-atom" ForSyDe.Atom.MoC.Stream
import "forsyde-atom" ForSyDe.Atom.MoC.SDF.SADF

switch :: Signal Bool -> Signal a -> (Signal a, Signal a)
switch sel = kernel12 selSig
  where selSig = detector11 1 (\_ [a] -> a) selFun True sel
        selFun True  = (1,[(1,(1,0), \a -> (a,[]))])
        selFun False = (1,[(1,(0,1), \a -> ([],a))])


select :: Signal Bool -> Signal a -> Signal a -> Signal a
select sel = kernel21 selSig
  where selSig = detector11 1 (\_ [a] -> a) selFun True sel
        selFun True  = (1,[((1,0),1,\a b-> a)])
        selFun False = (1,[((0,1),1,\a b-> b)])

