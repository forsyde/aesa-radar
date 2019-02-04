 ## A Process Network-Oriented Approach to Modeling{#sec:atom-network}

> {-# LANGUAGE PackageImports #-}
> module ForSyDe.Atom.AESA2 where

> import Data.Complex
> import ForSyDe.Atom.MoC.SDF as SDF
> import ForSyDe.Atom.MoC.SY  as SY

> import "forsyde-atom-extensions" ForSyDe.Atom.Skeleton.Vector as V
> import ForSyDe.Atom.Skeleton.Vector.Cube   as C
> import ForSyDe.Atom.Skeleton.Vector.Matrix as M
> import ForSyDe.Atom.Skeleton.Vector.DSP

> import ForSyDe.Atom.AESA.Types
> import ForSyDe.Atom.AESA.Coefs

> nb   = 256 :: Int
> nb'  = nb - 2 * nFFT - 2

 #### Digital Beamforming (DBF)

The DBF receives complex indata, from $N_A$ antenna elements and forms
 $N_B$ simultaneous receiver beams, or "listening directions", by
 summing individually phase-shifted indata signals from all
 elements. Basically, considering the input video "cube" described
 [previously](), the the transformation applied by DBF, could be
 depicted as in [@fig:dbf-cube], where the _pulse_ dimension goes to
 infinity (i.e. data is received pulse by pulse).

![Digital Beam Forming on video structure](figs/dbf-cube.pdf)

However, using knowledge of _how_ data arrives, the _range bin_
dimension can also be unrolled in time, and thus the DBF algorithm can
be applied as soon as $N_A$ complex samples arrive.

![Digital Beam Forming on streams of complex samples](figs/dbf-samp.pdf){#fig:dbf-samp}

To translate [@fig:dbf-samp] into MoC behavior, instead of modeling a
vector of $N_A$ synchronous signals, i.e. a signal for each antenna
element, we choose to represent it as one signal of $N_A$ samples, as
for the SY MoC the two representations are semantically
equivalent. However, with this approach we focus on the parallel
procesing of data aspect of the computation rather than the concurrent
distribution of signals, which is more suitable for this application.


> dbf :: Antenna (SY.Signal CpxData)
>     -> Beam    (SY.Signal CpxData)
> dbf antennaSigs = beamSigs
>   where
>     beamSigs   = V.reduce (V.farm21 (SY.comb21 (+))) beamMatrix
>     beamMatrix = M.farm21 (\c -> SY.comb11 (*c)) beamConsts sigMatrix
>     sigMatrix  = V.farm11 V.fanout antennaSigs
>     beamConsts = mkBeamConsts (V.length antennaSigs) nB


 #### Pulse Compression (PC)

> pc :: Beam (SY.Signal CpxData)
>    -> Beam (SY.Signal CpxData)
> pc = V.farm11 (fir' addProc mulProc (fst . delayCounter) pcCoefs)
>   where
>     addProc = SY.comb21 (+)
>     mulProc = SY.comb21 (*)
>     delayCounter = SY.stated12 countBeamLgth (0, nb)
>     countBeamLgth _ 0 _ = (0, nb)
>     countBeamLgth _ c s = (s, c-1)
>     pcCoefs = V.farm11 SY.constant1 mkPcCoefs


 #### Corner Turn (CT)

> ct :: Beam (SY.Signal CpxData)
>    -> (Beam (SDF.Signal CpxData),
>        Beam (SDF.Signal CpxData))
> ct pcSigs = (V.farm11 rightCorner pcSigs, V.farm11 leftCorner pcSigs)
>   where
>     rightCorner = SY.toSDF
>     leftCorner  = SDF.delay initBatch . rightCorner
>     initBatch = replicate (nFFT `div` 2) (cis 0)

 #### Doppler Filter Bank (DFB)

> doppler :: Beam (SDF.Signal CpxData)
>         -> Beam (SDF.Signal RealData)
> doppler = V.farm11
>           (SDF.comb11 (nFFT * nb, nFFT * nb,
>                        fromMatrix . transform . matrix nb nFFT))
>   where
>     transform  = V.farm11 (fDoppler . fWeight) . M.transpose 
>     fWeight    = V.farm21 (*) mkWeightCoefs
>     fDoppler   = V.farm11 envelope . fft nFFT
>     envelope a = let i = realPart a
>                      q = imagPart a
>                  in sqrt (i * i + q * q)

 #### Constant False Alarm Ratio (CFAR)

> cfar :: Beam (SDF.Signal RealData)
>      -> Beam (SDF.Signal RealData)
> cfar = V.farm11
>        (SDF.comb11 (nFFT * nb, nFFT * nb',
>                     fromMatrix . fCFAR . matrix nFFT nb))

> fCFAR :: Range (Window RealData) -> CRange (Window RealData)
> fCFAR r_of_d = V.farm41 (\m -> V.farm31 (normCfa m)) md bin lmv emv
>   where
>     bin = V.drop (nFFT + 1) r_of_d
>     md  = V.farm11 (logBase 2 . V.reduce min) bin
>     emv = V.farm11 (meanFun . V.take nFFT) stens
>     lmv = V.farm11 (meanFun . V.drop (nFFT + 3)) stens
>     -----------------------------------------------
>     normCfa m a l e = 2 ** (5 + logBase 2 a - maximum [l,e,m])
>     meanFun :: Vector (Vector RealData) -> Vector RealData
>     meanFun = V.reduce addV . V.farm11 (V.farm11 (logBase 2 . (/4)) . V.reduce addV) . V.group 4
>     stens   = V.stencil (2 * nFFT + 3) r_of_d
>     addV    = V.farm21 (+)
>   

 #### Integrator (INT)

> int :: Beam (SDF.Signal RealData)
>     -> Beam (SDF.Signal RealData)
>     -> Beam (CRange (SY.Signal RealData))
> int cr cl = M.farm11 firChain $ M.farm21 addSY (unroll cr) (unroll cl)
>   where
>     unroll   = M.farm11 SDF.toSY . V.farm11 streamify
>     streamify= SDF.unzipx (V.fanoutn nb' nFFT) .
>                SDF.comb11 (nFFT * nb', 1, (:[]) . vector)
>     firChain = fir' addSY mulSY (SY.delay 0) mkFirCoefs
>     addSY    = SY.comb21 (+)
>     mulSY c  = SY.comb11 (*c)

 ### System Process Network

> aesa :: Antenna (SY.Signal CpxData)
>      -> Beam (CRange (SY.Signal RealData))
> aesa video = int lDfb rDfb
>   where
>     lDfb      = cfar $ doppler lCt
>     rDfb      = cfar $ doppler rCt
>     (lCt,rCt) = ct $ pc $ dbf video

