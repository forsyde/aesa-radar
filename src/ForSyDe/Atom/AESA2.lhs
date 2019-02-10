 ## A Process Network-Oriented Approach to Modeling{#sec:atom-network}

The second approach to modeling the radar signal processing
application carries on the idea incepted earlier in
[@sec:int-shallow], namely to use skeletons to express parallelism at
the process level rather than at datum level. This way processes are
expressed as operating on elementary streams of data, i.e. originating
from each antenna element in particular, and skeletons rather describe
patterns of interaction and synchronization between these
processes. We thus trade the monolithic view of processes computing
"cubes" of data for a much finer-grained view which allows to quantize
the potential for parallelism by exploiting 1) the arithmetic/data
dependencies; 2) the precedence relations.

The ForSyDe-Atom definition file for the second approach to modeling
the Saab-AESA application is found at
`<root>/src/ForSyDe/Atom/AESA2.lhs` and it can be imported as a
generic library (e.g. in the interpreter session). Below you find the
module definition and exported functions.

> {-# LANGUAGE PackageImports #-}  -- you can ignore this line
> module ForSyDe.Atom.AESA2 where

By now the imported libraries are not a mistery any longer. Check
[@sec:atom-operation] for more details on what each is imported for.

> import Data.Complex
> import "forsyde-atom-extensions" ForSyDe.Atom.MoC.SDF as SDF
> import ForSyDe.Atom.MoC.SY  as SY
>
> import "forsyde-atom-extensions" ForSyDe.Atom.Skeleton.Vector as V
> import "forsyde-atom-extensions" ForSyDe.Atom.Skeleton.Vector.Cube   as C
> import "forsyde-atom-extensions" ForSyDe.Atom.Skeleton.Vector.Matrix as M
> import "forsyde-atom-extensions" ForSyDe.Atom.Skeleton.Vector.DSP

As in the previous section, we import these local submodule containing
the type synonym declarations presented in [@sec:aliases-shallow] and
the coefficient windows like in [@sec:coefs-shallow].

> import ForSyDe.Atom.AESA.Types
> import ForSyDe.Atom.AESA.Coefs

 ### Video Processing Pipeline Stages

By now the functional specification of each stage in the video
processing pipe should be quite clear clear. The modeling approaches
used until now tried to _translate_ this functional specification into
an _executable_ ForSyDe specification. We now stretch the intuition
gained with these models and exploit the initial assumption stated as
early as [@sec:video-chain-spec]: _"For each antenna the data arrives
_pulse by pulse_, and each pulse arrives _range bin by range
bin_. This happens _for all antennas in parallel_, and all complex
samples are synchronized with the same sampling rate, e.g. of the A/D
converter."_

This allows us to "unroll" the video cubes into parallel (synchronous)
streams, each stream being able to be processed as soon as it contains
enough data. This unrolling can be simply depicted in
[@fig:cube-unrolling] as chaining the pulses as they arive: range bin
by range bin. We say that we partition the data _in time_ rather than
_in space_, which is a more appropriate partition judging by the
speciffication's assumptions.

![Video cube unrolling](figs/cube-unrolling.pdf){#fig:cube-unrolling}

 #### Digital Beamforming (DBF)

Recall that the DBF $N_B$ simultaneous receiver beams from the complex
data received from $N_A$ antenna elements, as explained in
[@sec:dbf-shallow]. Depicted from a streaming point of view, DBF looks
like in @fig:dbf-samp.

![Digital Beam Forming on streams of complex samples](figs/dbf-samp.pdf){#fig:dbf-samp}

![DBF network](figs/dbf-net-atom.pdf){#fig:dbf-net-atom}


> dbf :: Antenna (SY.Signal CpxData)
>     -> Beam    (SY.Signal CpxData)
> dbf antennaSigs = beamSigs
>   where
>     beamSigs   = V.farm11 (V.reduce (SY.comb21 (+))) beamMatrix
>     beamMatrix = M.farm21 (\c -> SY.comb11 (*c)) beamConsts sigMatrix
>     sigMatrix  = V.farm11 V.fanout antennaSigs
>     beamConsts = mkBeamConsts (V.length antennaSigs) nB


 #### Pulse Compression (PC)

![Pulse Compression on streams of complex samples](figs/pc-samp.pdf){#fig:pc-samp}

![PC network](figs/pc-net-atom.pdf){#fig:pc-net-atom}

![`countDelay` process structure](figs/pc-counter-atom.pdf){#fig:pc-counter-atom}


> pc :: Beam (SY.Signal CpxData)
>    -> Beam (SY.Signal CpxData)
> pc = V.farm11 (fir' addProc mulProc delayCount mkPcCoefs)
>   where
>     addProc    = SY.comb21 (+)
>     mulProc c  = SY.comb11 (*c)
>     delayCount = fst . SY.stated12 countBin (0, nb)
>     countBin _ 0 _ = (0, nb)
>     countBin _ c s = (s, c-1)


 #### Corner Turn (CT)

![Building matrices of complex samples during CT](figs/ct-samp.pdf){#fig:ct-samp}

![CT network](figs/ct-net-atom.pdf){#fig:ct-net-atom}

> ct :: Beam (SY.Signal CpxData)
>    -> (Beam (SDF.Signal CpxData),
>        Beam (SDF.Signal CpxData))
> ct sigs = (V.farm11 rightCorner sigs, V.farm11 leftCorner sigs)
>   where
>     rightCorner = cornerTurn . SY.toSDF
>     leftCorner  = SDF.delay initBatch . rightCorner
>     initBatch   = replicate (nb * nFFT `div` 2) (cis 0)
>     cornerTurn  = SDF.comb11 (nFFT * nb, nb * nFFT,
>                               fromMatrix . M.transpose . matrix nb nFFT)

 #### Doppler Filter Bank (DFB)

![Doppler Filter Bank on streams of complex samples](figs/dfb-samp.pdf){#fig:dfb-samp}

![DFB network](figs/dfb-net-atom.pdf){#fig:dfb-net-atom}

> dfb :: Beam (SDF.Signal CpxData)
>     -> Beam (SDF.Signal RealData)
> dfb = V.farm11
>       (SDF.comb11 (nFFT, nFFT,
>                    fromVector . fDFB . vector))
>   where
>     fDFB       = V.farm11 envelope . fft nFFT . weight 
>     weight     = V.farm21 (*) mkWeightCoefs
>     envelope a = let i = realPart a
>                      q = imagPart a
>                  in sqrt (i * i + q * q)

 #### Constant False Alarm Ratio (CFAR)

![Constant False Alarm Ratio on cubes of complex samples](figs/cfar-cube.pdf){#fig:cfar-cube-atom}

![CFAR network](figs/cfar-net-atom.pdf){#fig:cfar-net-atom}

> cfar :: Beam (SDF.Signal RealData)
>      -> Beam (SDF.Signal RealData)
> cfar = V.farm11 (pCFAR . assign)

        
> assign :: SDF.Signal RealData
>        -> Vector (SDF.Signal (Matrix RealData)
>                  ,SDF.Signal (Vector RealData)
>                  ,SDF.Signal (Matrix RealData))
> assign = V.farm11 get . distrib . neighbors
>   where
>     neighbors = SDF.comb11 (nb * nFFT, nb',
>                             fromVector . V.stencil (2*nFFT+3) . matrix nFFT nb)
>     distrib   = SDF.distribute (V.fanoutn nb' 1)
>     get smx   = (early smx, mid smx, late smx)
>       where
>         early = SDF.comb11 (1,1,\[a] -> [V.take nFFT a])
>         mid   = SDF.comb11 (1,1,\[a] -> [V.first $ V.drop (nFFT + 1) a])
>         late  = SDF.comb11 (1,1,\[a] -> [V.drop (nFFT + 3) a])

> pCFAR :: Vector (SDF.Signal (Matrix RealData)
>                 ,SDF.Signal (Vector RealData)
>                 ,SDF.Signal (Matrix RealData))
>       -> SDF.Signal RealData
> pCFAR = SDF.merge (V.fanoutn nb' nFFT) . V.farm11 calc
>   where
>     calc (e,m,l) = normP (minVP m) (meanP e) (meanP l) m
>     ------------------------------------------------------
>     normP = SDF.comb41
>             ((1,1,1,1), nFFT,
>              \[m] [e] [l] [a] -> fromVector $ V.farm31 (norm m) e l a)
>     minVP = SDF.comb11 (1,1,\[a] -> [minVal a])
>     meanP = SDF.comb11 (1,1,\[a] -> [arithMean a])
>     ------------------------------------------------------
>     norm m e l a = 2 ** (5 + logBase 2 a - maximum [l,e,m])
>     minVal    = logBase 2 . V.reduce min
>     arithMean = V.farm11 (/n) . V.reduce addV . V.farm11 geomMean . V.group 4
>     geomMean  = V.farm11 (logBase 2 . (/4)) . V.reduce addV
>     addV      = V.farm21 (+)
>     n         = fromIntegral nFFT


 #### Integrator (INT)

![Integration on cubes of complex samples](figs/int-cube.pdf){#fig:int-cube-atom}

![INT network](figs/int-net-atom.pdf){#fig:int-net-atom}

> int :: Beam (SDF.Signal RealData)
>     -> Beam (SDF.Signal RealData)
>     -> Beam (CRange (Window (SY.Signal RealData)))
> int cr cl = C.farm11 firChain $ C.farm21 addSY (unroll cr) (unroll cl)
>   where
>     unroll   = C.farm11 SDF.toSY .
>                M.farm11 (SDF.distribute (V.fanoutn nFFT 1)) .
>                V.farm11 (SDF.distribute (V.fanoutn nb' nFFT))
>     firChain = fir' addSY mulSY (SY.delay 0) mkFirCoefs
>     addSY    = SY.comb21 (+)
>     mulSY c  = SY.comb11 (*c)

 ### System Process Network

> aesa :: Antenna (SY.Signal CpxData)
>      -> Beam (CRange (Window (SY.Signal RealData)))
> aesa video = int lDfb rDfb
>   where
>     lDfb      = cfar $ dfb lCt
>     rDfb      = cfar $ dfb rCt
>     (lCt,rCt) = ct $ pc $ dbf video

