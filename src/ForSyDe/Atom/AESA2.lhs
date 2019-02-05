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
> import ForSyDe.Atom.MoC.SDF as SDF
> import ForSyDe.Atom.MoC.SY  as SY
>
> import "forsyde-atom-extensions" ForSyDe.Atom.Skeleton.Vector as V
> import ForSyDe.Atom.Skeleton.Vector.Cube   as C
> import ForSyDe.Atom.Skeleton.Vector.Matrix as M
> import ForSyDe.Atom.Skeleton.Vector.DSP

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

> dbf :: Antenna (SY.Signal CpxData)
>     -> Beam    (SY.Signal CpxData)
> dbf antennaSigs = beamSigs
>   where
>     beamSigs   = V.farm11 (V.reduce (SY.comb21 (+))) beamMatrix
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

