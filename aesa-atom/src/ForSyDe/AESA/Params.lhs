 ### System Parameters {#sec:aliases-shallow label="Type Aliases and Constants"}

Here we define the size constants, for a simple test scenario provided by Saab AB. The
size `nA` can be inferred from the size of input data and the vector operations.

> module ForSyDe.AESA.Params where
> 
> nA   =   16 :: Int -- does not really affect the application
> nB   =    8 :: Int
> nb   = 1024 :: Int
> nFFT =  256 :: Int
> nS   =    8 :: Int -- 2^nS = nFFT; used for convenience
> 
> freqRadar  = 10e9 :: Float -- 10 Ghz X-band
> waveLength = 3e8 / freqRadar
> dElements  = waveLength/2
