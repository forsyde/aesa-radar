module ForSyDe.Atom.MoC.SY.Interface where

import ForSyDe.Atom.MoC.SY.Core as SY
import ForSyDe.Atom.MoC.DE.Core as DE
import ForSyDe.Atom.MoC.TimeStamp

toDE1 :: TimeStamp t => SY.Signal t -> SY.Signal a -> DE.Signal t a
eventToDE (SY.SY t) (SY.SY a) = DE.DE t a
toDE1 ts s1          = eventToDE <$> ts <*> s1
toDE2 ts s1 s2       = (toDE1 ts s1, toDE1 ts s2)
toDE3 ts s1 s2 s3    = (toDE1 ts s1, toDE1 ts s2, toDE1 ts s3)
toDE4 ts s1 s2 s3 s4 = (toDE1 ts s1, toDE1 ts s2, toDE1 ts s3, toDE1 ts s4)
