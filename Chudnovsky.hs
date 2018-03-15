--Chudnovsky.hs
--Copyright Laurence Emms 2018
--Executable for computing an infinite Chudnovsky series in Haskell

import System.IO
import Data.Number.CReal

fact :: CReal -> CReal
fact 0.0 = 1.0
fact a = a * fact (a - 1.0)

chudnovsky :: CReal -> CReal -> CReal
chudnovsky r k = r + numerator / denominator
    where numerator = (fact (6.0 * k)) * (545140134.0 * k + 13591409.0)
          denominator = (fact (3.0 * k)) * ((fact k) ** 3.0) * (-262537412640768000.0 ** k)

printChudnovsky :: CReal -> CReal -> IO ()
printChudnovsky r k = (putStrLn (showCReal 1000 (-(426880.0 * (sqrt 10005.0) / ch)))) >> hFlush stdout >> (printChudnovsky ch (k + 1.0))
    where ch = chudnovsky r k

main = printChudnovsky 0.0 0.0
