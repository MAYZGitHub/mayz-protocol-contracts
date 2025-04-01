module Main where

import qualified Prelude            as P
import qualified Protocol.Deploy    as Deploy

--Modulo:

main :: P.IO ()
main = do
  _ <- Deploy.deploy_Protocol_And_Fund_Factory2
  P.return ()
