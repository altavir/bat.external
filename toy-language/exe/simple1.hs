{-# LANGUAGE OverloadedStrings #-}
import qualified Data.Map as Map
import           Data.Histogram.Fill
import qualified System.Random.MWC as MWC
import qualified HEP.ROOT.Plot as HEP

import BAT.AST
import BAT.Interpreter.MH

----------------------------------------------------------------
-- Model description
--------------------
--
-- lam ~ Exponential(1)
-- n   ~ Poisson(lam)
--
-- n = 10
----------------------------------------------------------------

prog :: Program
prog = Program
  { progData  = Map.empty
  , progModel = model
      [ "lam" .: Real .~ Exponential 0.02
      , "n"   .: Int  .~ Poisson "lam"
      ]
  , progObserved = obs
      [ "n" .== ValI 8
      ]
  }


----------------------------------------------------------------

main :: IO ()
main = do
  let x0      = Map.fromList [("lam", ValD 100)]
      Right m = makeMH prog
  --
  seed <- MWC.save =<< MWC.create
  let chain    = runMH m x0 seed 20000
      chainLam = map (\x -> let ValD y = x Map.! "lam" in y) chain
  --
  let lamHB = forceInt -<< mkSimple (binD 0 200 20)
      lamH  = fillBuilder lamHB $ drop 500 chainLam
  --
  HEP.draws_ $ do
    HEP.addColumn $ do
      HEP.addPad $ do
        HEP.add $ HEP.Graph chainLam
        HEP.add $ HEP.HLine 8
        HEP.set $ HEP.lineColor HEP.RED
      HEP.addPad $ do
        HEP.add $ HEP.hist lamH
