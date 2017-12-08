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
-- lamSig ~ Exponential(1/100)
-- lamBgd ~ Exponential(1/1000)
-- nSig   ~ Poisson(lamBgd + lamSig)
-- nBgd   ~ Poisson(lamBgd)
--
--
-- nSig = 20
-- nBgd = 4
----------------------------------------------------------------

prog :: Program
prog = Program
  { progData  = Map.empty
  , progModel = model
      [ "lamSig" .: Real .~ Exponential 1e-3
      , "lamBgd" .: Real .~ Exponential 1e-2
      , "nSig"   .: Int  .~ Poisson ("lamSig" + "lamBgd")
      , "nBgd"   .: Int  .~ Poisson ("lamBgd")
      ]
  , progObserved = obs
      [ "nSig" .== ValI 20
      , "nBgd" .== ValI 4
      ]
  }


----------------------------------------------------------------

main :: IO ()
main = do
  let x0      = Map.fromList [ ("lamSig", ValD 100)
                             , ("lamBgd", ValD 40)
                             ]
      Right m = makeMH prog
  --
  seed <- MWC.save =<< MWC.create
  let chain     = runMH m x0 seed 20000
      chainLamS = map (\x -> let ValD y = x Map.! "lamSig" in y) chain
      chainLamB = map (\x -> let ValD y = x Map.! "lamBgd" in y) chain
  --
  let lamHB = forceInt -<< mkSimple (binD 0 200 40)
      lamSH = fillBuilder lamHB $ drop 500 chainLamS
      lamBH = fillBuilder lamHB $ drop 500 chainLamB
      --
      lamHB2D = forceInt -<< mkSimple (let b = binD 0 100 40 in Bin2D b b)
      lam2D = fillBuilder lamHB2D $ drop 500 $ zip chainLamS chainLamB
  --
  HEP.draws_ $ do
    HEP.addColumn $ do
      HEP.addPad $ do
        HEP.add $ HEP.Graph chainLamS
        HEP.set $ HEP.lineColor HEP.BLUE
        HEP.add $ HEP.Graph chainLamB
        HEP.set $ HEP.lineColor HEP.RED
      HEP.addPad $ do
        HEP.addRow $ do
          HEP.addPad $ do
            HEP.add $ HEP.hist lamSH
            HEP.set $ do HEP.fillStyle 3007
                         HEP.fillColor HEP.BLUE
            --
            HEP.add $ HEP.hist lamBH
            HEP.set $ do HEP.fillStyle 3002
                         HEP.fillColor HEP.RED
            HEP.add $ HEP.VLine 4
            HEP.add $ HEP.VLine 16
          HEP.addPad $ do
            HEP.add $ HEP.hist lam2D
            HEP.set $ HEP.histOpt $ HEP.histColor HEP.ON
            return ()
  --
  return ()
