-- | Fast host-backed integer time-series indicators for the reference VM.
-- | These functions intentionally return integer series; callers that need
-- | decimal precision should feed scaled integers, e.g. cents or basis points.
module Verdict.Eval.Series
  ( sma
  , ema
  , wma
  , rollingMedian
  , momentum
  , roc
  , rsi
  , macd
  , macdSignal
  , macdHistogram
  , slope
  , rollingStd
  , realizedVol
  , ewmStd
  , stdevRatio
  , atrApprox
  , bollingerUpper
  , bollingerLower
  , zscore
  , percentileRank
  , drawdown
  , pctChange
  , ratio
  , spread
  , rollingCorr
  , rollingBeta
  , relativeMomentum
  , hedgeRatio
  , seriesAdd
  , seriesSub
  , seriesMul
  , seriesDiv
  , seriesAbs
  , clip
  , shift
  , diff
  , logSeries
  , rollingMax
  , rollingMin
  , cummax
  , cummin
  , crossover
  , crossunder
  , atrOhlc
  , trueRange
  , vwap
  , obv
  , volumeSma
  , volumeRatio
  , bodySize
  , upperWick
  , lowerWick
  , rangePct
  ) where

foreign import sma :: Array String -> String -> Array String
foreign import ema :: Array String -> String -> Array String
foreign import wma :: Array String -> String -> Array String
foreign import rollingMedian :: Array String -> String -> Array String
foreign import momentum :: Array String -> String -> Array String
foreign import roc :: Array String -> String -> Array String
foreign import rsi :: Array String -> String -> Array String
foreign import macd :: Array String -> String -> String -> Array String
foreign import macdSignal :: Array String -> String -> String -> String -> Array String
foreign import macdHistogram :: Array String -> String -> String -> String -> Array String
foreign import slope :: Array String -> String -> Array String
foreign import rollingStd :: Array String -> String -> Array String
foreign import realizedVol :: Array String -> String -> Array String
foreign import ewmStd :: Array String -> String -> Array String
foreign import stdevRatio :: Array String -> String -> String -> Array String
foreign import atrApprox :: Array String -> String -> Array String
foreign import bollingerUpper :: Array String -> String -> String -> Array String
foreign import bollingerLower :: Array String -> String -> String -> Array String
foreign import zscore :: Array String -> String -> Array String
foreign import percentileRank :: Array String -> String -> Array String
foreign import drawdown :: Array String -> Array String
foreign import pctChange :: Array String -> String -> Array String
foreign import ratio :: Array String -> Array String -> Array String
foreign import spread :: Array String -> Array String -> Array String
foreign import rollingCorr :: Array String -> Array String -> String -> Array String
foreign import rollingBeta :: Array String -> Array String -> String -> Array String
foreign import relativeMomentum :: Array String -> Array String -> String -> Array String
foreign import hedgeRatio :: Array String -> Array String -> String -> Array String
foreign import seriesAdd :: Array String -> Array String -> Array String
foreign import seriesSub :: Array String -> Array String -> Array String
foreign import seriesMul :: Array String -> Array String -> Array String
foreign import seriesDiv :: Array String -> Array String -> Array String
foreign import seriesAbs :: Array String -> Array String
foreign import clip :: Array String -> String -> String -> Array String
foreign import shift :: Array String -> String -> Array String
foreign import diff :: Array String -> Array String
foreign import logSeries :: Array String -> Array String
foreign import rollingMax :: Array String -> String -> Array String
foreign import rollingMin :: Array String -> String -> Array String
foreign import cummax :: Array String -> Array String
foreign import cummin :: Array String -> Array String
foreign import crossover :: Array String -> Array String -> Array String
foreign import crossunder :: Array String -> Array String -> Array String
foreign import atrOhlc :: Array String -> Array String -> Array String -> String -> Array String
foreign import trueRange :: Array String -> Array String -> Array String -> Array String
foreign import vwap :: Array String -> Array String -> String -> Array String
foreign import obv :: Array String -> Array String -> Array String
foreign import volumeSma :: Array String -> String -> Array String
foreign import volumeRatio :: Array String -> String -> Array String
foreign import bodySize :: Array String -> Array String -> Array String
foreign import upperWick :: Array String -> Array String -> Array String -> Array String
foreign import lowerWick :: Array String -> Array String -> Array String -> Array String
foreign import rangePct :: Array String -> Array String -> Array String
