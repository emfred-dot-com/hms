{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Duration
  ( Duration (..)
  , DisplayMode (..)
  , showDuration
  , durAdd
  , durSubtract
  , durMultiply
  )
where

import Data.Scientific

newtype Duration = Duration
  { seconds :: Scientific }
  deriving (Fractional, RealFrac, Real,
            Num, Eq, Ord, Read)

data HMS = HMS
  { _hours   :: Int
  , _minutes :: Int
  , _seconds :: Scientific }

data DisplayMode =
  Decimal | NoDecimal

showDuration :: DisplayMode -> Duration -> String
showDuration mode d =
    case (toHMS $ abs d) of
      HMS h m s ->
        if d < 0.0
        then '-' : showInt h ++ ":" ++ showInt m ++ ":" ++ showSci s
        else showInt h ++ ":" ++ showInt m ++ ":" ++ showSci s
    where
      showInt :: Int -> String
      showInt n =
        let str = show n
        in case str of
          [_digit] -> "0" ++ str
          digits -> digits

      showSci :: Scientific -> String
      showSci n =
        case mode of
          Decimal ->
            let str = show n
            in if n < 10.0
               then '0' : str
               else str
          NoDecimal ->
            let dbl = toRealFloat n :: Double
                int = round dbl :: Int
            in showInt int

instance Show Duration where
  show = showDuration Decimal

toHMS :: Duration -> HMS
toHMS (Duration secs) =
  hmsNormalize $ HMS 0 0 secs

hmsNormalize :: HMS -> HMS
hmsNormalize d = carryMins $ carrySecs d

carrySecs :: HMS -> HMS
carrySecs d =
  case d of
    HMS h m s ->
      if s >= 60.0
      then carrySecs $ HMS h (m + 1) (s - 60.0)
      else d

carryMins :: HMS -> HMS
carryMins d =
  case d of
    HMS h m s ->
      if m >= 60
      then carryMins $ HMS (h + 1) (m - 60) s
      else d

durAdd :: Duration -> Duration -> Duration
durAdd = (+)

durSubtract :: Duration -> Duration -> Duration
durSubtract = (-)

durMultiply :: Duration -> Duration -> Duration
durMultiply = (*)
