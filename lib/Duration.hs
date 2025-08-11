{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Duration
  ( Duration (..)
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

instance Show Duration where
  show d =
    case (toHMS d) of
      HMS h m s ->
        showInt h ++ ":" ++ showInt m ++ ":" ++ showSci s
    where
      showInt :: Int -> String
      showInt n =
        let str = show n
        in case str of
          [_digit] -> "0" ++ str
          digits -> digits

      showSci :: Scientific -> String
      showSci n =
        let str = show n
        in if (abs n) < 10.0
           then case str of
                  '-' : rest ->
                    '-' : '0' : rest
                  _ ->
                    '0' : str
           else str

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
