module Duration
  ( Duration (..)
  , normalize
  , durAdd
  , durSubtract
  , durMultiply
  )
where

data Duration =
  HMS
  { h :: Int
  , m :: Int
  , s :: Int
  }
  deriving (Eq)

instance Show Duration where
  show d =
    case d of
      HMS h m s ->
        showDig h ++ ":" ++ showDig m ++ ":" ++ showDig s
    where
      showDig :: Int -> String
      showDig n =
        let str = show n
        in case str of
          _digit : [] -> "0" ++ str
          digits -> digits

carrySecs :: Duration -> Duration
carrySecs d =
  case d of
    HMS h m s ->
      let
        qS = s `quot` 60
        rS = s `rem` 60
      in
        if qS > 0
        then (HMS h (m + qS) rS)
        else d

carryMins :: Duration -> Duration
carryMins d =
  case d of
    HMS h m s ->
      let
        qM = m `quot` 60
        rM = m `rem` 60
      in
        if qM > 0
        then (HMS (h + qM) rM s)
        else d

normalize :: Duration -> Duration
normalize d =
  carryMins $ carrySecs d

type Seconds = Int

toSeconds :: Duration -> Seconds
toSeconds d =
  case d of
    HMS h m s ->
      (60 * 60 * h) + (60 * m) + s

toDuration :: Seconds -> Duration
toDuration secs = HMS 0 0 secs

durMath :: (Int -> Int -> Int)
           -> Duration -> Duration
           -> Duration
durMath f d d' =
  let
    secs = toSeconds d
    secs' = toSeconds d'
  in
    toDuration (f secs secs')

durAdd :: Duration -> Duration -> Duration
durAdd = durMath (+)

durSubtract :: Duration -> Duration -> Duration
durSubtract = durMath (-)

durMultiply :: Duration -> Duration -> Duration
durMultiply = durMath (*)
