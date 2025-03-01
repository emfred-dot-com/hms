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
  | MS
  { m :: Int
  , s :: Int
  }
  | S
  { s :: Int
  }
  deriving (Eq)

instance Show Duration where
  show d =
    let d' = hmsIfy d in
      case d' of
        HMS h m s ->
          showDig h ++ ":" ++ showDig m ++ ":" ++ showDig s
        _ -> error "NOT_REACHED: show duration: hmsIfy should have been called already"
    where
      showDig :: Int -> String
      showDig n =
        let str = show n
        in case str of
          _digit : [] -> "0" ++ str
          digits -> digits

hmsIfy :: Duration -> Duration
hmsIfy d =
  case d of
    HMS h m s -> HMS h m s
    MS m s -> HMS 0 m s
    S s -> HMS 0 0 s

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
    _ ->
      error "NOT REACHED: carrySecs: hmsIfy should have been called already"

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
    _ ->
      error "NOT REACHED: carrySecs: hmsIfy should have been called already"

normalize :: Duration -> Duration
normalize d =
  carryMins $ carrySecs $ hmsIfy d

type Seconds = Int

toSeconds :: Duration -> Seconds
toSeconds d =
  case (hmsIfy d) of
    HMS h m s ->
      (60 * 60 * h) + (60 * m) + s
    _ ->
      error "NOT REACHED: toSeconds: hmsIfy should have been called already"

toDuration :: Seconds -> Duration
toDuration secs = S secs

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
durAdd = durMath (\a b -> a + b)

durSubtract :: Duration -> Duration -> Duration
durSubtract = durMath (\a b -> a - b)

durMultiply :: Duration -> Duration -> Duration
durMultiply = durMath (\a b -> a * b)
