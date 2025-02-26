module Duration
  ( Duration (..)
  , normalize
  , durAdd
  , durSubtract
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
        then (HMS (h + qM) (m + rM) s)
        else d
    _ ->
      error "NOT REACHED: carrySecs: hmsIfy should have been called already"

normalize :: Duration -> Duration
normalize d =
  carryMins $ carrySecs $ hmsIfy d

durMath :: (Int -> Int -> Int)
           -> Duration -> Duration
           -> Duration
durMath f d d' =
  let
    dur = hmsIfy d
    dur' = hmsIfy d'
  in
    HMS (f (h dur) (h dur'))
        (f (m dur) (m dur'))
        (f (s dur) (s dur'))

durAdd :: Duration -> Duration -> Duration
durAdd = durMath (\a b -> a + b)

durSubtract :: Duration -> Duration -> Duration
durSubtract = durMath (\a b -> a - b)
