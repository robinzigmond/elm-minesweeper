module Types exposing (Grid, RealGrid, RealStatus(..), Status(..))

import Array exposing (Array)


type Status
    = Unknown
    | KnownMine
    | Neighbours Int


type RealStatus
    = Mine
    | Safe


type alias RealGrid =
    Array (Array RealStatus)


type alias Grid =
    Array (Array Status)
