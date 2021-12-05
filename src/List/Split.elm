module List.Split exposing (..)

import List.Extra exposing (dropWhile, splitWhen)


split : (a -> Bool) -> List a -> List (List a)
split f elts =
    let
        dropThem ( fst, snd ) =
            dropWhile f fst :: split f (dropWhile f snd)
    in
    splitWhen f elts
        |> Maybe.map dropThem
        |> Maybe.map (List.filter (not << List.isEmpty))
        |> Maybe.withDefault []
