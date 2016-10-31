module Util exposing (..)


getElement : Int -> List a -> Maybe a
getElement index list =
    case list of
        [] ->
            Nothing
        x :: xs ->
            if index == 0
                then Just x
                else getElement (index - 1) xs


setElement : Int -> a -> List a -> List a
setElement index item list =
    case list of
        [] -> 
            []
        x :: xs -> 
            if index == 0
                then item :: xs
                else x :: setElement (index - 1) item xs

range : Int -> Int -> List Int
range start end =
    if start > end
    then [] 
    else start :: range (start + 1) end