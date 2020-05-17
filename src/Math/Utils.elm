module Math.Utils exposing (decimals, equalLenList)


decimals : Int -> List Int
decimals num =
    let
        dec n acc =
            if n > -10 && n < 10 then
                n :: acc

            else
                dec (n // 10) (remainderBy 10 n :: acc)
    in
    dec num []


equalLenList : Bool -> a -> List a -> List a -> ( List a, List a )
equalLenList prepend filler lst1 lst2 =
    let
        lst1len =
            List.length lst1

        lst2len =
            List.length lst2

        merge =
            if prepend then
                \a b -> a ++ b

            else
                \a b -> b ++ a
    in
    if lst1len > lst2len then
        ( lst1, merge (List.repeat (lst1len - lst2len) filler) lst2 )

    else
        ( merge (List.repeat (lst2len - lst1len) filler) lst1, lst2 )
