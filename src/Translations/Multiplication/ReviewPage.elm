-- Do not manually edit this file, it was auto-generated by yonigibbs/elm-i18next-gen
-- https://github.com/yonigibbs/elm-i18next-gen


module Translations.Multiplication.ReviewPage exposing (..)

import I18Next exposing (Delims(..), Translations, t, tr)


operation : Translations -> String -> String -> String
operation translations num total =
    tr translations Curly "multiplication.reviewPage.operation" [ ( "num", num ), ( "total", total ) ]


time : Translations -> String -> String
time translations t =
    tr translations Curly "multiplication.reviewPage.time" [ ( "t", t ) ]


retries : Translations -> String -> String
retries translations n =
    tr translations Curly "multiplication.reviewPage.retries" [ ( "n", n ) ]


passed : Translations -> String -> String
passed translations result =
    tr translations Curly "multiplication.reviewPage.passed" [ ( "result", result ) ]


yes : Translations -> String
yes translations =
    t translations "multiplication.reviewPage.yes"


no : Translations -> String
no translations =
    t translations "multiplication.reviewPage.no"


return : Translations -> String
return translations =
    t translations "multiplication.reviewPage.return"
