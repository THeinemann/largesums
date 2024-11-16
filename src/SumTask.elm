module SumTask exposing (Task, fromInt, sumString)


type alias Task =
    { sum : Int
    , summands : List Int
    }


fromInt : Int -> Task
fromInt x =
    let
        loop exp r acc =
            if r == 0 then
                { sum = x, summands = List.filter (\s -> s /= 0) acc }

            else
                loop (exp + 1) (r // 10) (modBy 10 r * (10 ^ exp) :: acc)
    in
    loop 0 x []


sumString : Task -> String
sumString task =
    String.concat (List.intersperse " + " (List.map String.fromInt task.summands))
