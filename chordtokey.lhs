Chord to Key

> module CtK where
> import Euterpea
> import Data.List
> import Data.Bool



> chordToKey :: [AbsPitch] -> String
> chordToKey xs = chordToKeyHelper (map ((flip $ mod) 12) xs)


> chordToKeyHelper :: [AbsPitch] -> String
> chordToKeyHelper xs = let f (p, o) = p
>                       in case (mod ((xs !! 1) - (xs !! 0)) 12) of
>   4 -> show (f $ pitch (xs !! 0)) ++ " Major"
>   3 -> case (mod ((xs !! 2) - (xs !! 1)) 12) of
>           4 -> show (f $ pitch (xs !! 0)) ++ " Minor"
>           (-8) -> show (f $ pitch (xs !! 0)) ++ " Minor"
>           3 -> show (f $ pitch (xs !! 0)) ++ " Diminished"
>           (-9) -> show (f $ pitch (xs !! 0)) ++ " Diminished"
>           _ -> "Not a Triad"
>   _ -> "Not a Triad"

> -- Tests
> cM, csM, efM, fsM, afM, bfM, bfM2, cm, bd :: [AbsPitch]
> cM = [0, 4, 7]
> csM = [1, 5, 8]
> efM = [3, 7, 10]
> fsM = [6, 10, 13]
> afM = [8, 12, 15]
> bfM = [10, 14, 17]
> bfM2 = [10, 2, 5]
> cm = [0, 3, 7]
> bd = [47, 50, 53]