Profile analysis for HaskmelBuddy
By Danielle, Jackson, Rafi

> module HaskmelBuddy where
> import Euterpea
> import Data.Maybe
> import Data.List

> -- My own Key Signature because I felt like it
> type KSig = (PitchClass, Mode)

> -- Compiles notes
> hbcollect :: Maybe a -> [a] -> Maybe [a]
> hbcollect a as = if isNothing(a) then Nothing
>                  else Just (fromJust(a) : (init as))

> -- Takes an array of AbsPitches and a KeySignature and guesses what chord
> -- in that KS it might be. Returns that chord as AbsPitches (in Octave 3)
> hbprofile :: [AbsPitch] -> KSig -> [AbsPitch]
> hbprofile notes (pc, mode) = 
>       -- First get the chord degree of these notes in this key
>   let chordDegree = getChordDegree notes commonChords (pc, mode)
>       -- Calculate the third and the fifth
>       chordThird = (chordDegree + 2) `mod` 7
>       chordFifth = (chordDegree + 4) `mod`7
>       -- Now translate into absPitch
>       keyRoot = absPitch (pc, 0)
>       chord = map ((+ keyRoot) . (majorScalePitches !!)) [chordDegree, chordThird, chordFifth]

>       
>   in if mode == Minor then map (subtract 7) chord else chord

> -- We assume that chords will be most common in this order:
> -- First, Fifth, Fourth, sixth, second, seventh, third. 
> commonChords = [6, 2, 0, 4, 5, 3, 1]

> majorScalePitches = [0, 2, 4, 5, 7, 9, 11]
> majorScaleDegrees = [0, -1, 1, -1, 2, 3, -1, 4, -1, 5, -1, 6]
> minorScaleDegrees = [0, -1, 1, 2, -1, 3, 4, -1, 5, -1, 6, -1]


> -- Returns the chord degree (First, Fourth, etc -- as an integer) of the 
> -- chord an array of AbsPitches is most likely to be. 
> -- For minor chords, it adds 5 to map it onto its relative major.
> -- That way, hbprofile only has to map 0 -> major, 1->minor, etc, instead
> -- of accounting for the differences in chord degrees from major to minor
> getChordDegree :: [AbsPitch] -> [Int] -> KSig -> Int
> -- If the array is empty, counts has been updated or there were no notes.
> -- If no notes, then this will return the tonal. Otherwise, it will get 
> -- the maximum index we've calculated over other iterations (the chord)
> getChordDegree [] counts (pc, mode) = 
>     case mode of
>         Major -> maxValueIndex counts               -- Gets the most frequented chord
>         Minor -> (maxValueIndex counts) + 5 `mod` 7 -- Maps this minor key onto its relative major
> -- Otherwise, update the appropriate value in the counts array, making
> -- sure to check that the note is in the scale (an accidental). If not, just skip it
> getChordDegree (note:notes) counts (pc, mode) =
>   let scaleDeg = (note - absPitch (pc, 0)) `mod` 12 -- Note as a chromatic scale degree is this PitchClass
>   in  newCounts = 
>  = case mode of
>               Major ->
>                 if elem scaleDeg majorScaleDegs 
>                    -- Increment the chords with this note as the root by 3, the third by 1 and the fifth by 2
>                    then incAtIndex 3 scaleDeg $ incAtIndex 1 ((scaleDeg + 3) `mod` 7) $ (incAtIndex 2 ((scaleDeg + 5) `mod` 7) counts)
>                    else counts
>               Minor -> 
>                 if elem scaleDeg minorScaleDegs
>                    then incAtIndex 3 scaleDeg $ incAtIndex 1 ((scaleDeg + 3) `mod` 7) $ (incAtIndex 2 ((scaleDeg + 5) `mod` 7) counts)
>                    else counts
>   in getChordDegree notes newCounts (pc, mode)


> -- Changes the value of an array at an index to the given value
> changeIndex :: Int -> a -> [a] -> [a]
> changeIndex _ _ [] = error "changeIndex: empty list"
> changeIndex index value as = if index >= length as then error "changeIndex: index Too Large"
>                              else (take index as) ++ (value : drop (index+1) as)

> -- Gets the index of the first maximum value
> maxValueIndex :: Ord a => [a] -> Int
> maxValueIndex as = fromJust $ elemIndex (maximum as) as -- maximum as must be in as
