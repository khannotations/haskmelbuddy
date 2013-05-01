Profile analysis for HaskmelBuddy
By Danielle, Jackson, Rafi

> module HaskmelBuddy where
> import Euterpea
> import Data.Maybe
> import Data.List

Types
==============================

> -- For some reason, Euterpea wasn't picking this up
> data Mode = Major | Minor deriving (Eq, Show)
> -- My own Key Signature because I felt like it
> type KSig = (PitchClass, Mode)

Constants
==============================

> -- We assume that chords will be most common in this order:
> -- First, Fifth, Fourth, sixth, second, seventh, third. 
> baseFreqs = [6, 2, 0, 4, 5, 3, 1]
> -- Mapping chord degrees to absPitch offsets
> majorScalePitches = [0, 2, 4, 5, 7, 9, 11]
> -- Mapping absPitch offsets to chordDegrees (-1 => accidental)
> majorScaleDegrees = [0, -1, 1, -1, 2, 3, -1, 4, -1, 5, -1, 6]
> minorScaleDegrees = [0, -1, 1, 2, -1, 3, 4, -1, 5, -1, 6, -1]

Functions
==============================

> -- Compiles notes from Maybe notes
> hbcollect :: [a] -> Maybe a -> Maybe [a]
> hbcollect as = fmap ((flip (:)) as)

> -- Takes an array of AbsPitches and a KeySignature and guesses what chord
> -- in that KS it might be. Returns that chord as AbsPitches (in Octave 3)
> hbprofile :: [AbsPitch] -> KSig -> [AbsPitch]
> hbprofile notes (pc, mode) = 
>   -- First get the chord degree likelihoods of these notes in this key
>   let chordFreqs = getChordFreqs notes baseFreqs (pc, mode)--chordDegree = getChordFreqs notes baseFreqs (pc, mode)
>       chordDegree = case mode of 
>                       Major -> maxValueIndex chordFreqs -- Gets the most likely chord
>                       Minor -> (maxValueIndex chordFreqs + 5) `mod` 7 -- Maps this minor key onto its relative major
>       -- Calculate the third and the fifth
>       chordThird = (chordDegree + 2) `mod` 7
>       chordFifth = (chordDegree + 4) `mod` 7
>       chordArray = [chordDegree, chordThird, chordFifth]
>       -- Now translate into absPitch
>       keyRoot = absPitch (pc, 0)
>       chord = map ((+ keyRoot) . (majorScalePitches !!)) chordArray
>       -- Convert back from minor
>       mChord = if mode == Minor then map (subtract 7) chord else chord
>   in  map (+ 0) mChord -- Put it the third octave


> -- Returns the chord degree (First, Fourth, etc -- as an integer) of the 
> -- chord an array of AbsPitches is most likely to be. 
> -- For minor chords, it adds 5 to map it onto its relative major.
> -- That way, hbprofile only has to map 0 -> major, 1->minor, etc, instead
> -- of accounting for the differences in chord degrees from major to minor
> getChordFreqs :: [AbsPitch] -> [Int] -> KSig -> [Int]
> -- If the array is empty, we're done cycling
> getChordFreqs [] counts (pc, mode) = counts
> -- Otherwise, update the appropriate value in the counts array, making
> -- sure to check that the note is in the scale (not an accidental). 
> -- If it is, just skip it
> getChordFreqs (note:notes) counts (pc, mode) =
>   let scaleDeg = (note - absPitch (pc, 0)) `mod` 12 -- Note as a chromatic scale degree is this PitchClass
>       rootOfChord = case mode of 
>                       Major -> (majorScaleDegrees !! scaleDeg)
>                       Minor -> (minorScaleDegrees !! scaleDeg)
>       thirdOfChord = (rootOfChord + 5) `mod` 7 -- chord where scaleDeg is 3rd (i.e. the relative 6th)
>       fifthOfChord = (rootOfChord + 3) `mod` 7 -- chord where scaleDeg is 5th (i.e. the relative 4th)
>       -- Increment the chord where this note is the root by three, the third by 1 and the fifth by 2
>       updateCounts = incAtIndex rootOfChord 3 $ incAtIndex thirdOfChord 1 $ 
>                      (incAtIndex fifthOfChord 2 counts)
>       newCounts = if rootOfChord == -1 then counts else updateCounts
>   in getChordFreqs notes newCounts (pc, mode) -- Cycle through notes

> -- Increment the value at an index by a specified amount
> incAtIndex :: Num a => Int -> a -> [a] -> [a]
> incAtIndex index inc arr = changeIndex index ((arr !! index) + inc) arr


> -- Changes the value of an array at an index to the given value
> changeIndex :: Int -> a -> [a] -> [a]
> changeIndex _ _ [] = error "changeIndex: empty list"
> changeIndex index value as = (take index as) ++ (value : drop (index+1) as)

> -- Gets the index of the first maximum value
> maxValueIndex :: Ord a => [a] -> Int
> maxValueIndex as = fromJust $ elemIndex (maximum as) as

Tests
==============================

> -- hbcollect
> hbcoll1 = hbcollect [3, 2] (Just 4) -- Just [4, 3, 2]
> hbcoll2 = hbcollect (fromJust $ (hbcollect [3, 2] Nothing)) (Just 5) -- Error
> hbcoll3 = hbcollect (fromJust $ (hbcollect [3, 2] (Just 5))) Nothing -- Nothing


> hbtinc1 = incAtIndex 4 5 [0, 1, 2, 3, 4] -- [0, 1, 2, 3, 9]
> hbtinc2 = incAtIndex 4 5 [9] -- error
> hbtinc3 = incAtIndex 2 (-3) [9, 3, 5, 6] -- [9, 3, 2, 6]
> hbtinc4 = incAtIndex 0 5 $ incAtIndex 1 3 $ incAtIndex 2 1 [1, 2, 3] -- [6, 5, 4]

> -- hbtmax1 = maxValueIndex [] -- error (doesn't compile)
> hbtmax2 = maxValueIndex [1, 2, 3, 5, 2] -- 3
> hbtmax3 = maxValueIndex [3, 2, 1, 2, 3, 2] -- 0

> hbgetc1 = getChordFreqs [2, 2, 2] baseFreqs (C, Major) -- [6, 11, 0, 4, 11, 3, 4]
> hbgetc2 = getChordFreqs [2, 3, 4, 5, 5] baseFreqs (C, Major) -- [7, 7, 3, 10, 7, 5, 6]

> hbprof1 = hbprofile [2, 3, 4, 5, 5] (C, Major) -- [41, 45, 36] (the 4th)
> hbprof2 = hbprofile [2, 3, 4, 5, 5] (F, Major) -- [41, 45, 48] (same chord, now the root)
> hbprof3 = hbprofile [2, 3, 4, 5, 5] (D, Major) -- [38, 42, 45] (the root)
> hbprof4 = hbprofile [2, 3, 4, 5, 5] (Ef, Major) -- [10, 14, 5] (the fifth)

A Real Example: "Let It Be" by the Beatles
==============================

> -- majorScalePitches = [0, 2, 4, 5, 7, 9, 11]
> hbbeatks = (C, Major)
> hbbeat1 = map pitch (hbprofile [9, 9, 10, 4] hbbeatks) -- Ideally, 1 actually: 6
> hbbeat2 = map pitch (hbprofile [9, 9, 12, 14] hbbeatks) -- Ideally, 5 actually: 6
> hbbeat3 = map pitch (hbprofile [16, 16, 16, 14] hbbeatks) -- Ideally, 6 actually: 1
> hbbeat4 = map pitch (hbprofile [14, 12, 12] hbbeatks) -- Ideally, 4 actually: 1

> -- What if it were in C Minor?
> hbbeatmks = (A, Major)
> hbbeatm1 = map pitch (hbprofile [9, 9, 10, 4] hbbeatks) -- Ideally, 1 actually: 6
> hbbeatm2 = map pitch (hbprofile [9, 9, 12, 14] hbbeatks) -- Ideally, 5 actually: 6
> hbbeatm3 = map pitch (hbprofile [16, 16, 16, 14] hbbeatks) -- Ideally, 6 actually: 1
> hbbeatm4 = map pitch (hbprofile [14, 12, 12] hbbeatks) -- Ideally, 4 actually: 1

