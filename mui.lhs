The MUI for HaskmelBudddy

> {-# LANGUAGE Arrows #-}
> module HaskmelBuddy.MUI where
> import HaskmelBuddy
> import Euterpea
> import Euterpea.Examples.MUI
> import Control.Arrow
> import Codec.Midi
> import Data.Maybe
> import Data.Time
> import System.IO
> import Control.Concurrent

Constants
==============================

> -- Arrays for radio buttons
> keys :: [PitchClass]
> keys = [C, Cs, D, Ef, E, F, Fs, G, Af, A, Bf]
> modes :: [Mode]
> modes = [Major, Minor]
> -- Phrase length can be either 2, 4, 8 or 16 measures long
> phrLens :: [(String, Int)]
> phrLens = [("Two", 2), ("Four", 4),
>                     ("Eight", 8), ("Sixteen", 16)]
> -- Allow for 2, 3, 4, 6 or 8 beats per measure
> bpMeasures :: [(String, Int)]
> bpMeasures = [("Two", 2), ("Three", 3), ("Four", 4),
>                     ("Six", 6), ("Eight", 8)]

> skinny = setLayout $ makeLayout (Fixed 100) (Stretchy 500)
> medium = setLayout $ makeLayout (Fixed 150) (Stretchy 500)
> large = setLayout $ makeLayout (Fixed 300) (Stretchy 500)

> -- The UI
> hbui = leftRight $ proc _ -> do
>   (idevid, odevid) <- large $ getDeviceIDs -< ()
>   input <- midiIn -< idevid
>   -- key radio button from keys
>   keyIndex <- skinny $ leftRight $ title "Key" $
>                   radio (map show keys) 0 -< ()
>   -- mode radio button from modes
>   modeIndex <- skinny $ leftRight $ title "Major/Minor" $
>                   radio (map show modes) 0 -< ()
>   -- phrase length radio button
>   phrLenIndex <- medium $ leftRight $ title "Measures per Phrase" $
>                   radio (fst (unzip phrLens)) 1  -< ()
>   -- beats per measure radio button
>   bpMeasureIndex <- medium $ topDown $ title "Beats per Measure" $
>                   radio (fst (unzip bpMeasures)) 2 -< ()
>   -- tempo slider (in bpm), tempo = tempo in bpm
>   tempo' <- title "Tempo" (viSlider 10 (60, 240) 100) -< ()
>   -- title "Tempo" display -< tempo'
>   -- setup timer. Ticks for every beat, measure and phrase
>   let bpMeasure = snd $ bpMeasures !! bpMeasureIndex
>       phrLen = snd $ phrLens !! phrLenIndex
>       key = keys !! keyIndex
>       mode = modes !! modeIndex
>       secondsPerBeat = 60/(fromIntegral tempo')
>       secondsPerMeasure = secondsPerBeat * fromIntegral(bpMeasure)
>       secondsPerPhrase = secondsPerMeasure * fromIntegral(phrLen)
>   t <- time -< ()
>   tick <- timer -< (t, secondsPerBeat)
>   measureTick <- timer -< (t, secondsPerMeasure)
>   phraseTick <- timer -< (t, secondsPerPhrase)
>   rec beatCount <- hold 0 -< fmap(const $ (beatCount + 1) `mod` bpMeasure) tick
>   rec measureCount <- hold 0 -< fmap(const $ (measureCount + 1) `mod` phrLen) measureTick
>   -- ap    <- title "Note" (hiSlider 1 (0, 12) 0) -< ()        -- For testing, choose an ap
>   -- uap   <- unique -< ap --getChromNote2 chromNotes ap 5     -- Every time it's unique
>   let ap = if isNothing measureTick then midiExtract $ fmap head input  -- Get first ap from input
>            else Just (-1) -- clear hbcollect on measureTick
>   -- Update cnotes
>   rec cnotes <- hold [] -< hbcollect cnotes ap
>   -- Every measureTick, profile the notes in cnotes
>   rec mchord <- hold [] -< fmap (const $ hbprofile cnotes (key, mode)) measureTick
>   -- Also update phrasechords. We use measurecount -1 becuase it takes a while for hbprofile to work
>   rec phraseChords <- hold [] -< fmap (const $ 
>                                        updateOrAppend (mod (measureCount - 1) phrLen) mchord 
>                                       (take phrLen phraseChords)) measureTick
>   large $ title "Your chord progression" 
>           display -< show $ map chordToKeyString phraseChords
>   skinny $ title "Beat" display -< show $ beatCount + 1
>   skinny $ title "Measure" display -< show $ measureCount + 1
>   -- title "Last measure chord" display -< show mchord
>   large $ title "Notes played this measure"
>           display -< show $ map (fst . pitch) cnotes

>   let keyC = if phraseChords == [] then [] else head phraseChords
>       keyChord = if length keyC == 3 then Just keyC else Nothing
>       chordOut = fmap (map (\k -> ANote 0 k 120 secondsPerMeasure)) keyChord
>       metroVelocity = if isNothing measureTick then 20 else 100
>       tickOut = fmap (const [ANote 9 37 metroVelocity 0.1]) tick
>   medium $ title "Background chord" display -< show $ fmap chordToKeyString keyChord
>   midiOut -< (odevid, chordOut)

> hbmui = runUIEx (3000, 1000) "HaskmelBuddy" (setLayout (makeLayout (Stretchy 500) (Stretchy 1000)) hbui)

Functions
==============================

> -- Gets the ap from the midimessage
> midiExtract :: Maybe MidiMessage -> Maybe AbsPitch
> midiExtract Nothing = Nothing
> midiExtract (Just m) = 
>   case m of
>         Std (NoteOn c k v) -> Just k
>         Std (NoteOff c k v) -> Nothing

> -- updates index if it exists in arr, otherwise appends.
> updateOrAppend :: Int -> [a] -> [[a]] -> [[a]]
> updateOrAppend _ [] arr = arr -- Won't overwrite with nothing
> updateOrAppend index val arr = 
>     if index >= length arr then arr ++ [val]
>     else changeIndex index val arr


> toChord' i ms@(m:_) =
>   case m of
>       Std (NoteOn c k v) -> f NoteOn c k v
>       Std (NoteOff c k v) -> f NoteOff c k v
>       _ -> ms
>   where f g c k v = map (\k' -> Std (g c k' v))
>                           (scanl (+) k (snd (chordIntervals !! i )))

chordIntervals :: [(String, [Int ]) ]
chordIntervals = [("Maj", [4, 3, 5]), ("Maj7", [4, 3, 4, 1]),
                    ("Maj9", [4, 3, 4, 3]), ("Maj6", [4, 3, 2, 3]),
                    ("min", [3, 4, 5]), ("min7", [3, 4, 3, 2]),
                    ("min9", [3, 4, 3, 4]), ("min7b5", [3, 3, 4, 2]),
                    ("mMaj7", [3, 4, 4, 1]), ("dim", [3, 3, 3]),
                    ("dim7", [3, 3, 3, 3]), ("Dom7", [4, 3, 3, 2]),
                    ("Dom9", [4, 3, 3, 4]), ("Dom7b9", [4, 3, 3, 3])]

> -- Hudak's code
> chromNotes = [("C", 48), ("Cs", 49), ("D", 50), ("Ds", 51), ("E", 52), ("F", 53), 
>               ("Fs", 54), ("G", 55), ("Gs", 56), ("A", 57), ("As", 58), ("B", 59)]

> getChromNote2 :: [(a, AbsPitch)] -> Int -> Int -> AbsPitch
> getChromNote2 xs i o = let f (a, b) = (b - 48) + (o * 12)
>                     in f (xs !! i)
