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

 isNewMeasure :: Int -> Int -> UTCTime -> IO UTCTime
 isNewMeasure t bm oldtime = 
   do now <- getCurrentTime
      if now < (addUTCTime (((fromIntegral bm) / (fromIntegral t)) * 60) oldtime) 
      then return now
      else return oldtime 
 -- t = tempo; bm = beats in a measure; notes = notes played so far in measure
 -- ks
 -- ot = old time (time the list began being constructed)
 hbTimedProfile :: Int -> Int -> UTCTime -> [AbsPitch] -> KSig -> Maybe [AbsPitch]
 hbTimedProfile t bm ot notes ks = 
       if (isNewMeasure t bm ot) > ot then 

 hbTimedClear :: Int -> Int -> UTCTime -> IO [AbsPitch]
 hbTimedClear t m ot = 
       do nt <- getCurrentTime
          if (nt < (laterTime t m ot)) then return []
          else return $ fromJust (hbcollect [] (Just (-1)))

> --fromIO :: IO a -> a
> --fromIO (IO a) = a

> -- Arrays for radio buttons!
> keys :: [PitchClass]
> keys = [C, Cs, D, Ef, E, F, Fs, G, Af, A, Bf]
> modes :: [Mode]
> modes = [Major, Minor]
> phrLens :: [(String, Int)]
> phrLens = [("Two", 2), ("Four", 4),
>                     ("Eight", 8), ("Sixteen", 16)]
> bpMeasures :: [(String, Int)]
> bpMeasures = [("Two", 2), ("Three", 3), ("Four", 4),
>                     ("Six", 6), ("Eight", 8)]

> r = [1, 2, 3]

> midiExtract :: Maybe MidiMessage -> Maybe AbsPitch
> midiExtract Nothing = Nothing
> midiExtract (Just m) = 
>   case m of
>         Std (NoteOn c k v) -> Just k
>         Std (NoteOff c k v) -> Nothing


-- better layout, radio button for key sig/major/min, slider for tempo, radio button for phrase length, beats per measure radio button

> hbui = leftRight $ proc _ -> do
>   (idevid, odevid) <- getDeviceIDs -< ()
>   input <- midiIn -< idevid
> -- key radio button: 0 = C, 1 = C#/Bb, ..., 11 = B
>   keyIndex <- leftRight $ title "Key" $
>                   radio (map show keys) 0 -< ()
> -- major or minor radio button: 0 = Major, 1 = Minor
>   modeIndex <- leftRight $ title "Major/Minor" $
>                   radio (map show modes) 0 -< ()
> -- phrase length radio button: Returns an Integer: 2, 4, 8, or 16
>   phrLenIndex <- leftRight $ title "Phrase Length (in measures)" $
>                   radio (fst (unzip phrLens)) 1  -< ()
> -- beats per measure radio button: Returns an integer: 2, 4, 6, or 8
>   bpMeasureIndex <- topDown $ title "Beats per Measure" $
>                   radio (fst (unzip bpMeasures)) 2 -< ()
> -- tempo slider (in bpm), tempo = tempo in bpm
>   tempo' <- title "Tempo" (hiSlider 10 (60, 240) 80) -< ()
>   title "Tempo" display -< tempo'
>   -- setup timer. Ticks for every beat, and measure (and phrase)
>   t <- time -< ()
>   let bpMeasure = snd $ bpMeasures !! bpMeasureIndex
>   let phrLen = snd $ phrLens !! phrLenIndex
>   let key = keys !! keyIndex
>   let mode = modes !! modeIndex
>   let secondsPerBeat = 60/(fromIntegral tempo')
>   let secondsPerMeasure = secondsPerBeat * fromIntegral(bpMeasure)
>   let secondsPerPhrase = secondsPerMeasure * fromIntegral(phrLen)
>   tick <- timer -< (t, secondsPerBeat)
>   measureTick <- timer -< (t, secondsPerMeasure)
>   phraseTick <- timer -< (t, secondsPerPhrase)
>   rec beatCount <- hold 0 -< fmap(const $ (beatCount + 1) `mod` bpMeasure) tick
>   rec measureCount <- hold 0 -< fmap(const $ (measureCount + 1) `mod` phrLen) measureTick
>   -- ap    <- title "Note" (hiSlider 1 (0, 12) 0) -< ()        -- For testing, choose an ap
>   -- uap   <- unique -< ap --getChromNote2 chromNotes ap 5     -- Every time it's unique
>   let ap = if isNothing measureTick then midiExtract $ fmap head input  -- Get first ap from input
>            else Just (-1) -- clear hbcollect
>   -- Update cnotes
>   rec cnotes <- hold [] -< hbcollect cnotes ap
>   -- Every measureTick, profile the notes in cnotes
>   rec mchord <- hold [] -< fmap (const $ hbprofile cnotes (key, mode)) measureTick
>   -- Array for keeping phrase of chords
>   rec phraseChords <- hold [] -< fmap (\mc -> changeIndex measureCount mc phraseChords) mchord
>   title "CNOTES" display -< show cnotes
>   title "Beat" display -< show $ beatCount + 1
>   title "Measure in phrase" display -< show $ measureCount + 1
>   title "Last measure chord" display -< show mchord

>
>   let metroVelocity = if isNothing measureTick then 20 else 100
>   -- let fOutput = mergeE (++) input (fmap (\k -> [ANote 1 k 100 1]) lastNote)
>   -- let sOutput = mergeE (++) input (fmap (const [ANote 9 37 metroVelocity 0.1]) tick)
>   -- let sout = fmap (const [ANote 9 37 metroVelocity 0.1]) tick
> -- output merged streams

>   midiOut -< (odevid, input)

To run:

> hbmui = runUIEx (2000, 1000) "HaskmelBuddy" hbui


> reader :: [(Pitch, String)] -> AbsPitch
> reader [] = (-1)
> reader (x:xs) = let f (a, b) = absPitch a
>               in f $ x

> combiner m n = if (isJust m) && (isJust n) then Just (fromJust m, fromJust n)
>                else Nothing

> keyTextBox' = proc _ -> do
>--   rec str1 <- textbox " " -< "cmajor" 
>       title "Chord" display -< "cmajor"
> keyTextBox = runUI "Current Chord" keyTextBox'

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

-- Tests for merging metronome sound and outputting note
--> pitchBox :: UISF () ()
--> pitchBox = proc _ -> do
-->   rec str <- textbox "Type Pitch"-< str
-->       let ap = absPitch (getduple (reads str))
-->       playbutton' <- edge <<< button "PLAY" -< ()
-->       let ap' = if (isJust playbutton') then Just ap else Nothing
-->       t <- time -< ()
-->       f <- title "Tempo" $ withDisplay (hSlider (1, 240) 1) -< ()
-->       tick <- timer -< (t, 60/f)

-->       let m' = mergeE (++) (fmap (\k -> [ANote 0 k 120 1.0]) ap') (fmap (const [ANote 9 37 100 0.1]) tick)
-->   midiOut -< (0, m')

> -- pitchBox' = runUI "Pitch Player" pitchBox

> -- Hudak's code
> chromNotes = [("C", 48), ("Cs", 49), ("D", 50), ("Ds", 51), ("E", 52), ("F", 53), 
>               ("Fs", 54), ("G", 55), ("Gs", 56), ("A", 57), ("As", 58), ("B", 59)]

> getChromNote :: [(a, AbsPitch)] -> Int -> AbsPitch
> getChromNote xs i = let f (a, b) = b
>                     in f (xs !! i)

> getChromNote2 :: [(a, AbsPitch)] -> Int -> Int -> AbsPitch
> getChromNote2 xs i o = let f (a, b) = (b - 48) + (o * 12)
>                     in f (xs !! i)

> ui5' = proc _ -> do
>   devid <- selectOutput -< ()
>   i <- topDown $ title "Chromatic Scale" $ radio (fst (unzip chromNotes)) 0 -< ()
>   ap <- title "Octave" (hiSlider 1 (0, 10) 0) -< ()
>   uap <- unique -< getChromNote2 chromNotes i ap
>   midiOut -< (devid, fmap (\k -> [ANote 0 k 100 1]) uap)
> mui5' = runUI "Pitch Player" ui5'