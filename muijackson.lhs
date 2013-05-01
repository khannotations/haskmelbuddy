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
> import Control.Concurrent

> -- measureTime adds a measure's worth of time to a given time
> -- Used for comparing to old time to see if it's a new measure
> -- t = tempo; bm = beats per measure; UTCTime is the time
> checkNewMeasure :: Int -> Int -> UTCTime -> UTCTime -> Maybe UTCTime
> checkNewMeasure t bm oldTime compTime = 
>   let newMeasureTime = addUTCTime (((fromIntegral bm) / (fromIntegral t)) * 60) oldTime
>   in  if newMeasureTime > compTime then newMeasureTime
>       else Nothing 

> -- t = tempo; bm = beats in a measure; notes = notes played so far in measure
> -- ks
> -- ot = old time (time the list began being constructed)
> hbTimedProfile :: Int -> Int -> [AbsPitch] -> KSig -> UTCTime -> Maybe [AbsPitch]
> hbTimedProfile t bm notes ks ot = 
>       do nt <- getCurrentTime
>          if isNothing(checkNewMeasure t bm ot nt) then Nothing
>          else Just $ hbprofile notes ks


--Arrays for radio buttons!

> keyValues :: [(String, Int) ]
> keyValues = [("C", 0), ("C#", 1),
>                     ("D", 2), ("D#/Eb", 3),
>                     ("E", 4), ("F", 5),
>                     ("F#/Gb", 6), ("G", 7),
>                     ("G#/Ab", 8), ("A", 9),
>                     ("A#/Bb", 10), ("B", 11)]

> majMinArray = [("Major", 0), ("Minor", 1)]

> phraseLengthArray = [("Two", 2), ("Four", 4),
>                     ("Eight", 8), ("Sixteen", 16)]

> bpMeasureArray = [("Two", 2), ("Four", 4),
>                     ("Six", 6), ("Eight", 8)]

> r = [1, 2, 3]

> getNoteFromMidiMessage :: Maybe [MidiMessage] -> Int -> Int -> UTCTime -> Maybe AbsPitch
> getNoteFromMidiMessage ms t bm ot = 
>           do nt <- getCurrentTime
>              if isNothing (checkNewMeasure t bm ot nt) 
>              then if isNothing ms then Nothing
>                  else midiExtract $ head $ fromJust ms
>              else Just (-1)

> midiExtract :: MidiMessage -> Maybe AbsPitch
> midiExtract m = case m of
>                   Std (NoteOn c k v) -> Just k
>                   Std (NoteOff c k v) -> Nothing

-- better layout, radio button for key sig/major/min, slider for tempo, radio button for phrase length, beats per measure radio button

> hbui = leftRight $ proc _ -> do
>   (idevid, odevid) <- getDeviceIDs -< ()
>   input <- midiIn -< idevid

> --   i      <- leftRight $ title "Chord" $
> --            radio (fst (unzip chordIntervals)) 0 -< ()

> -- key radio button: 0 = C, 1 = C#/Bb, ..., 11 = B
>   keyValue    <- leftRight $ title "Key" $
>                   radio (fst (unzip keyValues)) 0 -< ()
> -- major or minor radio button: 0 = Major, 1 = Minor
>
>   majMin      <- leftRight $ title "Major/Minor" $
>                   radio (fst (unzip majMinArray)) 0 -< ()
> -- phrase length radio button: Returns an Integer: 2, 4, 8, or 16
>   phraseLength <- leftRight $ title "Phrase Length" $
>                   radio (fst (unzip phraseLengthArray)) 0 -< ()
> -- beats per measure radio button: Returns an integer: 2, 4, 6, or 8
>   bpMeasure <- topDown $ title "Beats per Measure" $
>                   radio (fst (unzip bpMeasureArray)) 0 -< ()
> -- tempo slider (in bpm), tempo = tempo in bpm
>   tempo' <- title "Tempo" (hiSlider 10 (60, 240) 120) -< ()
>   title "Tempo" display -< tempo'

>   -- ap    <- title "Note" (hiSlider 1 (0, 12) 0) -< ()        -- For testing, choose an ap
>   -- evs <- input
>   let ap = getNoteFromMidiMessage input
>   -- uap   <- unique -< ap --getChromNote2 chromNotes ap 5            -- Every time it's uniques
>   -- let cnotes = []
>   rec cnotes <- hold [] -< hbcollect cnotes ap
>   let lastNote = if isNothing(ap) then Nothing
>                  else if (cnotes == []) then Nothing
>                       else Just (last cnotes) 
>     -- Should be the first note 4 times, then the note that was played 4 ago each time

>   title "CNOTES" display -< show cnotes
>   title "Length" display -< show $ length cnotes

> --make metronome notes
>   t <- time -< ()
>   tick <- timer -< (t, 60/(fromIntegral tempo'))

--merge streams

>   let firstOutput = mergeE (++) input (fmap (\k -> [ANote 0 k 100 1]) lastNote)
>   let finalOutput = mergeE (++) firstOutput (fmap (const [ANote 9 37 1 0.1]) tick)

--output merged streams

>   midiOut -< (odevid, finalOutput)

--to run:

> hbmui = runUI "HaskmelBuddy" hbui




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