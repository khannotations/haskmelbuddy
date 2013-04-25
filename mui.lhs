The MUI for HaskmelBudddy

> {-# LANGUAGE Arrows #-}
> module HaskmelBuddy.MUI where

> import Prelude hiding (init)
> import HaskmelBuddy
> import Euterpea
> import Euterpea.Examples.MUI
> import Control.Arrow
> import Codec.Midi
> import Control.CCA.Types
> import Data.Maybe


> hbui = proc _ -> do
>   odevid <- selectOutput -< ()
>   idevid <- selectInput  -< ()
>   ap 	<- title "Note" (hiSlider 1 (0, 12) 0) -< ()		-- For testing, choose an ap
>   uap <- unique -< getChromNote2 chromNotes ap 5			-- Every time it's uniques
>   let cnotes = []
>   cnotes <- hold [] -< hbcollect uap cnotes
>   let lastNote = if isNothing(uap) then Nothing
>                  else Just (last cnotes) 
>   -- Should be the first note 4 times, then the note that was played 4 ago each time
>   midiOut -< (odevid, fmap (\k -> [ANote 0 k 100 1]) lastNote) 	-- Play it 

> hbmui = runUI "HaskmelBuddy" hbui

> reader :: [(Pitch, String)] -> AbsPitch
> reader [] = (-1)
> reader (x:xs) = let f (a, b) = absPitch a
>               in f $ x

> combiner m n = if (isJust m) && (isJust n) then Just (fromJust m, fromJust n)
>                else Nothing

> ui3' = proc _ -> do
>   rec str1 <- textbox "Pitch 1:" -< str1
>       str2 <- textbox "Pitch 2:" -< str2
>       let str1' = (reader (reads str1))
>       let str2' = (reader (reads str2))
>       t <- time -< ()
>       f <- title "Tempo" (hSlider (1, 10) 1) -< ()
>       tick <- timer -< (t, 1/f)
>       let n1 = str1'
>       let n2 = str2'
>   midiOut -< (0, fmap (const [ANote 0 n1 100 0.1, ANote 0 n2 100 0.1]) tick)
> mui3' = runUI "Pitch Player" ui3' 

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