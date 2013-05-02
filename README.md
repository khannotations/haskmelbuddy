# HaskmelBuddy
### A play-along chord profiler and harmonizer written in Haskell with Euterpea
#### CS 431 Final Project, May 2013
#### By Rafi Khan, Danielle Zucker and Jackson Thea

## Motivation

The project began because Jackson wanted something to harmonize with him as he sang into a mic. The goal was to have software that took MIDI input and an interface into which you could specify some settings. With that, HaskmelBuddy would analyze the inputted notes, profile them to see what chord they were most likely to be a part of, and play that chord as you played.

However, this initial idea proved very difficult. First, getting MIDI input from voice is not a trivial task; to solve this we resolved to use only a piano at first—obtaining a MIDI mic would be for later. The greater challenge was real-time analysis of notes. It's impossible to predict the chord one is currently playing in, because the input is just too sparse and coming in too fast. Instead, we provided an interface where one specifies a Key Signature, Time Signature and "Phrase Length": how many measures are in one musical phrase in the piece one is playing. This way, we can take advantage of the tendency in music that consecutive phrases usually have the same chord progression.

Our project has two main parts: the MUI (frontend) and the chord analysis (backend)

## The MUI

The MUI takes several pieces of data from the user. The user must input the source of the MIDI input, which should be a keyboard, the key the user will be playing in, the number of measures in a phrase in the music, and the number of beats per measure. The MUI also contains a `vislider` for the tempo of the music, which plays a metronome along with the user. For clarity, the MUI also displays `cnotes` (the notes being collected for analysis) and the current triad being played along with the user.

## The Analysis

Every measure, the MUI collects all the input notes with `hbcollect`. (The amount of time per measure is obtained from the tempo and time signature). Then, at the start of the next measure, it sends the notes it collected to the note profiler (`hbprofile`), which does the analysis. The MUI then keeps track of which measure in the phrase corresponds to the chord `hbprofile` returned in the array `phraseChords`. That way, in the following phrase, it can play the correct chords each measure. Of course, `hbprofile` continues to analyze the input notes so that if the chord progression changes, the changes are reflected in the following measure.

## hbprofile

(A note for this section: chords are written with capital case, while notes are written in lower case. That is, in the key of C Major, first refers to the note "C", while First refers to the chord "C E G").

`hbprofile` works by assigning points to chords in the given key signature. It assumes that in a given key signature, certain chords tend to come up more frequently. The order we determined was the First (root), Fifth, Fourth, Sixth, Second, Seventh. Thus, the First starts out with more points than the Fifth, which has more points than the Fourth, and so on. When `hbprofile` receives its notes from `hbcollect`, it keeps track of each chord that each note *is a part of*. That is, if the fifth is passed to it, `hbprofile` will count that as "points" for the Fifth, Third and First, because all those chords have the fifth in them. After tallies points for all the notes, it determines which chord has the most points, and returns that chord. 

## Future work

Something that was unfortunate with our hardware setup was the lag (about a quarter second) between the playing of the note on the MIDI device and it being output to the user. This also meant that lag was present in our analysis, so `hbprofile` didn't complete until the measure after it started! Thus, `phraseChords` doesn't update until two measures after the original input. Had we had more time, we would have implemented a faster profiler and polished the layout of our MUI.