Profile analysis for HaskmelBuddy
By Danielle, Jackson, Rafi

> module HaskmelBuddy where
> import Euterpea
> import Data.Maybe

> hbcollect :: Maybe a -> [a] -> Maybe [a]
> hbcollect a as = 	if isNothing(a) then Nothing
>										else if length as < 4 then Just (fromJust(a):as)
>													else Just (fromJust(a) : (init as))

> hbprofile :: [AbsPitch] -> ([AbsPitch], String)
> hbprofile aps = (aps, "C Major")