> import Data.Array.Diff

> main = do
>	 let arr = listArray (1,1000) [1..1000] :: DiffArray Int Int
>	 	a = arr ! 1
>	 	arr2 = arr // [(1, 37)]
>	 	b = arr2 ! 1
>	print (a, b)
