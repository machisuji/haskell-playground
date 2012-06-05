-- | Takes a list of numbers and 'collapses' that numbers into ranges.
-- | Example: (collapse [1,2,3,5,6,8]) == ([(1,3),(5,6),(8,8)])
collapse :: (Enum t, Eq t) => [t] -> [(t, t)]
collapse [] = []
collapse (x:xs) = reverse (collapseToRanges (x, x) xs [])
  where
  	collapseToRanges range [] ranges = range:ranges
	collapseToRanges (x, y) (z:xs) ranges
		| z == succ y = collapseToRanges (x, z) xs ranges
		| otherwise   = collapseToRanges (z, z) xs ((x, y):ranges)
