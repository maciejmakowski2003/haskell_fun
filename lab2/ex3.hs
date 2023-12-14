f x y = if (x>0) then 42 else x + y

neverEndingStory x = neverEndingStory (x + 1) `mod` 100