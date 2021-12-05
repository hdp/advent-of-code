main = do
  contents <- getContents
  print (processFile part1 contents)
  print (processFile part2 contents)
  where
    processFile f = f . map read . words

part1 :: [Integer] -> Integer
part1 [] = 0
part1 [x] = 0
part1 (a:b:xs) =
  (if b > a
     then 1
     else 0) +
  part1 ([b] ++ xs)

part2 :: [Integer] -> Integer
part2 xs
  | length xs < 4 = 0
part2 (a:x:y:b:xs) =
  (if b > a
     then 1
     else 0) +
  part2 ([x, y, b] ++ xs)
