import Data.Tuple.HT

main = do
  contents <- getContents
  print $ part1 $ parse contents
  print $ part2 $ parse contents
 where
  parse = map (toTuple . words) . lines
  toTuple [x, y] = (x, read y :: Integer)

part1 xs = a * b where (a, b) = part1' xs

part1' :: [(String, Integer)] -> (Integer, Integer)
part1' []       = (0, 0)
part1' (x : xs) = go x $ part1' xs
 where
  go ("forward", i) = mapSnd (+ i)
  go ("down"   , i) = mapFst (+ i)
  go ("up"     , i) = mapFst (+ (-i))

type State2 = (Integer, Integer, Integer)

part2 xs = h * d where (h, d, _) = part2' xs (0, 0, 0)

part2' :: [(String, Integer)] -> State2 -> State2
part2' []       = id
part2' (x : xs) = part2' xs . (go x)
 where
  go ("forward", i) = \(h, d, aim) -> (h + i, d + aim * i, aim)
  go ("down"   , i) = mapThd3 (+ i)
  go ("up"     , i) = mapThd3 (+ (-i))
