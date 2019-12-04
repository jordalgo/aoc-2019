module Day3
    ( getResult
    ) where

import qualified Data.List.Split as Split
import qualified Data.List as List

type Coordinate = (Int, Int)
type Line = (Coordinate, Coordinate)
type Path = (Line, Int)

getDirections :: String -> [String]
getDirections = Split.splitOn ","

getDistanceBetweenCoords :: Coordinate -> Coordinate -> Int
getDistanceBetweenCoords start end =
  if (startY == endY) then abs (endX - startX) else abs(endY - startY)
  where
    (startY, startX) = start
    (endY, endX) = end

getCoordinate :: (Int, Int) -> String -> (Int, Int)
getCoordinate (y, x) str
  | direction == 'R' = (y, x + amount)
  | direction == 'L' = (y, x - amount)
  | direction == 'U' = (y + amount, x)
  | direction == 'D' = (y - amount, x)
  where
    direction = head str
    amount = read (tail str)

getCoordinateWithAmount :: ((Int, Int), Int) -> String -> ((Int, Int), Int)
getCoordinateWithAmount ((y, x), initAmount) str
  | direction == 'R' = ((y, x + amount), initAmount + amount)
  | direction == 'L' = ((y, x - amount), initAmount + amount)
  | direction == 'U' = ((y + amount, x), initAmount + amount)
  | direction == 'D' = ((y - amount, x), initAmount + amount)
  where
    direction = head str
    amount = read (tail str)

getLineDirection :: Line -> Char
getLineDirection line =
  if x1 == x2 then 'v' else 'h'
  where
    ((y1, x1), (y2, x2)) = line

-- assume lines of the same direction don't intersect
-- (3,6),(3,2)
-- (5,3),(2,3)
doLinesIntersect :: Line -> Line -> Bool
doLinesIntersect lineA lineB =
  if (lineADirection == lineBDirection)
    then False
    else
      ((vy1 >= hy1 && vy2 <= hy2) || (vy1 <= hy1 && vy2 >= hy2)) && ((vx1 >= hx1 && vx1 <= hx2) || (vx1 <= hx1 && vx1 >= hx2))
  where
  lineADirection = getLineDirection lineA
  lineBDirection = getLineDirection lineB
  ((hy1, hx1), (hy2, hx2)) = if (lineADirection == 'h') then lineA else lineB
  ((vy1, vx1), (vy2, vx2)) = if (lineADirection == 'h') then lineB else lineA

getLineCoordinates :: (Int, Int) -> [String] -> [Line]
getLineCoordinates start [] = []
getLineCoordinates start (x:xs) =
  let nextCoordinate = getCoordinate start x
  in (start, nextCoordinate):(getLineCoordinates nextCoordinate xs)

getPaths :: ((Int, Int), Int) -> [String] -> [Path]
getPaths start [] = []
getPaths start (x:xs) = (((fst start), nextCoordinate), amount):(getPaths (nextCoordinate, amount) xs)
  where
    (nextCoordinate, amount) = getCoordinateWithAmount start x

buildLineCoordinates :: Line -> [(Int, Int)]
buildLineCoordinates tuple = [(i, j) | i <- yRange, j <- xRange ]
  where
    ((startY, startX), (endY, endX)) = tuple
    yRange = if startY < endY then [startY..endY] else [endY..startY]
    xRange = if startX < endX then [startX..endX] else [endX..startX]

getIntersectingLines :: [Line] -> [Line] -> [(Line, Line)]
getIntersectingLines linesA linesB =  filter (\(x,y) -> doLinesIntersect x y) [(x,y) | x <- linesA, y <- linesB]

getIntersectingPaths :: [Path] -> [Path] -> [(Path, Path)]
getIntersectingPaths pathsA pathsB =  filter (\(x,y) -> doLinesIntersect (fst x) (fst y)) [(x,y) | x <- pathsA, y <- pathsB]

getIntersectionPoint :: (Line, Line) -> (Int, Int)
getIntersectionPoint tuple = head (List.intersect (buildLineCoordinates (fst tuple)) (buildLineCoordinates (snd tuple)))

getDistance :: Line -> Coordinate -> Int
getDistance line intersectionPoint = getDistanceBetweenCoords (snd line) intersectionPoint

getTotalDistance :: (Path, Path) -> Int
getTotalDistance tuple = distance1 - (getDistance line1 intersectionPoint) + distance2 - (getDistance line2 intersectionPoint)
  where
    (path1, path2) = tuple
    (line1, distance1) = path1
    (line2, distance2) = path2
    intersectionPoint = getIntersectionPoint (line1, line2)

getManhattanDistance :: (Int, Int) -> Int
getManhattanDistance tuple = (abs x) + (abs y) where (x, y) = tuple

getResult1 xs =
  let lineCoords = map (getLineCoordinates (0, 0) . getDirections) xs
  in foldr1 min (map (getManhattanDistance . getIntersectionPoint) (getIntersectingLines (lineCoords !! 0) (lineCoords !! 1)))

getResult xs =
  let lineCoords = map (getPaths ((0, 0), 0) . getDirections) xs
  in foldr1 min (map getTotalDistance (getIntersectingPaths (lineCoords !!0) (lineCoords !! 1)))
