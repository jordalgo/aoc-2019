module Day6
    ( getPart1
    , getPart2
    ) where

import Data.List
import Data.List.Index
import Data.List.Split
import qualified Data.Map as Map

type Edge = (String, String)

getEdge :: String -> Edge
getEdge str = (node1, node2)
  where
    xs = splitOn ")" str
    node1 = xs !! 0
    node2 = xs !! 1

getRootEdge :: [Edge] -> Edge
getRootEdge = head . filter (\(head, tail) -> head == "COM")

splitEdgesWithMatch :: String -> [Edge] -> ([Edge], [Edge])
splitEdgesWithMatch match = foldl (\(m, r) edge -> if (match == (fst edge)) then (edge:m, r) else (m, edge:r)) ([], [])

getIndirectCount :: [Edge] -> Int -> String -> Int
getIndirectCount [] count _ = count
getIndirectCount edges count left = foldl (\acc (_, mR) -> acc + (getIndirectCount rest (count + 1) mR)) count matches
  where
  (matches, rest) = splitEdgesWithMatch left edges

-- direct orbits are the number of initial edges
getPart1 xs = (length edges) + (getIndirectCount edges 0 (snd rootEdge))
  where
  edges = map getEdge xs
  rootEdge = getRootEdge edges

getPathToCOM :: String -> Map.Map String String -> [String]
getPathToCOM x parentMap
  | x == "COM" = []
  | otherwise = parent : (getPathToCOM parent parentMap)
  where
    parent = parentMap Map.! x

getPart2 xs = (+) (length $ pathToYOU \\ pathToSAN) (length $ pathToSAN \\ pathToYOU)
  where
  edges = map getEdge xs
  parentMap = Map.fromList $ map (\(a,b) -> (b, a)) edges
  pathToYOU = getPathToCOM "YOU" parentMap
  pathToSAN = getPathToCOM "SAN" parentMap

