module Day6
    ( getPart1
    , getPart2
    ) where

import Data.List
import Data.List.Index
import Data.List.Split

type Edge = (String, String)

getEdge :: String -> Edge
getEdge str = (node1, node2)
  where
    xs = splitOn ")" str
    node1 = xs !! 0
    node2 = xs !! 1

getRootEdge :: [Edge] -> Edge
getRootEdge = head . filter (\(head, tail) -> head == "COM")

getIndirectCount :: [Edge] -> Int -> String -> Int
getIndirectCount [] count left = count
getIndirectCount edges count left = foldl (\acc (_, mR) -> acc + (getIndirectCount rest (count + 1) mR)) count matches
  where
  (matches, rest) = foldl (\(m, r) edge -> if (left == (fst edge)) then (edge:m, r) else (m, edge:r)) ([], []) edges

-- direct orbits are the number of initial edges
getPart1 xs = (length edges) + (getIndirectCount edges 0 (snd rootEdge))
  where
  edges = map getEdge xs
  rootEdge = getRootEdge edges

getPart2 :: [String] -> String
getPart2 (head:tail) = head
