module Day9
    ( getPart1
    , getPart2
    ) where

import Data.List
import Data.List.Index
import Data.List.Split

getOpCode x = x `mod` 100
getParamMode position x = (floor (fromIntegral (x `div` (10^(position + 1))))) `mod` 10

getParamFromMode :: Int -> Int -> Int -> [Int] -> Int
getParamFromMode mode pos relativeBase program
  | mode == 0 = if (val < programLength) then (program !! val) else 0
  | mode == 1 = val
  | mode == 2 = if (nextRelativeBase < programLength) then (program !! nextRelativeBase) else 0
  where
  programLength = length program
  val = program !! pos
  nextRelativeBase = relativeBase + val

getWriteParamFromMode :: Int -> Int -> Int -> [Int] -> Int
getWriteParamFromMode mode pos relativeBase program
  | mode == 0 = val
  | mode == 1 = val
  | mode == 2 = if (nextRelativeBase < programLength) then nextRelativeBase else 0
  where
  programLength = length program
  val = program !! pos
  nextRelativeBase = relativeBase + val

type State = (Int, Int, [Int], Int, [Int])

intCodeComputer :: [Int] -> State -> State
intCodeComputer inputs (position, relBase, xs, code, output)
  | code == 1 = (posP4, relBase, (setAt param3 (param1 + param2) extendedProgram), code, output)
  | code == 2 = (posP4, relBase, (setAt param3 (param1 * param2) extendedProgram), code, output)
  | code == 3 = (posP2, relBase, (setAt param3 input extendedProgram), code, output)
  | code == 4 = (posP2, relBase, xs, code, (output ++ [param1]))
  | code == 5 = ((if (param1 > 0) then param2 else posP3), relBase, xs, code, output)
  | code == 6 = ((if (param1 == 0) then param2 else posP3), relBase, xs, code, output)
  | code == 7 = (posP4, relBase, (setAt param3 (if (param1 < param2) then 1 else 0) extendedProgram), code, output)
  | code == 8 = (posP4, relBase, (setAt param3 (if (param1 == param2) then 1 else 0) extendedProgram), code, output)
  | code == 9 = (posP2, (relBase + param1), xs, code, output)
  | code == 99 = (position, relBase, xs, code, output)
  | otherwise = error "bad opcode"
  where
    instruction = xs !! position
    code = getOpCode instruction
    (input, nextInputs) = if (code /= 3) then (0, inputs) else (head inputs, tail inputs)
    posP1 = position + 1
    posP2 = position + 2
    posP3 = position + 3
    posP4 = position + 4
    valAtPos3 = xs !! posP3
    programLength = length xs
    extendedProgram = if (valAtPos3 < programLength) then xs else (xs ++ (replicate (valAtPos3 - programLength + 1) 0))
    param1 = getParamFromMode (getParamMode 1 instruction) posP1 relBase xs
    param2 = getParamFromMode (getParamMode 2 instruction) posP2 relBase xs
    param3 = getWriteParamFromMode (getParamMode 3 instruction) posP3 relBase xs


getProgram :: String -> [Int]
getProgram = map read . splitOn ","

getPart1 str = do
  let program = getProgram str
      initialState = (0, 0, [109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99], 0, [])
      initialState2 = (0, 0, [1102,34915192,34915192,7,4,7,99,0], 0, [])
      initialState3 = (0, 0, [104,1125899906842624,99], 0, [])
      initialState4 = (0, 0, program, 0, [])
      initialState5 = (0, 0, [3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31, 1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104, 999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99], 0, [])
      runProgram state = do
        let nextState = intCodeComputer [1] state
            (position, relBase, xs, code, output) = nextState
        putStrLn (if (code == 4) then (show nextState) else "")
        if (code == 99) then return() else runProgram nextState
  runProgram initialState4

getPart2 str = 6
