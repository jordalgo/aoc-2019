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

getWriteParamFromMode :: Int -> Int -> Int -> [Int] -> (Int, [Int])
getWriteParamFromMode mode pos relativeBase program
  | mode == 0 = if (val < programLength) then (val, program) else (val, (program ++ (replicate (val - programLength + 1) 0)))
  | mode == 1 = error "should not be in immediate mode"
  | mode == 2 = if (nextRelativeBase < programLength) then (nextRelativeBase, program) else (nextRelativeBase, (program ++ (replicate (nextRelativeBase - programLength + 1) 0)))
  where
  programLength = length program
  val = program !! pos
  nextRelativeBase = relativeBase + val

data State = State {
  code :: Int,
  output :: [Int],
  position :: Int,
  program :: [Int],
  relativeBase :: Int
} deriving (Eq, Ord, Show)

intCodeComputer :: [Int] -> State -> State
intCodeComputer inputs (State {output = output, position = position, program = xs, relativeBase = relBase})
  | code == 1 = State {code = code, position = posP4, program = (setAt setterParam (param1 + param2) extendedProgram), relativeBase = relBase, output = output}
  | code == 2 = State {code = code, position = posP4, program = (setAt setterParam (param1 * param2) extendedProgram), relativeBase = relBase, output = output}
  | code == 3 = State {code = code, position = posP2, program = (setAt setterParam input extendedProgram), relativeBase = relBase, output = output}
  | code == 4 = State {code = code, position = posP2, program = xs, relativeBase = relBase, output = (output ++ [param1])}
  | code == 5 = State {code = code, position = ((if (param1 > 0) then param2 else posP3)), program = xs, relativeBase = relBase, output = output}
  | code == 6 = State {code = code, position = ((if (param1 == 0) then param2 else posP3)), program = xs, relativeBase = relBase, output = output}
  | code == 7 = State {code = code, position = posP4, program = (setAt setterParam (if (param1 < param2) then 1 else 0) extendedProgram), relativeBase = relBase, output = output}
  | code == 8 = State {code = code, position = posP4, program = (setAt setterParam (if (param1 == param2) then 1 else 0) extendedProgram), relativeBase = relBase, output = output}
  | code == 9 = State {code = code, position = posP2, program = xs, relativeBase = (relBase + param1), output = output}
  | code == 99 = State {code = code, position = posP2, program = xs, relativeBase = relBase, output = output}
  | otherwise = error "bad opcode"
  where
    instruction = xs !! position
    code = getOpCode instruction
    input = if ((code == 3) && ((length inputs) == 0)) then error "no inputs" else head inputs
    posP1 = position + 1
    posP2 = position + 2
    posP3 = position + 3
    posP4 = position + 4
    programLength = length xs
    param1 = getParamFromMode (getParamMode 1 instruction) posP1 relBase xs
    param2 = getParamFromMode (getParamMode 2 instruction) posP2 relBase xs
    (param3, extendedProgram) = getWriteParamFromMode (getParamMode 3 instruction) posP3 relBase xs
    setterParam = if (param3 < 0) then error "setter param is negative" else param3


getProgram :: String -> [Int]
getProgram = map read . splitOn ","

runProgramWithInputs prg inputs = do
  let initialState4 = State {code = 0, position = 0, program = prg, relativeBase = 0, output = []}
      runProgram state = do
        let nextState = intCodeComputer inputs state
            c = code nextState
            nextCode = getOpCode ((program nextState) !! (position nextState))
        if (c == 99) then putStrLn (show (output nextState)) else runProgram nextState
  runProgram initialState4

getPart1 str = do
  let program = getProgram str
  runProgramWithInputs program [1]

getPart2 str = do
  let program = getProgram str
  runProgramWithInputs program [2]
