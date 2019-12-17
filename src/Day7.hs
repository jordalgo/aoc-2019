module Day7
    ( getPart1
    , getPart2
    ) where

import Data.List
import Data.List.Index
import Data.List.Split

getOpCode x = x `mod` 100
getParameterX position x = (floor (fromIntegral (x `div` (10^(position + 1))))) `mod` 10
getParameter1 = getParameterX 1
getParameter2 = getParameterX 2
getParameter3 = getParameterX 3

type State = (Int, [Int], Int)

runProgram :: [Int] -> State -> State
runProgram inputs (position, xs, output)
  | code == 1 = runProgram nextInputs (posP4, (setAt param3 (first + second) xs), output)
  | code == 2 = runProgram nextInputs (posP4, (setAt param3 (first * second) xs), output)
  | code == 3 = runProgram nextInputs (posP2, (setAt param1 input xs), output)
  | code == 4 = runProgram nextInputs (posP2, xs, (xs !! param1))
  | code == 5 = runProgram nextInputs ((if (first > 0) then second else posP3), xs, output)
  | code == 6 = runProgram nextInputs ((if (first == 0) then second else posP3), xs, output)
  | code == 7 = runProgram nextInputs (posP4, (setAt param3 (if (first < second) then 1 else 0) xs), output)
  | code == 8 = runProgram nextInputs (posP4, (setAt param3 (if (first == second) then 1 else 0) xs), output)
  | otherwise = (code, xs, output)
  where
    instruction = xs !! position
    code = getOpCode instruction
    (input, nextInputs) = if (code /= 3) then (0, inputs) else (head inputs, tail inputs)
    param1Mode = getParameter1 instruction
    param2Mode = getParameter2 instruction
    param3Mode = getParameter3 instruction
    posP2 = position + 2
    posP3 = position + 3
    posP4 = position + 4
    param1 = xs !! (position + 1)
    param2 = xs !! posP2
    param3 = xs !! posP3
    first = if (param1Mode == 1) then param1 else (xs !! param1)
    second = if (param2Mode == 1) then param2 else (xs !! param2)


type State2 = (Int, [Int], Int, Int)

runProgramWithPause :: [Int] -> State2 -> State2
runProgramWithPause inputs (position, xs, code, output)
  | code == 1 = runProgramWithPause nextInputs (posP4, (setAt param3 (first + second) xs), code, output)
  | code == 2 = runProgramWithPause nextInputs (posP4, (setAt param3 (first * second) xs), code, output)
  | code == 3 = runProgramWithPause nextInputs (posP2, (setAt param1 input xs), code, output)
  | code == 4 = (posP2, xs, code, (xs !! param1))
  | code == 5 = runProgramWithPause nextInputs ((if (first > 0) then second else posP3), xs, code, output)
  | code == 6 = runProgramWithPause nextInputs ((if (first == 0) then second else posP3), xs, code, output)
  | code == 7 = runProgramWithPause nextInputs (posP4, (setAt param3 (if (first < second) then 1 else 0) xs), code, output)
  | code == 8 = runProgramWithPause nextInputs (posP4, (setAt param3 (if (first == second) then 1 else 0) xs), code, output)
  | otherwise = (position, xs, code, output)
  where
    instruction = xs !! position
    code = getOpCode instruction
    (input, nextInputs) = if (code /= 3) then (0, inputs) else (head inputs, tail inputs)
    param1Mode = getParameter1 instruction
    param2Mode = getParameter2 instruction
    param3Mode = getParameter3 instruction
    posP2 = position + 2
    posP3 = position + 3
    posP4 = position + 4
    param1 = xs !! (position + 1)
    param2 = xs !! posP2
    param3 = xs !! posP3
    first = if (param1Mode == 1) then param1 else (xs !! param1)
    second = if (param2Mode == 1) then param2 else (xs !! param2)

runProgramWithInputs :: [Int] -> [Int] -> Int
runProgramWithInputs inputs program = if (x /= 99) then error "bad exit" else output
  where
  (x, y, output) = runProgram inputs (0, program, 0)

data Amps = Amps {
  a :: State2,
  b :: State2,
  c :: State2,
  d :: State2,
  e :: State2
} deriving (Show)

createAmps :: State2 -> State2 -> State2 -> State2 -> State2 -> Amps
createAmps stateA stateB stateC stateD stateE = Amps {a=stateA, b=stateB, c=stateC, d=stateD, e=stateE}

runFeedback :: [Int] -> [Int] -> Amps -> Char -> Int
runFeedback settings inputs (Amps {a = a, b = b, c = c, d = d, e = e}) next
  | next == 'A' = let (position, program, code, output) = runProgramWithPause inputsWithSettings a in runFeedback nextSettings [output] (createAmps (position, program, code, output) b c d e) 'B'
  | next == 'B' = let (position, program, code, output) = runProgramWithPause inputsWithSettings b in runFeedback nextSettings [output] (createAmps a (position, program, code, output) c d e) 'C'
  | next == 'C' = let (position, program, code, output) = runProgramWithPause inputsWithSettings c in runFeedback nextSettings [output] (createAmps a b (position, program, code, output) d e) 'D'
  | next == 'D' = let (position, program, code, output) = runProgramWithPause inputsWithSettings d in runFeedback nextSettings [output] (createAmps a b c (position, program, code, output) e) 'E'
  | next == 'E' = let (position, program, code, output) = runProgramWithPause inputsWithSettings e in if (code == 99) then output else runFeedback nextSettings [output] (createAmps a b c d (position, program, code, output)) 'A'
  | otherwise = error "bad next character"
  where
  nextSettings = if ((length settings == 0)) then [] else (tail settings)
  inputsWithSettings = if ((length settings == 0)) then inputs else (head settings) : inputs


getProgram :: String -> [Int]
getProgram = map read . splitOn ","

runAmplifiers :: [Int] -> [Int] -> Int
runAmplifiers program settings = foldl (\prevInput setting -> runProgramWithInputs [setting, prevInput] program) 0 settings

getPart1 str = maximum $ map (runAmplifiers program) $ permutations [0..4]
  where
  program = getProgram str
  range = [0..4]

getPart2 :: String -> Int
getPart2 str = maximum $ map (\x -> runFeedback x [0] (createAmps (0, program, 0, 0) (0, program, 0, 0) (0, program, 0, 0) (0, program, 0, 0) (0, program, 0, 0)) 'A') $ permutations [5..9]
  where
  program = getProgram str
