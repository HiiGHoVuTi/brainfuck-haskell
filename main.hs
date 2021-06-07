
import System.IO
import Control.Monad
import Data.Char

-- Alias important types
type InterpreterStack = [Int]
type MemoryTape = (Int, [Int])

-- Helper Functions
set_at :: MemoryTape -> Int -> MemoryTape
set_at (i, vals) n = (i, take i vals ++ [n] ++ drop (i + 1) vals)

modify_by :: MemoryTape -> Int -> MemoryTape
modify_by (i, vals) k = (i, take i vals ++ [vals!!i + k] ++ drop (i + 1) vals)

move_by :: MemoryTape -> Int -> MemoryTape
move_by (i, vals) k = (i + k, vals)

-- Run functions
run :: MemoryTape -> InterpreterStack -> String -> Int -> IO MemoryTape
run tape stack instr idx = case instr!!idx of

  '+' -> run (modify_by tape   1)  stack instr (idx + 1)
  '-' -> run (modify_by tape (-1)) stack instr (idx + 1)

  '>' -> run (move_by   tape   1)  stack instr (idx + 1)
  '<' -> run (move_by   tape (-1)) stack instr (idx + 1)

  '.' -> do
    putChar $ chr $ (snd tape)!!(fst tape)
    run tape stack instr (idx + 1)

  ',' -> do
    c <- getChar
    let n = ord c
    run (set_at tape n) stack instr (idx + 1)

  '[' -> run tape ([idx] ++ stack) instr (idx + 1)

  ']' -> if (snd tape)!!(fst tape) == 0
    then
           run tape (tail stack) instr (idx + 1)
    else
           run tape stack instr (head stack)

  otherwise -> if idx >= length instr - 1
    then
                 return tape
    else
                 run tape stack instr (idx + 1)

-- Main function
main :: IO ()
main = do
  handle <- openFile "source.bf" ReadMode
  contents <- hGetContents handle
  putStrLn []
  putStrLn $ "Running Program..."
  putStrLn []
  tape <- run (0, map (\_->0) [0..]) [] contents 0
  putStrLn []
  putStrLn "Output..."
  putStrLn $ show $ take 10 (snd tape)


