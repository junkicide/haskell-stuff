data BrainfuckCommand = GoRight
                      | GoLeft
                      | Increment
                      | Decrement
                      | Print
                      | Read
                      | LoopL
                      | LoopR
                      | Comment Char

type BrainfuckSource = [BrainfuckCommand] deriving Show

parseBrainfuck :: String -> BrainfuckSource
parseBrainfuck = map charToBF
  where
    charToBF x = case x of
      '>' -> GoRight
      '<' -> GoLeft
      '+' -> Increment
      '-' -> Decrement
      '.' -> Print
      ',' -> Read
      '[' -> LoopL
      ']' -> LoopR
      c -> Comment c


data Tape a = Tape [a] a [a]

emptyTape :: Tape Int
emptyTape = Tape zeros 0 zeros
  where
    zeros = repeat 0


moveRight :: Tape a -> Tape a
moveRight (Tape ls p (r:rs)) = Tape (p:ls) r rs

moveLeft :: Tape a -> Tape a
moveLeft (Tape (l:ls) p rs) = Tape ls l (p:rs)

runBrainfuck :: BrainfuckSource -> IO()
runBrainfuck = run emptyTape . bfSource2Tape
  where
    bfSource2Tape (b:bs) = Tape [] b bs

run :: Tape Int -> Tape BrainfuckCommand -> IO ()
run dataTape source@(Tape _ GoRight _) = advance (moveRight dataTape) source
run dataTape source@(Tape _ GoLeft _) = advance (moveLeft dataTape) source

advance :: Tape Int -> TapeBrainfuckCommand -> IO()
advance dataTape (Tape _ _ []) = return ()
advance dataTape source = run dataTape (moveRight source)

run (Tape l p r) source@(Tape _ Increment _) = advance (Tape l (p+1) r) source
run (Tape l p r) source@(Tape _ Decrement _) = advance (Tape l (p-1) r) source
