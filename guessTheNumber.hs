guessTheNumber :: Int -> IO ()
guessTheNumber numberToGuess =
  do putStrLn "Please, try guess my number::";
     guess <- getLine;
     makeGuess numberToGuess (read guess :: Int);

makeGuess :: Int -> Int -> IO ()
makeGuess numberToGuess guess
  | numberToGuess > guess = wrongGuess numberToGuess "you are to small"
  | numberToGuess < guess = wrongGuess numberToGuess "you are to BIG"
  | otherwise = do putStrLn "You win";

wrongGuess :: Int -> String -> IO ()
wrongGuess numberToGuess message =
  do putStrLn message;
     guessTheNumber numberToGuess;