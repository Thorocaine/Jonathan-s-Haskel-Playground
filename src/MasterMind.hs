module MasterMind (masterMind,markGuess) where
    
    
    import System.Random
    
    
    startMasterMind :: IO ()
    startMasterMind =
      do n1 <- getStdRandom (randomR (0,9)) :: IO Int;
         n2 <- getStdRandom (randomR (0,9)) :: IO Int;
         n3 <- getStdRandom (randomR (0,9)) :: IO Int;
         n4 <- getStdRandom (randomR (0,9)) :: IO Int;
         startWithSecret n1 n2 n3 n4;
    
    startWithSecret :: Int -> Int -> Int -> Int -> IO ()
    startWithSecret n1 n2 n3 n4 =
      do let secret = show n1 ++ show n2 ++ show n3 ++ show n4;
         putStrLn "Welcome to my Mind...";
         putStrLn "I have a four digit code for you to guess";
         masterMind secret 0;
    
    masterMind :: String -> Int -> IO ()
    masterMind secret turn =
      do putStrLn "MM:>";
         guess <- getLine;
         checkGuess secret guess (turn + 1);
    
    checkGuess :: String -> String -> Int -> IO ()
    checkGuess secret guess turn
      | mark == "****" = putStrLn $ show turn ++ " tries"
      | otherwise = do putStrLn mark; masterMind secret turn;
      where mark = markGuess secret guess
        
    markGuess :: String -> String -> String
    markGuess secret guess =
      let (looseSecret, looseGuess, preciseMark) = checkPrecise secret guess "" 0;
          (_, _, finalMark) = checkLoose looseSecret looseGuess preciseMark
      in finalMark

    checkPrecise :: String -> String -> String -> Int -> (String, String, String)
    checkPrecise secret guess mark checkIndex =
      let exactlyCorrect = checkIndex < length guess && secret!!checkIndex == guess!!checkIndex
          nextCheckIndex = if exactlyCorrect then checkIndex else checkIndex + 1
          nextSecret = if exactlyCorrect then removeChar secret checkIndex else secret
          nextGuess = if exactlyCorrect then removeChar guess checkIndex else guess
          nextMark = if exactlyCorrect then mark ++ "*" else mark
      in if nextCheckIndex >= length guess then (nextSecret, nextGuess, nextMark) else checkPrecise nextSecret nextGuess nextMark nextCheckIndex
    
    checkLoose :: String -> String -> String -> (String, String, String)
    checkLoose secret guess mark =
      let guessHead = head guess 
          looslyCorrect = guessHead `elem` secret
          nextSecret = if looslyCorrect then deleteFirst guessHead secret else secret
          guessLength = length guess
          nextGuess = if guessLength > 0 then tail guess else ""
          nextMark = if looslyCorrect then mark ++ "." else mark
          remainingLength = length nextGuess
      in if remainingLength > 0 then checkLoose nextSecret nextGuess nextMark else (nextSecret, nextGuess, nextMark)

    removeChar :: String -> Int -> String
    removeChar text i = take i text ++ drop (1 + i) text

    deleteFirst :: Char -> String -> String
    deleteFirst _ [] = [] 
    deleteFirst a (b:bc) | a == b    = bc 
                         | otherwise = b : deleteFirst a bc