{-**************************************

        dominoes.hs
        Written by Aydin Hepsaydir
        Date Started: 11/10/2017

**************************************-}

module Dominoes where

  -- datatyp for a domino
  type Domino = (Int, Int)

  -- datatype for a hand
  type Hand = [Domino]

  -- datatype for a board
  type Board = [Domino]

  -- datatype for an end
  data End = L | R
      deriving (Eq, Show)

  {- Predicate returning True if a given domino can be played at a given end of a
      given board -}
  goesP :: Domino -> End -> Board -> Bool
  goesP _ _ [] = True
  goesP (fD,sD) L ((fB,sB): t) -- checks the left hand of the board
    | fB == fD || fB == sD = True
    | otherwise = False

  goesP (fD, sD) R board -- checks the right side of board
    | last_item_board == fD || last_item_board == sD = True
    | otherwise = False
      where last_item_board = snd (last board)

  {- Predicate given a Hand and a Board returning True if there is no
      domino in the hand which can be played on the given board (i.e. the player is
      ‘knocking’). -}
  knockingP :: Hand -> Board -> Bool
  knockingP [] _ = True
  knockingP _ [] = False
  knockingP (h : t) board
    | goesP h L board = False
    | goesP h R board = False
    | otherwise = knockingP t board-- recursion

  {- predicate returning True if a given Domino has already been played on a
      given Board. -}
  playedP :: Domino -> Board -> Bool
  playedP _ [] = False
  playedP domino (h : t)
    | domino == h = True
    | otherwise = playedP domino t -- recursion

  {- given a Hand and a Board, return all the Dominoes which may be played
      at the left End and all those that may be played at the right End. -}
  possPlays :: Hand -> Board -> ([Domino], [Domino])
  left_poss = []
  right_poss = []

  possPlays [] _ = (left_poss, right_poss)
  possPlays (h : t) board
    | goesP h L board && goesP h R board = let (left_poss, right_poss) = possPlays t board in (left_poss ++ [h], right_poss ++ [h])
    | goesP h L board = let (left_poss, right_poss) = possPlays t board in (left_poss ++ [h], right_poss)
    | goesP h R board = let (left_poss, right_poss) = possPlays t board in (left_poss, right_poss ++ [h])
    | otherwise = possPlays t board

  {- given a Domino, a Board and an End, play the domino at the given end if
      it will go, returning the new Board. -}
  playDom :: Domino -> End -> Board -> Maybe Board
  playDom domino _ [] = Just [domino]
  playDom domino end board
    | end == L && goesP domino L board = Just ((checkFlip domino L board) : board)
    | end == R && goesP domino R board = Just (board ++ [checkFlip domino R board])
    | otherwise = Nothing

  -- checks if a domino needs flipping
  checkFlip :: Domino -> End -> Board -> Domino
  checkFlip domino end board
    | goesP domino L board && second_item_domino == first_item_board = domino
    | goesP domino R board && first_item_domino == second_item_board = domino
    | goesP domino L board && second_item_domino /= first_item_board = flipDom domino
    | goesP domino R board && first_item_domino /= second_item_board = flipDom domino
    | otherwise = domino
    where
      first_item_domino = fst domino
      second_item_domino = snd domino
      first_item_board = fst (head board)
      second_item_board = snd (last board)

  -- flips the domino
  flipDom :: Domino -> Domino
  flipDom (x, y) = (y, x)

  -- checks if the board contains a double domino on either side
  isDoubleDom :: Board -> End -> Bool
  isDoubleDom board L
    | fst (head board) == snd (head board) = True
    | otherwise = False

  isDoubleDom board R
    | fst (last board) == snd (last board) = True
    | otherwise = False

  -- finds sum of double
  sumOfDouble :: Board -> Int
  sumOfDouble board
    | isDoubleDom board L && isDoubleDom board R = fst (head board) + snd (head board) + fst (last board) + snd (last board)
    | isDoubleDom board L = fst (head board) + snd (head board) + snd (last board)
    | isDoubleDom board R = fst (last board) + snd (last board) + fst (head board)
    | otherwise = 0

  -- checks if the total is divisible by 3
  divisibleBy3 :: Board -> Bool
  divisibleBy3 board
    | isDoubleDom board L && rem (sumOfDouble board) 3 == 0 = True
    | isDoubleDom board R && rem (sumOfDouble board) 3 == 0 = True
    | rem total 3 == 0 = True
    | otherwise = False
    where total = fst (head board) + snd (last board)

  -- checks if divisible by 5
  divisibleBy5 :: Board -> Bool
  divisibleBy5 board
    | isDoubleDom board L && rem (sumOfDouble board) 5 == 0 = True
    | isDoubleDom board R && rem (sumOfDouble board) 5 == 0 = True
    | rem total 5 == 0 = True
    | otherwise = False
    where total = fst (head board) + snd (last board)

  {-add up the score on the right and the left side of the board and return
      as an integer-}
  scoreBoard :: Board -> Int
  scoreBoard [] = 0
  scoreBoard board
    | total == 15 = 8
    | head board == last board && divisibleBy3 board = quot (fst (head board) + snd (head board)) 3
    | head board == last board && divisibleBy5 board = quot (fst (head board) + snd (head board)) 5
    | divisibleBy3 board && divisibleBy5 board = quot (sumOfDouble board) 3 + quot (sumOfDouble board) 5
    | isDoubleDom board L && divisibleBy3 board = quot (sumOfDouble board) 3
    | isDoubleDom board R && divisibleBy3 board = quot (sumOfDouble board) 3
    | isDoubleDom board L && divisibleBy5 board = quot (sumOfDouble board) 5
    | isDoubleDom board R && divisibleBy5 board = quot (sumOfDouble board) 5
    | rem total 3 == 0 && rem total 5 == 0 = quot total 3 + quot total 5
    | rem total 3 == 0 = quot total 3
    | rem total 5 == 0 = quot total 5
    | otherwise = 0
    where
      total = (fst (head board)) + (snd (last board))

  -- the full set of Doms
  domSet :: [Domino]

  domSet = [(6,6),(6,5),(6,4),(6,3),(6,2),(6,1),(6,0),
                  (5,5),(5,4),(5,3),(5,2),(5,1),(5,0),
                        (4,4),(4,3),(4,2),(4,1),(4,0),
                              (3,3),(3,2),(3,1),(3,0),
                                    (2,2),(2,1),(2,0),
                                          (1,1),(1,0),
                                                (0,0)]
  {-Given a Board and an Int n, return all the Dominoes not already played
      which would give a 5s-and-3s score of n and the End at which to play
      each one-}
  scoreN :: Board -> Int -> ([Domino],[Domino])

  scoreN board int =
    (filter (\n -> scoreBoard (resMaybe (playDom n L board)) == int ) (doms_left_side_list),
      filter (\n -> scoreBoard (resMaybe (playDom n R board)) == int) (doms_right_side_list))
      where
        doms_left_side_list = fst (possPlays not_played board)
        doms_right_side_list = snd (possPlays not_played board)
        not_played = filter (\n -> not (playedP n board)) (domSet)

  -- predicate testing for success
  isJust :: (Maybe a) -> Bool
  isJust (Just _) = True
  isJust Nothing = False

  -- extract the result from a successful maybe
  resMaybe :: (Maybe a) -> a
  resMaybe (Just x) = x
