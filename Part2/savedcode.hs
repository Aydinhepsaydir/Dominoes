{-**************************************

        dominoes.hs
        Written by Aydin Hepsaydir
        Date Started: 31/10/2017

**************************************-}

module DominoGame where
  import Dominoes
  import System.Random
  import MergeSort

  --datatype for a domino player
  type DomsPlayer = Hand -> Board -> (Domino, End)

  data Turn = P1 | P2
      deriving (Eq, Show)

  score :: Int
  score = 0

  simplePlayer :: DomsPlayer
  simplePlayer [] _ = ((0,0), R)
  simplePlayer (h : t) board
    | board == [] = (h, L)
    | goesP h L board = (h, L)
    | goesP h R board = (h, R)
    | otherwise = simplePlayer t board

  shuffleDoms :: Int -> [Domino]
  shuffleDoms n =
    let
      randomList = take 28 (randoms (mkStdGen n):: [Int])
      zipList = zip domSet randomList
      sortList = mergesort (\(_,n1) (_,n2)->n1<n2) zipList
      cypher = map fst sortList
    in cypher

  assignHands :: Int -> (Hand, Hand)
  assignHands n =
    (player_one_hand, player_two_hand)
    where
      player_one_hand = take 9 (shuffleDoms n)
      player_two_hand = take 9 (reverse (shuffleDoms n))

  isGameOver :: Hand -> Board -> Bool
  isGameOver [] _ = True
  isGameOver hand board
    | knockingP hand board = True
    | otherwise = False

  updateBoard :: DomsPlayer -> Board -> Hand -> Board
  updateBoard _ board [] = board
  updateBoard player board hand
    | goesP domino end board = resMaybe (playDom domino end board)
    | otherwise = board
    where
      domino = fst (player hand board)
      end = snd (player hand board)

  updateHand :: DomsPlayer -> Hand -> Board -> Hand
  updateHand player hand board =
    remove (fst (player hand board)) hand

  remove :: Domino -> Hand -> Hand
  remove _ [] = []
  remove domino (h:t)
    | domino == h = remove domino t
    | otherwise = (h:remove domino t)


  whoseTurn :: Turn -> Turn
  whoseTurn turn
    | turn == P1 = P2
    | otherwise = P1

  takeTurn :: DomsPlayer -> DomsPlayer -> Hand -> Hand -> Board -> Turn -> (Int, Int)
  takeTurn player1 player2 hand1 hand2 board turn
    | game_not_over1 && game_not_over2 && turn == P1 = let (p1_score, p2_score) = takeTurn player1 player2 new_hand1 hand2 (updateBoard player1 board hand1) P2 in (p1_score + scoreBoard (updateBoard player1 board hand1), p2_score)
    | game_not_over1 && game_not_over2 && turn == P2 = let (p1_score, p2_score) = takeTurn player1 player2 hand1 new_hand2 (updateBoard player2 board hand2) P1 in (p1_score, p2_score + scoreBoard (updateBoard player2 board hand2))
    | game_not_over1 && turn == P1 = let (p1_score, p2_score) = takeTurn player1 player2 new_hand1 hand2 (updateBoard player1 board hand1) P2 in (p1_score + scoreBoard (updateBoard player1 board hand1), p2_score)
    | game_not_over2 && turn == P2 = let (p1_score, p2_score) = takeTurn player1 player2 hand1 new_hand2 (updateBoard player2 board hand2) P1 in (p1_score, p2_score + scoreBoard (updateBoard player2 board hand2))
    | isGameOver hand1 board && isGameOver hand2 board = (0,0)
    | isGameOver hand1 board && turn == P1 = let (p1_score, p2_score) = takeTurn player1 player2 hand1 hand2 board P2 in (p1_score, p2_score)
    | isGameOver hand2 board && turn == P2 = let (p1_score, p2_score) = takeTurn player1 player2 hand1 hand2 board P1 in (p1_score, p2_score)
    | otherwise = (0, 0)
    where
      new_hand1 = updateHand player1 hand1 board
      new_hand2 = updateHand player2 hand2 board
      game_not_over1 = isGameOver hand1 board == False
      game_not_over2 = isGameOver hand2 board == False

  playDomsRound :: DomsPlayer -> DomsPlayer -> Int -> (Int, Int)
  playDomsRound player_one player_two n =
    takeTurn player_one player_two p1_hand p2_hand board P1
    where
      p1_hand = fst (assignHands n)
      p2_hand = snd (assignHands n)
      board = []
