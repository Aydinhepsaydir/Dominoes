{-**************************************

        Domes.hs
        Written by Aydin Hepsaydir
        Date Started: 31/10/2017

**************************************-}

module DominoGame where
  import System.Random
  import Debug.Trace

{--------------------------------------------------------------------------------
                          TYPE DECLARATIONS
---------------------------------------------------------------------------------}

  -- doms in player's hand
  type Hand = [Dom]

  -- doms played: head of board is left end, last is right end. Order maintained e.g [(1,6),(6,6),(6,3), (3,1)]
  type Board = [Dom]

  --datatype for a Dom player
  type DomsPlayer = Hand -> Board -> (Dom, End)

  -- list of Doms which is neither a Hand or a Board
  type DomList = [Dom]

  -- Domino datatype
  type Dom = (Int,Int)

  -- Player 1's turn or Player 2
  data Turn = P1 | P2
      deriving (Eq, Show)

  -- Left end or Right end
  data End = L|R
        deriving (Eq,Show)

  -- The full set of dominoes
  domSet :: [Dom]
  domSet = [(i,j) | i <- [0..6],
                     j <- [i..6]]

{-----------------------------------------------------------------------------------
                                PART TWO CODE
-----------------------------------------------------------------------------------}

  -- simplePlayer
  -- the player function, returns a dom and an end to play at
  simplePlayer :: DomsPlayer

  -- return ((0,0), R) if hand is empty
  -- never gets called as simplePlayer is only called if a dom can be played
  -- however, need to satisfy empty hand case for simplePlayer
  simplePlayer [] _ = ((0,0), R)

  simplePlayer (h : t) board
    | board == [] = (h, L)
    | goesP h L board = (h, L)
    | goesP h R board = (h, R)
    | otherwise = simplePlayer t board


  -- shuffleDoms
  -- takes list of all dominoes and randomises it
  shuffleDoms :: Int -> [Dom]

  shuffleDoms n =
    let
      randomList = take 28 (randoms (mkStdGen n):: [Int])
      zipList = zip domSet randomList
      sortList = mergesort (\(_,n1) (_,n2)->n1<n2) zipList
      cypher = map fst sortList
    in cypher


  -- assignHands
  -- uses shuffleDoms to allocate each player their hand
  assignHands :: Int -> (Hand, Hand)

  assignHands n =
    (player_one_hand, player_two_hand)
    where
      player_one_hand = take 9 (shuffleDoms n)
      player_two_hand = take 9 (reverse (shuffleDoms n))


  -- updateBoard
  -- updates the board when a move is made
  updateBoard :: DomsPlayer -> Board -> Hand -> Board

  updateBoard _ board [] = board

  updateBoard player board hand
    | goesP domino end board = resMaybe (playDom domino end board)
    | otherwise = board
    where
      domino = fst (player hand board)
      end = snd (player hand board)


  -- updateHand
  -- uses remove function to remove dom from hand
  updateHand :: DomsPlayer -> Hand -> Board -> Hand

  updateHand player hand board =
    remove (fst (player hand board)) hand


  -- remove
  -- used to remove domino from hand
  remove :: Dom -> Hand -> Hand

  remove _ [] = []

  remove domino (h:t)
    | domino == h = remove domino t
    | otherwise = (h:remove domino t)


  -- takeTurn
  -- uses other smaller helper functions to return score for each player
  takeTurn :: DomsPlayer -> DomsPlayer -> Hand -> Hand -> Board -> Turn -> (Int, Int)

  takeTurn player1 player2 hand1 hand2 board turn
    | not_knocking1 && not_knocking2 && turn == P1 = let (p1_score, p2_score) = traceShow ("Updated board: " ++ show new_board1) (takeTurn player1 player2 new_hand1 hand2 new_board1 P2) in (p1_score + (scoreBoard new_board1), p2_score)
    | not_knocking1 && not_knocking2 && turn == P2 = let (p1_score, p2_score) = traceShow ("Updated board: " ++ show new_board2) (takeTurn player1 player2 hand1 new_hand2 new_board2 P1) in (p1_score, p2_score + (scoreBoard new_board2))
    | not_knocking1 && turn == P1 = let (p1_score, p2_score) = traceShow ("Updated board: " ++ show new_board1) (takeTurn player1 player2 new_hand1 hand2 new_board1 P2) in (p1_score + (scoreBoard new_board1), p2_score)
    | not_knocking2 && turn == P2 = let (p1_score, p2_score) = traceShow ("Updated board: " ++ show new_board2) (takeTurn player1 player2 hand1 new_hand2 new_board2 P1) in (p1_score, p2_score + (scoreBoard new_board2))
    | knocking1 && knocking2 = (0,0)
    | knocking1 && turn == P1 = let (p1_score, p2_score) = traceShow ("Board: " ++ show board) (takeTurn player1 player2 hand1 hand2 board P2) in (p1_score, p2_score)
    | knocking2 && turn == P2 = let (p1_score, p2_score) = traceShow ("Board: " ++ show board) (takeTurn player1 player2 hand1 hand2 board P1) in (p1_score, p2_score)
    | otherwise = (0, 0)
    where
      new_hand1 = updateHand player1 hand1 board
      new_hand2 = updateHand player2 hand2 board
      not_knocking1 = knockingP hand1 board == False
      not_knocking2 = knockingP hand2 board == False
      knocking1 = knockingP hand1 board
      knocking2 = knockingP hand2 board
      new_board1 = updateBoard player1 board hand1
      new_board2 = updateBoard player2 board hand2

---------------------------------------------------------------------------------

  -- playDomsRound
  -- calls helper function to return scpre for either player in tuple form
  playDomsRound :: DomsPlayer -> DomsPlayer -> Int -> (Int, Int)

  playDomsRound player_one player_two n =
    takeTurn player_one player_two p1_hand p2_hand board P1
    where
      p1_hand = fst (assignHands n)
      p2_hand = snd (assignHands n)
      board = []


{----------------------------------------------------------------------------------
                  PART ONE CODE (COPY AND PASTED FROM Doms1_17.hs)
----------------------------------------------------------------------------------}


  -- extract ends from a board
  -- assumes at least 1 Dom played

  getEnds :: Board->(Dom,Dom)
  getEnds b = (head b,last b)

  ----------------------------------------------------------------------------------
  -- goesP
  -- can a given dom be played at a given end of a given board?
  -- separate fns for L & R

  goesP :: Dom->End->Board->Bool

  goesP d _ [] = True

  goesP d L b= goesLP d b
  goesP d R b= goesRP d b


  -- goesLP & goesRP

  -- predicate - will given domino go at left?


  goesLP :: Dom->Board->Bool

  goesLP _ [] = True

  goesLP (d1,d2) b = (l==d1)||(l==d2)
                     where ((l,_),_) = getEnds b -- extract left end from board

  -- will dom go to the right?


  goesRP :: Dom->Board->Bool

  goesRP _ [] = True

  goesRP (d1,d2) b = (r==d1)||(r==d2)
                   where (_,(_,r)) = getEnds b

  -------------------------------------------------------------
  -- knockingP
  -- True if no dom in a hand will go either left or right
  -- uses possPlays - true if that finds nothing to go either l or r
  knockingP :: Hand->Board->Bool

  knockingP h b = (null gl)&& (null gr)
                  where (gl,gr)=possPlays h b

  -----------------------------------------------------------------
  -- sameDomP
  -- are 2 doms the same .. allowing for reverse order?

  sameDomP :: Dom->Dom->Bool
  sameDomP(l1,r1) (l2,r2)
   |l1==l2 = r1==r2
   |l1==r2 = r1==l2
   |otherwise = False

  ----------------------------------------------------------
  -- playedP
  -- has a dom been played?


  playedP :: Dom->Board->Bool

  playedP _ [] = False

  playedP d (h:t)
   |sameDomP d h = True
   |otherwise = playedP d t


  {- with filter
  playedP d b = not (null (filter (\x->sameDomP x d) b))

  -- with currying

  playedP d b = not (null (filter (sameDomP d) b))

  -}


  -----------------------------------------------------------
  -- possPlays
  -- possible drops
  -- given hand and board, return all possible plays as pair
  -- left plays, right plays

  possPlays :: Hand->Board->(DomList,DomList)

  possPlays h b = (leftdrops h b, rightdrops h b)

  -- doms which will go left
  leftdrops :: Hand->Board->DomList

  leftdrops [] _ = []
  leftdrops (h:t) b
    |goesLP h b = h:leftdrops t b
    |otherwise = leftdrops t b

  {- with a filter
  leftdrops h b = filter (\d -> goesLP d b) h
  -}



  -- doms which go right
  rightdrops :: Hand->Board->Hand

  rightdrops [] _ = []
  rightdrops (h:t) b
    |goesRP h b = h:rightdrops t b
    |otherwise = rightdrops t b


  {- with a filter
  rightdrops h b = filter (\d -> goesRP d b) h
  -}


  ------------------------------------------------------------
  -- playDom
  -- given player plays
  -- play a dom at left or right, if it will go


  playDom :: Dom->End->Board->Maybe Board

  playDom d L b
    |goesLP d b = Just (playLeft d b)
    |otherwise = Nothing

  playDom d R b
    |goesRP d b = Just (playRight d b)
    |otherwise = Nothing

  -- play to left - it will go
  playLeft :: Dom->Board->Board

  playLeft d [] = [d]

  playLeft (d1,d2) b
   |d1==l1 = (d2,d1):b
   |otherwise = (d1,d2):b
   where
     ((l1,l2),_)= getEnds b


  -- play to right
  playRight :: Dom->Board->Board

  playRight d [] = [d]

  playRight (d1,d2) b
   |d1==r2 =  b++ [(d1,d2)]
   |otherwise = b++[(d2,d1)]
   where
    (_,(r1,r2))=getEnds b

  -----------------------------------------------------
  -- scoreBoard

  -- 5s & threes score for a board

  scoreBoard :: Board -> Int

  scoreBoard [] = 0
  scoreBoard [(d1,d2)] = score53 (d1+d2)

  scoreBoard b =
   let
    (lend,rend)=getEnds b
   in
    score53 ((domScore lend L)+ (domScore rend R))


  -- allow for doubles

  domScore :: Dom->End->Int

  domScore (l,r) e
   |l==r = 2*l
   |e == L = l
   |otherwise = r


  -------------------------------------------------
  -- 5s and 3s score for a number

  score53 :: Int->Int
  score53 n
   |n==3 = 1
   |n==5 = 1
   |n==6 = 2
   |n==9 = 3
   |n==10 = 2
   |n==12 = 4
   |n ==15 = 8
   |n==18= 6
   |n==20 = 4
   |otherwise = 0


  ------------------------------------------------
  -- scoreN
  -- all doms not yet played which will score n, and end to play them

  scoreN :: Board->Int->[(Dom,End)]

  scoreN b n =
   let
    remdoms = domsNotPlayed b -- all the doms not yet played
    (lplays,rplays)=possPlays remdoms b -- the ones which will go at right & left
    lposs = leftScoreN lplays n b -- doms scoring n at left.. returns [(Dom,L)]
    rposs = rightScoreN rplays n b -- doms scoring n at right .. returns [(Dom,R)]
   in
    lposs++rposs -- concatenate L & R


  {- with mapping fns
  scoreN b n =
   let
    remdoms = filter (\d->not (domPresent d b)) domSet     -- all the doms not yet played
    (lplays,rplays)=possPlays remdoms b -- the ones which will go at right & left
    lscores = map (\ d->scoreBoard (playLeft d b)) lplays -- scores for the left ones
    lposs =   map (\ (d,_)->(d,L)) (filter (\ (_,s)->(s==n))(zip lplays lscores)) -- zip the doms & their scores, filter ones with score n, extract doms
    rscores = map (\ d->scoreBoard (playRight d b)) rplays -- ditto for right end
    rposs =   map (\ (d,_)->(d,R)) (filter (\ (_,s)->(s==n))(zip rplays rscores))
   in
    lposs++rposs -- concatenate L & R
  -}

  -- with comprehensions
  {-
  scoreN b n =
   let
    remdoms = [d|d<-domSet, not (domPresent d b)]
    (lplays,rplays)=possPlays remdoms b
    lscores = [scoreBoard (playLeft d b)|d<-lplays]
    lposs = [(d,L)|(d,s)<-(zip lplays lscores), s==n]
    rscores = [scoreBoard (playRight d b)|d<-rplays]
    rposs = [(d,L)|(d,s)<-(zip rplays rscores), s==n]
   in
    lposs++rposs -- concatenate L & R
  -}




  -- find remaining doms.. not on board
  -- is each dom presnt in given board?

  domsNotPlayed :: Board->Hand -- have this return a Hand because we need to give result to possPlays

  domsNotPlayed b = domsNotPlayedA domSet b

  domsNotPlayedA :: DomList->Board->Hand

  domsNotPlayedA [] _ = []

  domsNotPlayedA (h:t) b
   |domPresent h b = domsNotPlayedA t b
   |otherwise = h: (domsNotPlayedA t b)

  {- with a filter

  domsNotPlayed b = filter (\d->not (domPresent d b)) domSet

  -}

  -- find doms which score n at left

  leftScoreN :: DomList->Int->Board->[(Dom,End)]

  leftScoreN [] _ _ = []

  leftScoreN (h:t) n b
   |scoreBoard (playLeft h b) == n = (h,L):leftScoreN t n b
   |otherwise = leftScoreN t n b

  -- find doms which score n at right

  rightScoreN :: DomList->Int->Board->[(Dom,End)]

  rightScoreN [] _ _ = []

  rightScoreN (h:t) n b
   |scoreBoard (playRight h b) == n = (h,R):rightScoreN t n b
   |otherwise = rightScoreN t n b

  -- domPresent
  -- is a domino in a hand?

  domPresent :: Dom->Hand->Bool

  domPresent d [] = False

  domPresent d (f:r)
   |sameDomP d f = True
   |otherwise = domPresent d r

  resMaybe :: Maybe a ->a
  resMaybe (Just x) = x

{---------------------------------------------------------------------------------
                  MERGESORT CODE (COPY AND PASTED FROM MergeSort.hs)
---------------------------------------------------------------------------------}

  --merge

  merge :: Ord a=> (a->a -> Bool)->[a]->[a] -> [a]

  merge _ [] lis2 = lis2

  merge _ lis1 [] = lis1
  merge compfn lis1 lis2
    | compfn h1 h2 = (h1:merge compfn t1 lis2)
    | otherwise = (h2:merge compfn lis1 t2)
    where
      (h1:t1)=lis1
      (h2:t2)=lis2
  -------------------------------------------------
  --mergesort

  mergesort :: Ord a=> (a->a -> Bool)->[a] -> [a]

  mergesort _ [] = [] --check this once only
  mergesort compfn dlis =
              mergesortA compfn (map (\ e -> [e]) dlis) -- give aux fn list of lists length 1
  -------------------------------------------------
  --mergsortA
  mergesortA :: Ord a=> (a->a -> Bool)->[[a]] -> [a]

  mergesortA _ [lis] = lis -- one list only, it's the answer
  -- general case - merge list pairs & repeat
  mergesortA compfn mlis= mergesortA compfn (mergesortpass compfn mlis)
  ---------------------------------------------------------------------
  --mergesortpass
  -- merge pairs of lists
  mergesortpass :: Ord a=> (a->a -> Bool)->[[a]] -> [[a]]

  mergesortpass _ [] = []
  mergesortpass _ [l]= [l] -- one element only, return list unchanged

  -- general case - merge first two lists, cons to remainder

  mergesortpass compfn (lis1:(lis2:rest)) =(merge compfn lis1 lis2): mergesortpass compfn rest
