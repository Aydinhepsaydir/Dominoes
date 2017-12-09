module Doms1_17 where

{- COM2001 2017-18
  Assignment 1: dominoes
  includes variants with mapping fns & comprehensions
  students don't need to use these to get full marks
  simple version first
 -}

 resMaybe :: Maybe a ->a
 resMaybe (Just x) = x


 -- type definitions
 -- several different types for [Dom], for clarity

 type Dom = (Int,Int)
 -- with highest pip first i.e. (6,1) not (1,6)

 -- the full set of Doms
 domSet :: [Dom]

 -- create list of all dominoes using list recursion
 domSet = [(i,j) | i <- [0..6],
                    j <- [i..6]]

 {- defining domSet with a comprehension

 domSet =  [(n,m)|n<-[0..6],m<-[0..6], m<=n]
 -}

 type Hand = [Dom] -- doms in player's hand

 type Board = [Dom] -- doms played: head of board is left end, last is right end. Order maintained e.g [(1,6),(6,6),(6,3), (3,1)]

 type DomList = [Dom] -- list of Doms which is neither a Hand or a Board

 data End = L|R -- left end or right end
            deriving (Eq,Show)

------------------------------------------------
{- VARIABLE NAMES
   b a board
   d a dom
   l left pip or left dom
   r right pip or right dom
-}

------------------------------------------------

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

 {- with filter
 domPresent d h = not (null (filter (\ d2->sameDomP d d2) h))
 -}

 hand1 :: Hand
 hand1 = [(3,3),(1,2),(3,5),(4,3),(0,0),(4,4)]
