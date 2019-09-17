
import Test.QuickCheck

import Cards 
import RunGame

hand2 :: Hand
hand2 = [Card Queen Spades,Card Ace Spades,Card Ace Spades,Card Ace Spades]




-- sizeSteps :: [Int]
-- sizeSteps = [ size hand2
--              , size (Card (Numeric 2) Hearts : (Card Jack Spades : []))
--              , 1 + size (Card Jack Spades : [] )
--              , 1 + 1 + size []
--              , 1 + 1 + 0
--              , 2
--              ]


myCard = Card (Numeric 10) Spades

displayCard:: Card -> String 
displayCard (Card rank suit) = show rank ++ " of " ++ show suit

display :: Hand -> String
display [] = []
display  [card] = displayCard card
display  (card:cards) = displayCard card ++ ", "++ display(cards) 

 

strToInt:: String -> Int
strToInt string = read string ::Int


getLastStr:: Rank -> String 
getLastStr r = last(words(show(r))) 

lastStrToInt r = strToInt (getLastStr r)

valueRank :: Rank -> Int
valueRank r
    | r == Queen = 10
    | r == Jack = 10
    | r == King = 10
    | r == Ace = 11
    -- | lastStrToInt r == 0 = 10
    | otherwise = lastStrToInt r

myRank = rank myCard

valueHand :: Hand -> Int
valueHand [] = 0
valueHand  [card] = valueRank (rank card)
valueHand (card:cards) 
    | rank card == Ace && ((numberOfAces cards) > 1 || 11 + valueHand(cards) > 21) =  1 + valueHand(cards)
    | otherwise =  valueRank (rank card) + valueHand(cards)


numberOfAces :: Hand -> Int
numberOfAces [] = 0
numberOfAces (card:cards)
    | rank card == Ace = 1 + numberOfAces(cards) 
    | otherwise = 0 + numberOfAces(cards) 


gameOver :: Hand -> Bool
gameOver hand  = valueHand hand > 21



-- player1 = Guest
-- player2 = Bank


winner :: Hand -> Hand -> Player 
winner hand1 hand2 
    | valueHand hand1 > valueHand hand2 && not (gameOver hand1) = Guest  
    | valueHand hand1 < valueHand hand2 || gameOver hand1 = Bank  
    | valueHand hand1 == valueHand hand2 = Bank  






            