
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


myCard = Card Jack Spades

displayCard:: Card -> String 
displayCard (Card rank suit) = show rank ++ " of " ++ show suit

display :: Hand -> String
display [] = []
display  [card] = displayCard card
display  (card:cards) = displayCard card ++ ", "++ display(cards) 


             
             
valueRank :: Rank -> Int
valueRank r
    | r == Queen = 10
    | r == Jack = 10
    | r == King = 10
    | r == Ace && numberOfAces hand2 == 1 = 11
    | r == Ace && numberOfAces hand2 > 1 = 1

    
valueHand :: Hand -> Int
valueHand [] = 0
valueHand  [card] = valueRank (rank card)
valueHand (card:cards) =  valueRank (rank card) + valueHand(cards)


numberOfAces :: Hand -> Int
numberOfAces [] = 0
numberOfAces (card:cards)
    | rank card == Ace = 1 + numberOfAces(cards) 
    | otherwise = 0 + numberOfAces(cards) 





            