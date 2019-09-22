
import Test.QuickCheck hiding (shuffle)
import Cards 
import RunGame

hand2 :: Hand
hand2 = [Card Queen Spades,Card Ace Spades,Card Ace Spades,Card Ace Spades]

hand3 :: Hand
hand3 = [Card Queen Spades,Card King Spades,Card Ace Spades,Card Ace Spades]





-- sizeSteps :: [Int]
-- sizeSteps = [ size hand2
--              , size (Card (Numeric 2) Hearts : (Card Jack Spades : []))
--              , 1 + size (Card Jack Spades : [] )
--              , 1 + 1 + size []
--              , 1 + 1 + 0
--              , 2
--              ]


myCard = Card (Numeric 9) Spades

displayCard:: Card -> String 
displayCard (Card rank suit) = show rank ++ " of " ++ show suit

display :: Hand -> String
display [] = []
display  [card] = displayCard card
display  (card:cards) = displayCard card ++ ", "++ display cards


valueRank :: Rank -> Int
valueRank (Numeric r) = r
valueRank Ace = 11
valueRank _ = 10

myRank = rank myCard

valueHand :: Hand -> Int
valueHand [] = 0
valueHand  [card] = valueRank (rank card)
valueHand (card:cards) =  valueRank (rank card) + valueHand cards


numberOfAces :: Hand -> Int
numberOfAces [] = 0
numberOfAces (card:cards)
    | rank card == Ace = 1 + numberOfAces cards
    | otherwise = 0 + numberOfAces cards


value :: Hand -> Int
value cards
    | valueHand cards > 21 = valueHand cards - numberOfAces cards * 10
    | otherwise = valueHand cards



gameOver :: Hand -> Bool
gameOver hand  = value hand > 21


winner :: Hand -> Hand -> Player 
winner hand1 hand2 
    | value hand1 > value hand2 && not (gameOver hand1) = Guest
    | gameOver hand2 && not(gameOver hand1) = Guest
    | value hand1 < value hand2 || gameOver hand1 = Bank  
    | value hand1 == value hand2 = Bank  



allPossibleRanks = [Numeric x| x<-[2..10]] ++ [Ace,Queen,Jack,King]
allPossibleSuits = [Hearts,Spades,Diamonds,Clubs]


fullDeck :: Deck
fullDeck = [ Card r s | s<-allPossibleSuits,r<-allPossibleRanks]

prop_size_fullDeck :: Bool
prop_size_fullDeck = size fullDeck == 52

draw :: Deck -> Hand -> (Deck, Hand)
draw [] hand = error "draw: The deck is empty."
draw deck hand = (drop 1 deck,take 1 deck ++ hand )


-- the bank plays. The bank draws cards until its score is 16 or higher, and then it stops.

-- The value of a hand (the score) is the sum of the values of the cards. The values are as follows:

-- To write this function you will probably need to introduce a helper function that takes the deck and the bank’s hand as input. To draw a card from the deck you can use where in the following way:


-- playBank :: Deck -> Hand
-- playBank


playBank :: Deck -> Hand
playBank deck = playBank' deck []

playBank'::Deck -> Hand -> Hand
playBank' deck bankHand
    | value bankHand' >= 16 = bankHand'
    | value bankHand' <= 16 = playBank' deck' bankHand'
    where (deck', bankHand') = draw deck bankHand


shuffle :: [Double] -> Deck -> [Double]
shuffle [float] cards  = [float]

