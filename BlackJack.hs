
import Test.QuickCheck hiding (shuffle)
import Cards 
import RunGame

hand2 :: Hand
hand2 = [Card Queen Spades,Card Ace Spades,Card Ace Spades,Card Ace Spades]

hand3 :: Hand
hand3 = [Card Queen Spades,Card King Spades,Card Ace Spades,Card Ace Spades]





sizeSteps :: [Int]
sizeSteps = [ size hand2
             , size (Card (Numeric 2) Hearts : (Card Jack Spades : []))
             , 1 + size (Card Jack Spades : [] )
             , 1 + 1 + size []
             , 1 + 1 + 0
             , 2
             ]


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



-- B1

allPossibleRanks = [Numeric x| x<-[2..10]] ++ [Ace,Queen,Jack,King]
allPossibleSuits = [Hearts,Spades,Diamonds,Clubs]


fullDeck :: Deck
fullDeck = [ Card r s | s<-allPossibleSuits,r<-allPossibleRanks]

prop_size_fullDeck :: Bool
prop_size_fullDeck = size fullDeck == 52

-- B2
draw :: Deck -> Hand -> (Deck,Hand)
draw [] _ = error "draw: The deck is empty"
draw (card:cards) hand = (cards, card : hand)

-- B3

playBank :: Deck -> Hand
playBank deck = playBank' deck []

playBank'::Deck -> Hand -> Hand
playBank' deck bankHand
    | value bankHand' >= 16 = bankHand'
    | value bankHand' <= 16 = playBank' deck' bankHand'
    where (deck', bankHand') = draw deck bankHand



-- B4
shuffle :: [Double] -> Deck -> Deck
shuffle r deck = shuffle' r deck []

shuffle' :: [Double] -> Deck -> Deck -> Deck
shuffle' _      []   newDeck = newDeck
shuffle' (float:floats) deck newDeck = 
    shuffle' floats
             (removeCardInDeck (pos float deck) deck)
             ((getCardInDeck (pos float deck) deck) : newDeck)
  where
    pos float list = round (float * (fromIntegral(length list) - 1))


-- B5
belongsTo :: Card -> Deck -> Bool
card `belongsTo` []      = False
card `belongsTo` (card':decj) = card == card' || card `belongsTo` deck

prop_shuffle :: Card -> Deck -> Rand -> Bool
prop_shuffle card deck (Rand randomlist) =
    card `belongsTo` deck == card `belongsTo` shuffle randomlist deck

prop_size_shuffle :: Rand -> Deck -> Bool
prop_size_shuffle (Rand randomlist) deck = 
    length (shuffle randomlist deck) == length deck

getCardInDeck :: Int -> Deck -> Card
getCardInDeck n [] = error "getCardInDeck: List is empty"
getCardInDeck n list = list !! n

removeCardInDeck :: Int -> Deck -> Deck
removeCardInDeck _ [] = []
removeCardInDeck n (x:xs) | n == 0 = xs
                   | otherwise = x : removeCardInDeck (n-1) xs


-- B6
implementation = Interface
    {  iFullDeck  = fullDeck
    ,  iValue     = value
    ,  iDisplay   = display
    ,  iGameOver  = gameOver
    ,  iWinner    = winner
    ,  iDraw      = draw
    ,  iPlayBank  = playBank
    ,  iShuffle   = shuffle
    }
main :: IO ()
main = runGame implementation

