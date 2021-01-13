--data SportsCar = SportsCar Car Spoiler
 


data Artist = Person Name | Band String deriving Show

data Author = Author Name deriving Show

data Creator = AuthorCreator Author | ArtistCreator Artist deriving Show

type FirstName = String
type LastName = String
type MiddleName = String

data Name = Name FirstName LastName
    | NameWithMiddle FirstName MiddleName LastName
    | TwoInitialsWithLast Char Char LastName deriving Show

data Book = Book {
      author    :: Creator
    , isbn      :: String
    , bookTitle :: String
    , bookYear  :: Int
    , bookPrice :: Double
    }


data VinylRecord = VinylRecord {
artist :: Creator
, recordTitle :: String
, recordYear :: Int
, recordPrice :: Double
}
data CollectibleToy = CollectibleToy {
      name :: String
    , description :: String
    , toyPrice :: Double
    }

-- data StoreItem = BookItem Book
--   | RecordItem VinylRecord
--   | ToyItem CollectibleToy

madeBy :: StoreItem -> String
madeBy (BookItem book) = show $ author book
madeBy (RecordItem record) = show $ artist record
madeBy _ = "Not made by anyone. It's a real toy"


--Q16.1
data Pamphlet = Pamphlet {
      title :: String
    , pamphletDescription :: String
    , contact :: String
    , pamphletPrice :: Double
}

data StoreItem = BookItem Book
  | RecordItem VinylRecord
  | ToyItem CollectibleToy
  | PamphletItem Pamphlet


price :: StoreItem -> Double
price (BookItem book) = bookPrice book
price (RecordItem record) = recordPrice record
price (ToyItem toy) = toyPrice toy
price (PamphletItem _) = 0.0

--Q16.2

type Radius = Double
type Side = Double
type Height = Double
type Width = Double
data Shape = Circle Radius | Square Side | Rectangle Height Width

perimeter :: Shape -> Double
perimeter (Circle r) = 2 * pi * r
perimeter (Square s) = s * 4
perimeter (Rectangle h w) = 2 * (h + w)

area :: Shape -> Double
area (Circle r) = pi * (r ^ 2)
area (Square s) = s ^ 2
area (Rectangle h w) = h * w