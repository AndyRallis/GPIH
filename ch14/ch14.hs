data SixSidedDie = S1 | S2 | S3 | S4 | S5 | S6 deriving (Enum)
--deriving (Show)

instance Show SixSidedDie where
  show S1 = "I"
  show S2 = "II"
  show S3 = "III"
  show S4 = "IV"
  show S5 = "V"
  show S6 = "VI"

--instance Eq SixSidedDie where
--  (==) S6 S6 = True
--  (==) S5 S5 = True
--  (==) S4 S4 = True
--  (==) S3 S3 = True
--  (==) S2 S2 = True
--  (==) S1 S1 = True
--  (==) _  _  = False

--instance Enum SixSidedDie where
--  toEnum 0 = S1
--  toEnum 1 = S2
--  toEnum 2 = S3
--  toEnum 3 = S4
--  toEnum 4 = S5
--  toEnum 5 = S6
--  toEnum _ = error "No such value"
--
--  fromEnum S1 = 0
--  fromEnum S2 = 1
--  fromEnum S3 = 2
--  fromEnum S4 = 3
--  fromEnum S5 = 4
--  fromEnum S6 = 5

-- type Name = (String, String)
newtype Name = Name (String, String) deriving (Show, Eq)

names :: [Name]
names =
  [ Name ("Emil", "Cioran")
  , Name ("Eugene", "Thacker")
  , Name ("Friedrich", "Nietzsche")
  ]

instance Ord Name where
  compare (Name (f1, l1)) (Name (f2, l2)) = compare (l1, f1) (l2, f2)

--Q14.1
instance Ord SixSidedDie where
  compare s1 s2 = compare (fromEnum s1) (fromEnum s2)

instance Eq SixSidedDie where
  (==) s1 s2 = fromEnum s1 == fromEnum s2

--Q14.2
data FiveSidedDie = Side1 | Side2 | Side3 | Side4 | Side5 deriving (Eq, Enum, Show)

class (Eq a, Enum a) => Die a where
  roll :: Int -> a

instance Die FiveSidedDie where
  roll n = toEnum (n `mod` 5)
