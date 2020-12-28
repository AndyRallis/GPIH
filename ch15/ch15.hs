data FourLetterAlphabet = L1 | L2 | L3 | L4 deriving (Show, Enum, Bounded)
data ThreeLetterAlphabet = Alpha | Beta | Kappa deriving (Show, Enum, Bounded)

rotN :: (Bounded a, Enum a) => Int -> a -> a
rotN alphabetSize c = toEnum rotatedValue
 where
  halfAlphabet = alphabetSize `div` 2
  offset       = fromEnum c + halfAlphabet
  rotatedValue = offset `mod` alphabetSize

rotNDecoder :: (Bounded a, Enum a) => Int -> a -> a
rotNDecoder n c = toEnum rotatedValue
 where
  halfAlphabet = n `div` 2
  offset =
    if even n then fromEnum c + halfAlphabet else 1 + fromEnum c + halfAlphabet
  rotatedValue = offset `mod` n


fourLetterEncoder :: [FourLetterAlphabet] -> [FourLetterAlphabet]
fourLetterEncoder = map rot41
  where rot41 = rotN (1 + fromEnum (maxBound :: FourLetterAlphabet))

threeLetterEncoder :: [ThreeLetterAlphabet] -> [ThreeLetterAlphabet]
threeLetterEncoder = map rot31
  where rot31 = rotN (1 + fromEnum (maxBound :: ThreeLetterAlphabet))

threeLetterDecoder :: [ThreeLetterAlphabet] -> [ThreeLetterAlphabet]
threeLetterDecoder = map decode13
  where decode13 = rotNDecoder (1 + fromEnum (maxBound :: ThreeLetterAlphabet))


rotEncoder :: String -> String
rotEncoder = map rotCharEncoder
  where rotCharEncoder = rotN (1 + fromEnum (maxBound :: Char))

rotDecoder :: String -> String
rotDecoder = map rotCharDecoder
  where rotCharDecoder = rotNDecoder (1 + fromEnum (maxBound :: Char))

xorBool :: Bool -> Bool -> Bool
xorBool v1 v2 = (v1 || v2) && not (v1 && v2)

xorPair :: (Bool, Bool) -> Bool
xorPair (v1, v2) = xorBool v1 v2

xor :: [Bool] -> [Bool] -> [Bool]
xor l1 l2 = map xorPair (zip l1 l2)

type Bits = [Bool]

intToBits' :: Int -> Bits
intToBits' 0 = [False]
intToBits' 1 = [True]
intToBits' n = if remainder == 0
  then False : intToBits' nextVal
  else True : intToBits' nextVal
 where
  remainder = n `mod` 2
  nextVal   = n `div` 2

maxBits :: Int
maxBits = length (intToBits' maxBound)

intToBits :: Int -> Bits
intToBits n = leadingFalses ++ reversedBits
 where
  reversedBits  = reverse $ intToBits' n
  missingBits   = maxBits - length reversedBits
  leadingFalses = replicate missingBits False

charToBits :: Char -> Bits
charToBits = intToBits . fromEnum

bitsToInt :: Bits -> Int
bitsToInt bits = sum (map (\x -> 2 ^ (snd x)) trueLocations)
 where
  size          = length bits
  indices       = [size - 1, size - 2 .. 0]
  trueLocations = filter fst (zip bits indices)

bitsToChar :: Bits -> Char
bitsToChar bits = toEnum (bitsToInt bits)

myPad :: String
myPad = "Shhhhhh"

myPlainText :: String
myPlainText = "Haskell"

applyOTP' :: String -> String -> [Bits]
applyOTP' pad plainText = map (\pair -> (fst pair) `xor` (snd pair))
                              (zip padBits plainTextBits)
 where
  padBits       = map charToBits pad
  plainTextBits = map charToBits plainText

applyOTP :: String -> String -> String
applyOTP pad plainText = map bitsToChar encodedBits
  where encodedBits = applyOTP' pad plainText

class Cipher a where
  encode :: a -> String -> String
  decode :: a -> String -> String

data Rot = Rot

instance Cipher Rot where
  encode Rot text = rotEncoder text
  decode Rot text = rotDecoder text

data OneTimePad = OTP String

instance Cipher OneTimePad where
  encode (OTP pad) text = applyOTP pad text
  decode (OTP pad) text = applyOTP pad text

myOTP :: OneTimePad
myOTP = OTP (cycle [minBound .. maxBound])

prng :: Int -> Int -> Int -> Int -> Int
prng a b maxNumber seed = (a*seed + b) `mod` maxNumber

data StreamCipher = Generator (Int -> Int) Int

makePRNGPad :: (Int-> Int) -> Int -> String
makePRNGPad f seed = let n = f seed
                      in toEnum n : makePRNGPad f n

instance Cipher StreamCipher where
  encode (Generator f seed) text = applyOTP (makePRNGPad f seed) text
  decode (Generator f seed) text = applyOTP (makePRNGPad f seed) text

myGen :: StreamCipher
myGen = Generator (prng 1337 7 100) 1235
