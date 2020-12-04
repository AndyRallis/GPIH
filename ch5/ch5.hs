ifEven f x = if even x
             then f x
             else x

-- genIfXEven x = ifEven (\f -> ifEven f x)

getRequestURL host apiKey resource id = host ++
                                        "/" ++
                                        resource ++
                                        "/" ++
                                        id ++
                                        "?token=" ++
                                        apiKey

hostBuilder = getRequestURL "http://www.example.com"

genApiRequstBuilderId hostBuilder apiKey resource = (\id -> hostBuilder apiKey resource id)

exampleHostBuilderBook = hostBuilder "1337hAsk3ll" "book"

-- FROM CH4

sfOffice name = if lastName < "L"
                  then nameText
                    ++ " - PO Box 1234 - San Fran, CA, 94111"
                  else nameText
                    ++ " - PO Box 1010 - San Fran, CA, 94109"
  where lastName = snd name
        nameText = fst name ++ " " ++ lastName

nyOffice name = nameText ++ ": PO Box 789 - New York, NY, 10013"  
  where nameText = fst name ++ " " ++ snd name

renoOffice name = nameText ++ " - PO Box 456 - Reno, NV 89523" 
  where nameText = snd name

getLocationFunction location = case location of
  "ny" -> nyOffice
  "sf" -> sfOffice
  "reno" -> renoOffice
  "dc" -> washingtonOffice
  _ -> \ name -> fst name ++ " " ++ snd name

addressLetter name location = locationFunction name
  where locationFunction = getLocationFunction location

washingtonOffice name = nameText ++ " Esq. Some Large Government Building DC"
  where nameText = fst name ++ " " ++ snd name

--back to CH5
flipBinaryArgs f = (\b a -> f a b)

addressLetterV2 = flipBinaryArgs addressLetter
addressLetterNY = addressLetterV2 "ny"

subtract2 = flip (-) 2 

--Q5.1
ifEvenInc = ifEven (+ 1)

ifEvenSquare = ifEven (^2)

ifEvenDouble = ifEven (*2)

--Q5.2
binaryPartialApplication f b = flip f b