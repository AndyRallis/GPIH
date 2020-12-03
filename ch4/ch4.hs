ifEven myFunc x = if even x
                  then myFunc x
                  else x  


lambdaEvenCube y = ifEven (\x -> x^3) y

names = [("Ian", "Curtis"), ("Bernard","Sumner"), ("Peter", "Hook"), ("Stephen","Morris")]

compareLastNames name1 name2 = if lastName1 > lastName2
                               then GT
                               else if lastName1 < lastName2 
                                 then LT
                                 else 
                                  if firstName1 < firstName2
                                  then GT
                                  else if firstName1 > firstName2
                                  then LT
                                  else EQ      
                                  
  where lastName1 = snd name1
        lastName2 = snd name2
        firstName1 = fst name1
        firstName2 = fst name2


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

--Q4.1
compareLastNames2 name1 name2 = let lastComp = compare (snd name1) (snd name2)
                                in
                                  if lastComp /= EQ
                                    then lastComp
                                    else compare (fst name1) (fst name2)


--Q4.2
washingtonOffice name = nameText ++ " Esq. Some Large Government Building DC"
  where nameText = fst name ++ " " ++ snd name