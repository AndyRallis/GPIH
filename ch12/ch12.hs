type FirstName = String
type LastName = String
type Age = Int
type Height = Int

patientInfo :: FirstName -> LastName -> Age -> Height -> String
patientInfo fname lname age height = name ++ " " ++ ageHeight
 where
  name      = lname ++ ", " ++ fname
  ageHeight = "(" ++ show age ++ " yrs. " ++ show height ++ "in.)"

type PatientName = (String, String)

firstName :: PatientName -> String
firstName (fname, _) = fname

lastName :: PatientName -> String
lastName (_, lname) = lname

patientInfo2 :: PatientName -> Age -> Height -> String
patientInfo2 patientName age height = name ++ " " ++ ageHeight
 where
  name      = firstName patientName ++ ", " ++ lastName patientName
  ageHeight = "(" ++ show age ++ " yrs. " ++ show height ++ "in.)"

data Sex = Male | Female

sexInitial :: Sex -> String
sexInitial Male   = "M"
sexInitial Female = "F"

data RhType = Pos | Neg
data ABOType = A | B | AB | O

data BloodType = BloodType ABOType RhType


showRh :: RhType -> String
showRh Pos = "+"
showRh Neg = "-"

showABO :: ABOType -> String
showABO A  = "A"
showABO B  = "B"
showABO AB = "AB"
showABO O  = "O"

showBloodType :: BloodType -> String
showBloodType (BloodType abo rh) = showABO abo ++ showRh rh

canDonateTo :: BloodType -> BloodType -> Bool
canDonateTo (BloodType O _) _                = True
canDonateTo _               (BloodType AB _) = True
canDonateTo (BloodType A _) (BloodType A  _) = True
canDonateTo (BloodType B _) (BloodType B  _) = True
canDonateTo _               _                = False

type MiddleName = String
data Name = Name FirstName LastName | NameWithMiddle FirstName MiddleName LastName

showName :: Name -> String
showName (Name f l            ) = f ++ " " ++ l
showName (NameWithMiddle f m l) = f ++ " " ++ m ++ " " ++ l

data Patient = Patient Name Sex Int Int Int BloodType

johnDoe :: Patient
johnDoe = Patient (Name "John" "Doe") Male 30 74 200 (BloodType AB Pos)

janeSmith :: Patient
janeSmith = Patient (NameWithMiddle "Jane" "Liz" "Smith")
                    Female
                    22
                    62
                    120
                    (BloodType O Neg)

getName :: Patient -> Name
getName (Patient n _ _ _ _ _) = n

getAge :: Patient -> Int
getAge (Patient _ _ a _ _ _) = a

getBloodType :: Patient -> BloodType
getBloodType (Patient _ _ _ _ _ bt) = bt


data Patient2 = Patient2 { name :: Name
                        , sex :: Sex
                        , age :: Int
                        , height :: Int
                        , weight :: Int
                        , bloodType :: BloodType
                        }

jackieSmith :: Patient2
jackieSmith = Patient2 { name      = Name "Jackie" "Smith"
                       , age       = 43
                       , sex       = Female
                       , height    = 62
                       , weight    = 115
                       , bloodType = BloodType O Neg
                       }

jackieSmithUpdate = jackieSmith { age = 44 }

--Q12.1

canDonatePatient :: Patient -> Patient -> Bool
canDonatePatient (Patient _ _ _ _ _ bloodType1) (Patient _ _ _ _ _ bloodType2)
  = canDonateTo bloodType1 bloodType2

--Q12.2
patientSummary :: Patient2 -> IO ()
patientSummary pt =
  putStr
    $  br
    ++ cr
    ++ "Patient Name: "
    ++ nm
    ++ cr
    ++ "Sex: "
    ++ sx
    ++ cr
    ++ "Age: "
    ++ ag
    ++ cr
    ++ "Height: "
    ++ hght
    ++ " in."
    ++ cr
    ++ "Weight: "
    ++ wght
    ++ " lbs."
    ++ cr
    ++ "Blood Type: "
    ++ bldType
    ++ cr
    ++ br
    ++ cr
 where
  nm      = showName $ name pt
  sx      = sexInitial $ sex pt
  ag      = show $ age pt
  hght    = show $ height pt
  wght    = show $ weight pt
  bldType = showBloodType $ bloodType pt
  cr      = "\n"
  br      = replicate 30 '*'
