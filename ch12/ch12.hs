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

sexInitial :: Sex -> Char
sexInitial Male   = 'M'
sexInitial Female = 'F'

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





