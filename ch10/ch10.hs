--Create cup object
cup floz = (\message -> message floz)

getOz aCup = aCup id

drink aCup oz = cup remainder
 where
  remainder = if diffOz < 0 then 0 else diffOz
  diffOz    = getOz aCup - oz

isEmpty aCup = getOz aCup == 0
