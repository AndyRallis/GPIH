--Create cup object
cup floz = (\message -> message floz)

getOz aCup = aCup id

drink aCup oz = cup remainder
 where
  remainder = if diffOz < 0 then 0 else diffOz
  diffOz    = getOz aCup - oz

isEmpty aCup = getOz aCup == 0

--Robot fighters
robot (name, attack, hp) message = message (name, attack, hp)

name (n, _, _) = n
attack (_, a, _) = a
hp (_, _, h) = h

getName aRobot = aRobot name
getAttack aRobot = aRobot attack
getHP aRobot = aRobot hp
