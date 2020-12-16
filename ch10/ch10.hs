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

setName aRobot newName = aRobot (\(_, a, h) -> robot (newName, a, h))
setAttack aRobot newAttack = aRobot (\(n, _, h) -> robot (n, newAttack, h))
setHP aRobot newHP = aRobot (\(n, a, _) -> robot (n, a, newHP))

printRobot aRobot = aRobot
  (\(name, attack, hp) ->
    name ++ " attack: " ++ (show attack) ++ " hp: " ++ (show hp)
  )

damage aRobot attackDamage =
  aRobot (\(n, a, h) -> robot (n, a, h - attackDamage))

fight aRobot defender = damage defender attack
  where attack = if getHP aRobot > 10 then getAttack aRobot else 0

