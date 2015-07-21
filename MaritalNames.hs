module MaritalNames (
	maritalNames,
	parsePerson
) where

data Person = Person {
	firstName :: String,
	lastName :: String,
	birthName :: String
} deriving (Show)

personFromArray :: [String] -> Person
personFromArray [] = error "Illegal person definition"
personFromArray [_] = error "Illegal person defintion"
personFromArray [fname, lname] = Person fname lname lname
personFromArray [fname, lname, _, bname] = Person fname lname bname
personFromArray _ = error "Illegal person definition"

parsePerson :: String -> Person
parsePerson nameString = personFromArray $ words nameString

withLastName :: Person -> String -> Person
withLastName p lastname = Person (firstName p) lastname (birthName p)

combinedName :: (Person -> String) -> Person -> (Person -> String) -> Person -> String
combinedName accessorA pa accessorB pb = accessorA pa ++ "-" ++ accessorB pb

isOriginalName :: Person -> Bool
isOriginalName person = birthName person == lastName person

maritalNames :: Person -> Person -> [(Person, Person)]
maritalNames personA personB = 
	
	-- no change in names
	[(personA,
	  personB)]
	-- fallback on name
	++?
	(not $ isOriginalName personA,
	  (personA `withLastName` birthName personA,
	   personB))
	++?
        (not $ isOriginalName personB,
          (personA,
           personB `withLastName` birthName personB))
	++?
	((not $ isOriginalName personA) && (not $ isOriginalName personB),
	  (personA `withLastName`birthName personA,
           personB `withLastName` birthName personB))
	-- select birth name of A for both
	++
	[(personA `withLastName` birthName personA,
          personB `withLastName` birthName personA)]
	-- select last name of B for both
	++
	[(personA `withLastName` birthName personB,
	  personB `withLastName` birthName personB)]
	-- person A can choose combined name
	++
	[(personA `withLastName` combinedName birthName personA birthName personB,
	  personB `withLastName` birthName personB)]
	-- person A can keep name gotten from previous marriage in combined name
	++?
	(not $ isOriginalName personA,
	 (personA `withLastName`combinedName lastName personA birthName personB,
	  personB `withLastName` birthName personB))
	-- person B can choose combined name
	++
	[(personA `withLastName` birthName personA,
	  personB `withLastName` combinedName birthName personB birthName personA)]
	-- person B can keep name gotten from previous marriage in combined name
	++?
	(not $ isOriginalName personB,
	  (personA `withLastName` birthName personA,
	   personB `withLastName` combinedName lastName personB birthName personA))

-- utility function for adding value to array conditionally
-- [1,2,4] ++? (True, 5)  =>  [1,2,4,5]
(++?) :: [a] -> (Bool, a) -> [a]
arr ++? (b, x) = if b then arr ++ [x] else arr ++ []

-- printing utilities

tellPerson :: Person -> String
tellPerson p = firstName p ++ " " ++ lastName p

tellCombination :: (Person, Person) -> String
tellCombination (p1, p2) = tellPerson p1 ++ " & " ++ tellPerson p2

pprint :: [(Person, Person)] -> IO()
pprint res = mapM_ (\x -> print (tellCombination x)) res 





