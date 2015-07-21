module MaritalNames (
	maritalNames,
	parsePerson
) where

data Person = Person {
	firstName :: String,
	lastName :: String,
	birthName :: String
} deriving (Show)

-- | creates person from array
personFromArray :: [String] -> Person
personFromArray [] = error "Illegal person definition EMPTY"
personFromArray [e] = error ("Illegal person defintion '" ++ e ++ "'")
personFromArray [fname, lname] = Person fname lname lname
personFromArray [fname, lname, _, bname] = Person fname lname bname
personFromArray e = error ("Illegal person definition '" ++ unwords e ++ "'")

-- | parses person from string
parsePerson :: String -> Person
parsePerson nameString = personFromArray $ words nameString

-- | returns instance of person with lastname as last name
withLastName :: Person -> String -> Person
withLastName p lastname = Person (firstName p) lastname (birthName p)

-- | returns combined name with accessor functions (TODO: refactor ordering)
combinedName :: (Person -> String) -> Person -> (Person -> String) -> Person -> String
combinedName accessorA pa accessorB pb = accessorA pa ++ "-" ++ accessorB pb

-- | tells if Person has still his birth name
isOriginalName :: Person -> Bool
isOriginalName person = birthName person == lastName person

-- | It's convenient to call this lib by strings so one don't have to parse names outside
maritalNamesFromString :: String -> String -> [(Person, Person)]
maritalNamesFromString a b = maritalNames (parsePerson a) (parsePerson b)

-- | creates array of Person tuples with all possible name combinations
maritalNames :: Person -> Person -> [(Person, Person)]
maritalNames personA personB = 
	-- TODO: make more "fluid"
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

-- | String representation of Person
tellPerson :: Person -> String
tellPerson p = 
	firstName p ++ 
	" " ++ 
	lastName p ++ 
	if isOriginalName p 
		then "" 
		else " os " ++ birthName p

-- | String representation of tuple of Person
tellCombination :: (Person, Person) -> String
tellCombination (p1, p2) = tellPerson p1 ++ " & " ++ tellPerson p2

-- | prints marital name combinations neatly
printMaritalNames :: [(Person, Person)] -> IO()
printMaritalNames res = mapM_ (\x -> print (tellCombination x)) res 





