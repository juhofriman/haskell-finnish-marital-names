module MaritalNames (
	maritalNamesFromString,
	maritalNames,
	parsePerson,
	printMaritalNames
) where

data Person = Person {
	-- First name of Person. I.e "Jarmo"
	firstName :: String,
	-- Current last-name of Person. I.e "Keimonen". 
	-- Can be combined name i.e "Huttunen-Keimonen" 
	lastName :: String,
	-- Birth name (faminy name) of Person.
	birthName :: String
} deriving (Show)

-- | creates person from array which supposedly is (words "Jack Penttinen")
-- | if last name is combined name birthname is parsed accordingly
personFromArray :: [String] -> Person
personFromArray [] = error "Illegal person definition EMPTY"
personFromArray [e] = error ("Illegal person defintion '" ++ e ++ "'")
personFromArray [fname, lname] = 
	if isCombinedName lname
		then Person fname lname (birthNameFromCombinedName lname)
		else Person fname lname lname
-- This is pretty horrible...
personFromArray [fname, lname, _, bname] = 
	if isCombinedName lname &&  birthNameFromCombinedName lname /= bname
		then error ("bname=" ++ bname ++ 
			    " but lname=" ++ lname ++ " NO DICE!")
	        else Person fname lname bname
personFromArray e = error ("Illegal person definition '" ++ unwords e ++ "'")

-- | parses person from string
parsePerson :: String -> Person
parsePerson nameString = personFromArray $ words nameString

-- | returns instance of person with new lastname last name
withLastName :: Person -> String -> Person
withLastName p lastname = Person (firstName p) lastname (birthName p)

-- | returns combined name with accessor functions (TODO: refactor ordering)
combinedName :: (Person -> String) -> Person -> (Person -> String) -> Person -> String
combinedName accessorA pa accessorB pb = accessorA pa ++ "-" ++ accessorB pb

-- | tells if Person has still his birth name
hasBirthName :: Person -> Bool
hasBirthName person = birthName person == lastName person

-- | tells if name is combined name
isCombinedName :: String -> Bool
isCombinedName = elem '-'

hasCombinedName :: Person -> Bool
hasCombinedName p = isCombinedName $ lastName p

-- | returns birth name part from combined name
birthNameFromCombinedName :: String -> String
birthNameFromCombinedName = takeWhile (/= '-')

-- | It's convenient to call this lib by strings so one don't have to parse names outside
maritalNamesFromString :: String -> String -> [(Person, Person)]
maritalNamesFromString a b = maritalNames (parsePerson a) (parsePerson b)

-- | creates array of Person tuples with all possible name combinations
maritalNames :: Person -> Person -> [(Person, Person)]
maritalNames personA personB = 
	[] -- seed for "pipe"
	-- Keep original names
	`pjoin` 	(personA, 
			 personB)
	-- Select common birth name of A
	`pjoin` 	(personA `withLastName` birthName personA, 
			 personB `withLastName` birthName personA)
	-- Select common birth name of B
	`pjoin`		(personA `withLastName` birthName personB,
			 personB `withLastName` birthName personB)
	-- Select common name of A but use combined name
	`pjoin`		(personA `withLastName` birthName personA,
			 personB `withLastName` 
			 	combinedName birthName personB birthName personA)
	-- Select common name of a but Can keep name from previous marriage 
	-- in combined name
	`pjoinCond`	(not (hasBirthName personB) &&
			 not (hasCombinedName personB),
			 personA `withLastName` birthName personA,
			 personB `withLastName`
				combinedName lastName personB birthName personA)
	-- Select common name of A but use combined name
 	`pjoin`     	(personA `withLastName`
                         	combinedName birthName personA birthName personB,
			 personB `withLastName` birthName personB)
        -- Select common name of B but can keep name from previous marriage 
	-- in combined name
        `pjoinCond`     (not (hasBirthName personA) &&
			 not (hasCombinedName personA),
                         personA `withLastName` 
				combinedName lastName personA birthName personB,
                         personB `withLastName` birthName personB)  

-- | Joins tuple of Persons to array of tuple of persons
-- note: these could be more generic! Theres no need to restrict to persons here!
-- but on the other hand, it's meant precicely for this
pjoin :: [(Person, Person)] -> (Person, Person) -> [(Person, Person)]
pjoin arr v = arr ++ [v]

-- | Joins tuple of Persons from triple (Person, Person, Bool) to array of
-- | tuple of Persons if boolean in triple is True
pjoinCond :: [(Person, Person)] -> (Bool, Person, Person) -> [(Person, Person)]
pjoinCond arr (cond, personA, personB) = 
	if cond then arr ++ [(personA, personB)] else arr

-- printing utilities

-- | String representation of Person
tellPerson :: Person -> String
tellPerson p = 
	firstName p ++ 
	" " ++ 
	lastName p ++ 
	if hasCombinedName p || hasBirthName p 
		then "" 
		else " os " ++ birthName p

-- | String representation of tuple of Person
tellCombination :: (Person, Person) -> String
tellCombination (p1, p2) = tellPerson p1 ++ " & " ++ tellPerson p2

-- | prints marital name combinations neatly
printMaritalNames :: [(Person, Person)] -> IO()
printMaritalNames = mapM_ (print . tellCombination)





