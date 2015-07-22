import MaritalNames
import System.Environment

main = do
    args <- getArgs
    let nameCombinations = 
	maritalNamesFromString (args !! 0) (args !! 1)
    -- this is from MaritalNames module, it should probably be defined here instead
    printMaritalNames nameCombinations
