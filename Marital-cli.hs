import MaritalNames
import System.Environment

main = do
    args <- getArgs
    let nameCombinations = maritalNamesFromString (args !! 0) (args !! 1)
    printMaritalNames nameCombinations
