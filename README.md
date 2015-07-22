# haskell-finnish-marital-names
It's simple program for generating possible marital name combinations according to finnish legislation. This uses finnish syntax for describing names which is as follows.

```
Person who still has birth name
FirstName Lastname
-> Haskell Curry

Person who took new last name (os is short for "omaa sukua" "own family")
FirstName CurrentLastName os BirthName
-> Ada Lovelace os Byron

Person who uses combined name
FirstName OwnLastNamePart-CommonLastNamePart
-> Ada Byron-Curry

Due to multiple marriages even this is possible
FirstName LastNamePart-AnotherLastNamePart os OriginalBirthName
-> Ada Lovelace-Curry os Byron
```


## Using ghci
Naturally you need ghc installed

Kick ghci in and load MaritalNames.hs
```
$ ghci
GHCi, version 7.8.3: http://www.haskell.org/ghc/  :? for help
Loading package ghc-prim ... linking ... done.
Loading package integer-gmp ... linking ... done.
Loading package base ... linking ... done.
Prelude> :load MaritalNames.hs
[1 of 1] Compiling MaritalNames     ( MaritalNames.hs, interpreted )
Ok, modules loaded: MaritalNames.
*MaritalNames>
```

Let's see what we have
```
*MaritalNames> :t maritalNamesFromString 
maritalNamesFromString :: String -> String -> [(Person, Person)]
*MaritalNames> :t printMaritalNames 
printMaritalNames :: [(Person, Person)] -> IO ()
```

Now you can query for combinations.
```
*MaritalNames> printMaritalNames $ maritalNamesFromString "Haskell Curry" "Ada Lovelace"
"Haskell Curry & Ada Lovelace"
"Haskell Curry & Ada Curry os Lovelace"
"Haskell Lovelace os Curry & Ada Lovelace"
"Haskell Curry & Ada Lovelace-Curry"
"Haskell Curry-Lovelace & Ada Lovelace"
```

If Ada had dumped William and got together with good ol' Curry this would be the case
```
*MaritalNames> printMaritalNames $ maritalNamesFromString "Haskell Curry" "Ada Lovelace os Byron"
"Haskell Curry & Ada Lovelace os Byron"
"Haskell Curry & Ada Curry os Byron"
"Haskell Byron os Curry & Ada Byron"
"Haskell Curry & Ada Byron-Curry"
"Haskell Curry & Ada Lovelace-Curry os Byron"
"Haskell Curry-Byron & Ada Byron"
```

## Compiling
Naturally you need ghc installed.

Compile:
```
$ ghc --make Marital-cli.hs
```

Now you can run cli with names as arguments
```
$ ./Marital-cli "Jim Haskell" "Mark Curry"
"Jim Haskell & Mark Curry"
"Jim Haskell & Mark Haskell os Curry"
"Jim Curry os Haskell & Mark Curry"
"Jim Haskell & Mark Curry-Haskell"
"Jim Haskell-Curry & Mark Curry"
```

**This piece of software supports equal marriage rights.**

![Just a rainbow flag](http://ecx.images-amazon.com/images/I/219aWO4ZH0L._SL500_SS115_.jpg)
