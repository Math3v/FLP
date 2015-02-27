import System.IO
import Data.Char
import Data.List

data NandRules = 
	NandRules
		{ nonterminals :: String
		, rules :: [Rule]
		} deriving Show

data Rule =
	Rule
		{ from :: String 
		, to :: String
		} deriving Show

--charToString
charToString :: Char -> String
charToString c = [c]

--consume->
consumeLeftArrow :: String -> String
consumeLeftArrow [] = []
consumeLeftArrow (x:xs) = 
	if (x == '-' || x == '>')
	then consumeLeftArrow xs
	else (x:xs)

--parseRule
parseRule :: String -> Rule
parseRule (x:xs) = Rule (charToString x) (consumeLeftArrow xs)

--tokenize
tokenize delim "" = []
tokenize delim (x:xs) = 
	if x == delim
	then tokenize delim xs
	else x : tokenize delim xs

--isSimple
isSimple :: Rule -> Bool
isSimple r = 
	if ((length (to r)) == 1) && (isLower ((to r) !! 0))
		then True
		else if ((length (to r)) == 2) && (isUpper ((to r) !! 0)) && (isUpper ((to r) !! 0))
			then True
			else False

--leftToTwoNonterminals
lttn :: String -> String
lttn (x:xs) = 
	if (length (x:xs)) == 1
		then x : '\'' : []
		else if (length (x:xs)) == 2
				then if isLower x && isLower (head xs)
					then (toUpper x) : '\'' : (toUpper (head xs)) : '\'' : []
					else if isLower x && isUpper (head xs)
						then (toUpper x) : '\'' : xs
						else if isLower (head xs)
							then x : (toUpper (head xs)) : '\'' : []
							else x:xs
				else if isLower x
					then (toUpper x) : '\'' : '<' : xs ++ ['>']
					else x : '<' : xs ++ ['>']

--readRules
readRules file = do
	eof <- hIsEOF file
	if eof
		then return ()
		else do
			line 	<- hGetLine file
			if isSimple (parseRule line)
				then putStrLn ("Simple rule: " ++ line)
				else putStrLn ("Not simple rule: " ++ line)
			readRules file

--
annt :: Rule -> Rule
annt r =
	if isSimple r
		then r
		else Rule (from r) (lttn (to r))

--addNewNonterminals
--annt [] = []
--annt (r:rs) = 
--	if isSimple r
--		then annt (rs)
--		else annt (rs ++ (Rule (from r) (lttn (to r))))


--read file
rf file = do
	f 				<- openFile file ReadMode
	nonterminals	<- hGetLine f
	terminals		<- hGetLine f
	starting		<- hGetLine f
	putStrLn ("Nonterminals: " ++ show (tokenize ',' nonterminals))
	putStrLn ("Terminals: " ++ show (tokenize ',' terminals))
	putStrLn ("Starting symbol: " ++ show starting)
	linesList		<- fmap lines (readFile file)
	putStrLn("Rules: " ++ show(drop 3 linesList))
	putStrLn (show (map (annt) (map (parseRule) (drop 3 linesList))))