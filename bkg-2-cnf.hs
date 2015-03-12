import System.IO
import Data.Char
import Data.List.Split
import Debug.Trace

data Rule =
	Rule
		{ from :: String 
		, to :: String
		}

data CFG = CFG
	{ nonterminals :: [String]
	, terminals :: String
	, starting :: Char
	, rules :: [Rule]
	}

instance Show CFG where
	show (CFG ns ts s rs) = (showNs ns []) ++ "\n" ++ (showTs ts []) ++ "\n" ++ (s:[]) ++ "\n" ++ (showRules rs []) ++ "\n"

instance Show Rule where
	show (Rule from to) = "Rule " ++ from ++ "->" ++ to ++ "\n"

printAll xs = if null xs        -- If the list is empty
    then return ()              -- then we're done, so we quit.
    else do print (head xs)     -- Otherwise, print the first element
            printAll (tail xs)  -- then print all the other elements.

showAll xs = if null xs        -- If the list is empty
    then return ()              -- then we're done, so we quit.
    else do show (head xs)     -- Otherwise, print the first element
            showAll (tail xs)  -- then print all the other elements.

showRules :: [Rule] -> String -> String
showRules [] acc = init acc
showRules (r:rs) acc = showRules rs (show r ++ acc)

showNs :: [String] -> String -> String
showNs [] acc = init acc
showNs (n:ns) acc = showNs ns (n ++ "," ++ acc)

showTs :: String -> String -> String
showTs [] acc = init acc
showTs (t:ts) acc = showTs ts (t : ',' : acc)

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
		else if ((length (to r)) == 2) && (isUpper ((to r) !! 0)) && (isUpper ((to r) !! 1))
			then True
			else False

--right side of tule to two Nonterminals
rttn :: String -> String
rttn (x:xs) = 
	if (length (x:xs)) == 1
		then x : '\'' : []
		else if (length (x:xs)) == 2
				then if isLower x && isLower (head xs)
					then (toUpper x) : '\'' : (toUpper (head xs)) : '\'' : []
					else if isLower x && isUpper (head xs)
						then (toUpper x) : '\'' : xs
						else if isLower (head xs)
							then x : (toUpper (head xs)) : '\'' : []
							else x:xs ++ "\'"
				else if isLower x
					then (toUpper x) : '\'' : '<' : xs ++ ['>']
					else x : '<' : xs ++ ['>']

--wrapper which gets rule and generate rule
--with two Ns on the right side
annt :: Rule -> Rule
annt r =
	if isSimple r
		then r
		else Rule (from r) (rttn (to r))

--find char in string
findInString :: Int -> Char -> String -> Int
findInString cnt c [] = -1
findInString cnt c (x:xs) = 
	if c == x
		then cnt
		else findInString (cnt + 1) c xs

--get right side and return new nonterminal
getLeft str =  snd (splitAt (findInString 0 '<' str) str)
getRight str = fst (splitAt (findInString 1 '>' str) str)
getNewN rule = (getLeft . getRight) (to rule)

--return the right beginning
returnNonterminal :: Char -> String
returnNonterminal c = 
	if (isUpper c)
		then (c : [])
		else ((toUpper c) : "'")

--two symbols to two nonterminals
twoSymbolsToTwoNonterminal :: String -> String
twoSymbolsToTwoNonterminal (z:x:y:ys) = (returnNonterminal x) ++ (returnNonterminal y) ++ []

--this has to be implemented separately for length of 2
--get new nonterminal and return rule
newNToRule :: String -> Rule
--newNToRule a | trace ("newNToRule " ++ show a) False = undefined
newNToRule (x:y:ys) = 
	if ((length (x:y:ys)) == 4) --4 = (2 + '<' and '>')
		then
			Rule (x:y:ys) (twoSymbolsToTwoNonterminal (x:y:ys))
		else 
			Rule (x:y:ys) ((returnNonterminal y) ++ "<" ++ (ys))

--newNToListOfRules
newNToRs :: String -> [Rule] -> [Rule]
--newNToRs a b | trace ("newNToRs " ++ show a ++ " " ++ show b) False = undefined
newNToRs [] [] = error "Nothing to parse"
newNToRs [] x = error "Nothing to parse"
newNToRs (s1:s2:ss) [] = 
	if (length (s1:s2:ss)) == 4 -- '<' + 2 + '>'
		then
			((newNToRule (s1:s2:ss)) : [])
		else
			newNToRs ('<' : ss) ((newNToRule (s1:s2:ss)) : [])
newNToRs (s1:s2:ss) (x:xs) = 
	if (length (s1:s2:ss)) == 4 -- '<' + 2 + '>'
		then
			((x:xs) ++ ((newNToRule (s1:s2:ss)) : []))
		else
			newNToRs ('<' : ss) ((newNToRule (s1:s2:ss)) : (x:xs))

--array of new nonterminals to array of new rules
newNsToRs :: [String] -> [Rule] -> [Rule]
newNsToRs (n:ns) [] = newNsToRs (ns) (newNToRs (n) [])	--first call
newNsToRs [] (x:xs) = (x:xs) --last call
newNsToRs (n:ns) (r:rs) = newNsToRs (ns) (newNToRs (n) (r:rs))

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

beginWithL :: String -> Bool
beginWithL (x:_) = 
	if x == '<'
		then True
		else False
beginWithL _ = False

--get nonterminals ending with ' from 1 rule
getSpecialFromR :: String -> [String] -> [String]
--getSpecialFromR a acc | trace ("getSpecialFromR " ++ a) False = undefined
getSpecialFromR [] acc = acc
getSpecialFromR (e:s:ss) acc = 
	if (s == '\'')
		then getSpecialFromR (e:ss) ((e:"\'") : acc)
		else getSpecialFromR (s:ss) acc
getSpecialFromR s acc = acc

--rules to list of nonterminals ending with '
getSpecialNs :: [Rule] -> [String] -> [String]
--getSpecialNs a acc | trace ("getSpecialNs " ++ show acc) False = undefined
getSpecialNs [] acc = acc
getSpecialNs (r:rs) [] =  getSpecialNs rs ((getSpecialFromR (to r) []) ++ [])
getSpecialNs (r:rs) acc = getSpecialNs rs ((getSpecialFromR (to r) []) ++ acc)

--generate new rules from special Ns
generateRules :: [String] -> [Rule] -> [Rule]
generateRules (s:ss) [] = generateRules ss  ((Rule s ((toLower (s !! 0)) : [])) : [])
generateRules (s:ss) acc = generateRules ss ((Rule s ((toLower (s !! 0)) : [])) : acc)
generateRules [] acc = acc

addSpecialNs cfg = 
	CFG ((nonterminals cfg) ++ (filter (not . null) (getSpecialNs (rules cfg) []))) (terminals cfg) (starting cfg) ((rules cfg) ++ generateRules (getSpecialNs (rules cfg) [] ) [])

convertGrammar cfg = 
	CFG newNs (terminals cfg) (starting cfg) newRules
	where
		newNs = (nonterminals cfg) ++ (filter (not . null) (map (getNewN) (map (annt) (rules cfg))))
		newRules = map annt (rules cfg) ++ (newNsToRs (filter (beginWithL) newNs) [])

procLns :: [String] -> CFG
procLns (ns:ts:start:rules) = 
	if null rules 
		then error "Rules are missing"
		else CFG getNs getTs getStarting (map parseRule rules)
	where
		getNs = splitOn "," ns
		getTs = tokenize ',' ts
		getStarting = head start
procLns _ = error "Input file with bad syntax provided"

getCFGrammar :: Handle -> IO CFG
getCFGrammar hIn = do
	contents <- hGetContents hIn
	let lns = lines contents
	let cfg = procLns lns
	return cfg

entry file = do
	hInFile 	<- openFile file ReadMode
	cfg 		<- getCFGrammar hInFile
	putStr( show ((addSpecialNs . convertGrammar) cfg))

	hClose hInFile
	return ()