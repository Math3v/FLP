import System.IO

data Rule =
	Rule
		{ from :: Char 
		, to :: String
		} deriving Show

--consume->
consumeLeftArrow :: String -> String
consumeLeftArrow [] = []
consumeLeftArrow (x:xs) = 
	if (x == '-' || x == '>')
	then consumeLeftArrow xs
	else (x:xs)

--parseRule
parseRule :: String -> Rule
parseRule (x:xs) = Rule x (consumeLeftArrow xs)	

--tokenize
tokenize delim "" = []
tokenize delim (x:xs) = 
	if x == delim
	then tokenize delim xs
	else x : tokenize delim xs

--readRules
readRules file = do
	eof <- hIsEOF file
	if eof
		then return ()
		else do
			line 	<- hGetLine file
			putStrLn (show(parseRule line))
			readRules file

--read file
rf file = do
	f 				<- openFile file ReadMode
	nonterminals	<- hGetLine f
	terminals		<- hGetLine f
	starting		<- hGetLine f
	putStrLn ("Nonterminals: " ++ show (tokenize ',' nonterminals))
	putStrLn ("Terminals: " ++ show (tokenize ',' terminals))
	putStrLn ("Starting symbol: " ++ show starting)
	readRules f