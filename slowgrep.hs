import System.Environment
import System.IO (stdout,stderr,hPutStr,hPutStrLn)

--
-- Simpleton Regular Expressions in Haskell (Slow!)
--

-- To-Do:
-- - Feature1: Escaped Metacharacters  (Finished)
-- - Feature2: Any Metacharacter       (Finished)
-- - Feature3: Option Metacharacter    (Finished)
-- - Feature4: Plus Metacharacter      (Finished)
-- - Feature5: Character Classes       ()

-- 1. An algebraic data type for regular expressions

data RE = Epsilon
        | Any
        | Ch Char 
        | Seq RE RE
        | Alt RE RE
        | Star RE
        | Option RE
        | Plus RE
        | Group RE
    deriving Show

-- 2. A simple match function to determine if a string matches a regular expression

match :: RE -> [Char] -> Bool
splits :: [Char] -> [([Char], [Char])]
add_to_prefixes :: Char -> [([Char], [Char])] -> [([Char], [Char])]
match_any_split :: RE -> RE -> [([Char], [Char])] -> Bool
match_any_nonempty_split :: RE -> RE -> [([Char], [Char])] -> Bool

-- match :: RE -> [Char] -> Bool
match Epsilon s = s == ""
match (Ch a) "" = False
match (Ch a) (c : more_chars) = a == c && more_chars == []
match (Alt r1 r2) string = match r1 string || match r2 string
match (Seq r1 r2) string = match_any_split r1 r2 (splits string)
match (Star r1) "" = True
match (Star r1) s = match_any_nonempty_split r1 (Star r1) (splits s)
match (Option r1) "" = True
match (Option r1) s = match r1 s
match (Plus r1) "" = False
match (Plus r1) s = match_any_nonempty_split r1 (Star r1) (splits s)
match (Group r1) s = match r1 s
match Any "" = False
match Any (c : more_chars) = more_chars == []

-- splits :: [Char] -> [([Char], [Char])]
splits "" = [("", "")]
splits (c1:chars) = ("", c1:chars) : add_to_prefixes c1 (splits chars)

-- add_to_prefixes :: Char -> [([Char], [Char])] -> [([Char], [Char])]
add_to_prefixes c []  = []
add_to_prefixes c ((pfx, sfx) : more) = (c:pfx, sfx) : (add_to_prefixes c more)

-- match_any_split :: RE -> RE -> [([Char], [Char])] -> Bool
match_any_split r1 r2 [] = False
match_any_split r1 r2 ((s1, s2) : more_splits) 
   | match r1 s1 && match r2 s2     = True
   | otherwise                      = match_any_split r1 r2 more_splits 

-- match_any_nonempty_split :: RE -> RE -> [([Char], [Char])] -> Bool
match_any_nonempty_split r1 r2 [] = False
match_any_nonempty_split r1 r2 ((s1, s2) : more) 
   | s1 /= "" && match r1 s1 && match r2 s2     = True
   | otherwise                                  = match_any_nonempty_split r1 r2 more 


-- 3.  A parser to convert text into regular expressions

-- ==BNF Grammars reference from notes==
--  <RE> ::= <seq> | <RE> "|" <seq>
--  <seq> ::= <item> | <seq> <item>
--  <item> ::= <element> | <element> "*" | <element> "?" | <element> "+"
--  <element> ::= <char> | "(" <RE> ")"
--  <char> ::= any character except "|", "*", "(", ")", "?" | ".", "\"

parseRE :: [Char] -> Maybe (RE, [Char])
parseSeq :: [Char] -> Maybe (RE, [Char])
parseItem :: [Char] -> Maybe (RE, [Char])
parseElement :: [Char] -> Maybe (RE, [Char])
parseChar :: [Char] -> Maybe (RE, [Char])
parseMetachar :: [Char] -> Maybe (RE, [Char])

extendSeq :: (RE, [Char]) -> Maybe (RE, [Char])
extendRE :: (RE, [Char]) -> Maybe (RE, [Char])

-- parseMetachar :: [Char] -> Maybe (RE, [Char])
parseMetachar [] = Nothing
parseMetachar (c:s)
  | c == '|' || c == '*' || c == '(' || c == ')'  || c == '?' || c == '+' || c == '.' || c == '\\'  = Just ((Ch c), s)
  | otherwise                                                                                       = Nothing

-- parseChar :: [Char] -> Maybe (RE, [Char])
parseChar [] = Nothing
parseChar (c:s)
  | c == '|' || c == '*' || c == '(' || c == ')'  || c == '?' || c == '+' = Nothing
  | c == '.'                                                              = Just (Any, s)
  | c == '\\'                                                             = parseMetachar s
  | otherwise                                                             = Just ((Ch c), s)

-- parseElement :: [Char] -> Maybe (RE, [Char])
parseElement ('(':more) =
    case parseRE(more) of
        Just (re, ')':yet_more) -> Just(Group re, yet_more)
        _ -> Nothing
parseElement s = parseChar s

-- parseItem :: [Char] -> Maybe (RE, [Char])
parseItem s =
   case parseElement(s) of
        Just (re, '*':more) -> Just (Star re, more)
        Just (re, '?':more) -> Just (Option re, more)
        Just (re, '+':more) -> Just (Plus re, more)
        Just (re, more) -> Just (re, more)
        _ -> Nothing

-- parseSeq :: [Char] -> Maybe (RE, [Char])
parseSeq s =
    case parseItem(s) of
        Just (r, more_chars) -> extendSeq(r, more_chars)
        _ -> Nothing

-- extendSeq :: (RE, [Char]) -> Maybe (RE, [Char])
extendSeq (e1, after1) =
    case parseItem(after1) of 
        Just(e2, more) -> extendSeq(Seq e1 e2, more)
        _ -> Just(e1, after1)

-- parseRE :: [Char] -> Maybe (RE, [Char])
parseRE s =
    case parseSeq(s) of
        Just (r, more_chars) -> extendRE(r, more_chars)
        _ -> Nothing

-- extendRE :: (RE, [Char]) -> Maybe (RE, [Char])
extendRE (e1, []) = Just (e1, [])
extendRE (e1, '|' : after_bar) =
    case parseSeq(after_bar) of 
        Just(e2, more) -> extendRE(Alt e1 e2, more)
        _ -> Nothing
extendRE(e1, c:more) = Just (e1, c:more)

parseMain :: [Char] -> Maybe RE
parseMain s = case parseRE s of 
    Just (e, []) -> Just e
    _ -> Nothing

-- 4.  Searching for matching lines in a file

matches :: RE -> [[Char]] -> [[Char]]
matches re lines = filter (match re) lines

matching :: [Char] -> [[Char]] -> [[Char]]
matching regexp lines = case parseMain regexp of
                            Just r -> matches r lines
                            _ -> []

-- for testing in GHCi
matchTest :: String -> String -> Bool
matchTest regexp str = case parseMain regexp of
    Just r -> match r str
    _ -> False

-- checks to see if a string is made up of only backslashes
isOnlyBackslashes :: String -> Bool
isOnlyBackslashes (c:s)
  | c == '\\' && s /= [] = isOnlyBackslashes s
  | c == '\\' && s == [] = True
  | otherwise            = False

-- if string is only back slashes, adds back the backslash characters
-- that the command prompt takes away
fixBackslashString :: String -> Bool -> String
fixBackslashString str bool = case bool of
  True  -> str ++ str
  False -> str

    

-- 5.  Command line interface

main = do
  [regExp1, fileName] <- getArgs

  let regExp2 = fixBackslashString regExp1 (isOnlyBackslashes regExp1)

  srcText <- readFile fileName
  hPutStr stdout (unlines (matching regExp2 (lines srcText)))