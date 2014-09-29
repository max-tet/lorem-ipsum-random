import WList (WList(..), distribFromList, pickRandom, bumpOrInsert)
import Data.List.Split (splitOn)
import Data.List (intersect, intersperse)
import Data.Maybe (fromJust)
import qualified System.Random as Random
import qualified Data.Map as Map

allowedChars = ['A'..'Z']++['a'..'z']

main = do
	input <- getContents
	gen <- Random.getStdGen
	let cleanInput = (cleanup . decompose) input
	    distribSentLen = distribFromList (\x -> length x) cleanInput
	    distribWordLen = distribFromList (\x -> length x) $ concat cleanInput
	    distribLetters = foldl analyseWord Map.empty $ concat cleanInput :: Map.Map Int [WList Char]
	    mkWordInst = mkWord distribWordLen (\wordLen pos seed -> [fromJust $ fst $ pickRandom (distribLetters Map.! wordLen !! pos) gen])
	    mkSentenceInst = mkSentence distribSentLen mkWordInst
	putStrLn $ mkSentenceInst $ fst $ Random.random gen

decompose = (map (splitOn " ")) . (splitOn ".")
cleanup = filter (\x -> length x > 0) . (map $ filter (\x -> length x > 0) . map (filter (`elem` allowedChars)))
distribLength = distribFromList (\x -> length x)

-- sentence length distribution -> (seed -> word) -> seed -> sentence
mkSentence :: WList Int -> (Int -> String) -> Int -> String
mkSentence wl mkWord seed = concat $ intersperse " " $ take sentenceLen $ map mkWord (drop 1 $ Random.randoms (Random.mkStdGen seed))
	where sentenceLen = fromJust $ fst $ pickRandom wl (Random.mkStdGen seed)

-- word length distribution -> (word length -> char position -> seed -> char) -> seed -> word
mkWord :: WList Int -> (Int -> Int -> Int -> String) -> Int -> String
mkWord wld mkChar seed = snd $ foldl (\(pos, wrd) r -> (pos+1, wrd++(mkChar wordLen pos r))) (0,"") randomList
	where wordLen = fromJust $ fst $ pickRandom wld (Random.mkStdGen seed)
	      randomList = take wordLen $ drop 1 $ Random.randoms (Random.mkStdGen seed)

-- list of letter distrib for each letter pos -> word of the same length -> new list of letter distrib
analyseLetterDistrib :: [WList Char] -> String -> [WList Char]
analyseLetterDistrib [] word = analyseLetterDistrib (replicate (length word) Empty) word
analyseLetterDistrib wll word = map (\(wl, char) -> bumpOrInsert char wl) (zip wll word)

-- map over word lengths, containing list over letter distribs for each letter pos -> word of any length -> updated map
analyseWord :: Map.Map Int [WList Char] -> String -> Map.Map Int [WList Char]
analyseWord mp word = if k `Map.member` mp
	then Map.insert k (analyseLetterDistrib (mp Map.! k) word) mp
	else Map.insert k (analyseLetterDistrib [] word) mp
	where k = length word