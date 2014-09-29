import WList
import Data.List.Split (splitOn)
import Data.List (intersect, intersperse)
import Data.Maybe (fromJust)
import qualified System.Random as Random

allowedChars = ['A'..'Z']++['a'..'z']

main = do
	input <- getContents
	gen <- Random.getStdGen
	let cleanInput = (cleanup . decompose) input
	    distribSentLen = WList.distribFromList (\x -> length x) cleanInput
	    distribWordLen = WList.distribFromList (\x -> length x) $ concat cleanInput
	    mkWordInst = mkWord distribWordLen (\wordLen pos seed -> "a")
	    mkSentenceInst = mkSentence distribSentLen mkWordInst
	putStrLn $ mkSentenceInst $ fst $ Random.random gen

decompose = (map (splitOn " ")) . (splitOn ".")
cleanup = filter (\x -> length x > 0) . (map $ filter (\x -> length x > 0) . map (filter (`elem` allowedChars)))
distribLength = WList.distribFromList (\x -> length x)

-- sentence length distribution -> (seed -> word) -> seed -> sentence
mkSentence :: WList.WList Int -> (Int -> String) -> Int -> String
mkSentence wl mkWord seed = concat $ intersperse " " $ take sentenceLen $ map mkWord (drop 1 $ Random.randoms (Random.mkStdGen seed))
	where sentenceLen = fromJust $ fst $ WList.pickRandom wl (Random.mkStdGen seed)

-- word length distribution -> (word length -> char position -> seed -> char) -> seed -> word
mkWord :: WList.WList Int -> (Int -> Int -> Int -> String) -> Int -> String
mkWord wld mkChar seed = snd $ foldl (\(pos, wrd) r -> (pos+1, wrd++(mkChar wordLen pos r))) (0,"") randomList
	where wordLen = fromJust $ fst $ WList.pickRandom wld (Random.mkStdGen seed)
	      randomList = take wordLen $ drop 1 $ Random.randoms (Random.mkStdGen seed)
