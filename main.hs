import qualified WList as WL
import Data.List.Split (splitOn)
import Data.List (intersect)

allowedChars = ['A'..'Z']++['a'..'z']

main = do
	input <- getContents
	putStrLn $ show $ (cleanup . decompose) input

decompose = (map (splitOn " ")) . (splitOn ".")
cleanup = filter (\x -> length x > 0) . (map $ filter (\x -> length x > 0) . map (filter (`elem` allowedChars)))