import Control.Monad
import Text.Regex
import Text.XML.Light

concatWith :: String -> [String] -> String
concatWith _ [] = []
concatWith d (h:t) = foldl fun h t where
  fun s0 s1 = s0 ++ d ++ s1

getChild :: [Int] -> Element -> Element
getChild [] e = e
getChild (h:t) e = foldl fun e (h:t) where
  fun e n = (elChildren e) !! n

getF :: String -> Element -> Element
getF s e = (filterChildren (\x -> findAttr (unqual "name") x == Just s) e)!!0

printFs :: Element -> String
printFs fs = error ++ form ++ "\t" ++ lemma ++ "\t" ++ (concatWith ":" interp) where
  form = strContent $ getChild [0] $ getF "orth" fs
  (lemma:interp) = splitRegex (mkRegex ":") (strContent $ getChild [0, 1, 0] $ getF "disamb" fs)
  error = if (length $ filter (== ':') $ form) /= 0 then "ERROR " else ""

main = do
  s <- getContents
  let segs = filterElementsName (\e -> (qName e) == "fs") $ (onlyElems $ parseXML s)!!1
  mapM_ (putStrLn . printFs) segs
