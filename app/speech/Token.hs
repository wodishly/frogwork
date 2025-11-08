{- HLINT ignore "Use infix" -}
module Token where

import Mean (Shell, Shift, scoop, leave, shell)


shades :: [(String, String)]
shades = [
   ("th", "θ")
 , ("dd", "ð")
 , ("lh", "ɬ")
 , ("lz", "ɮ")

 , ("bh", "bʰ")
 , ("dh", "dʰ")
 , ("gh", "gʰ")

 , ("c", "kʸ")
 , ("ky", "kʸ")
 , ("q", "kʷ")
 , ("kw", "kʷ")

 , ("j", "gʸ")
 , ("gy", "gʸ")
 , ("v", "gʷ")
 , ("gw", "gʷ")

 , ("jh", "gʸʰ")
 , ("gyh", "gʸʰ")
 , ("vh", "gʷʰ")
 , ("gwh", "gʷʰ")

 , ("h1", "h₁")
 , ("h2", "h₂")
 , ("h3", "h₃")

 , ("ng", "ŋ")
 , ("ngw", "ŋʷ")

 , ("@1", "ə₁")
 , ("@2", "ə₂")
 , ("@3", "ə₃")

 , ("M", "m̩")
 , ("N", "n̩")
 , ("L", "l̩")
 , ("R", "r̩")

 , ("MM", "m̩̄")
 , ("NN", "n̩̄")
 , ("LL", "l̩̄")
 , ("RR", "r̩̄")

 , ("A", "ā")
 , ("E", "ē")
 , ("I", "ī")
 , ("O", "ō")
 , ("U", "ū")
 ]

sharps :: [(String, String)]
sharps = [
   ("a", "á")
 , ("e", "é")
 , ("i", "í")
 , ("o", "ó")
 , ("u", "ú")
 ]

shadesOf :: Int -> [(String, String)]
shadesOf n = filter ((== n) . length . fst) shades

betoken :: Shell String
betoken = filter (/= ".") . betoken' (maximum (map (length.fst) shades)) . map shell

-- inwend on length of n-graph
betoken' :: Int -> Shift [String]
betoken' 0 = id
betoken' n = betoken' (n-1) . betoken'' n

-- leftfare through `s` on `n`
betoken'' :: Int -> Shift [String]
betoken'' _ [] = []
betoken'' n s = if length s >= n
  then if elem (concat (scoop n s)) (map fst (shadesOf n))
    then betoken'' n (leave n s) ++ [concat (scoop n s)]
    else betoken'' n (init s) ++ [last s]
  else s