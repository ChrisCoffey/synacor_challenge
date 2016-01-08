{-# LANGUAGE ScopedTypeVariables #-}
module Synacor.Parser where

import Synacor.Machine

-- parseInst (n:rest) = p n rest where
--    p 21 _ =  ()

main :: IO ()
main = do
    contents <- getContents
    putStr contents
