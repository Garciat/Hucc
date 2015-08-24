{-# LANGUAGE FlexibleContexts #-}

module Language.Python.Parser.Common where

import qualified Text.Parsec as P

countRange :: P.Stream s m t => Int -> Int -> P.ParsecT s u m a -> P.ParsecT s u m [a]
countRange min' max' p = go 0
  where
    go n
      | n >= max'  = return []
      | otherwise = do{ mx <- P.optionMaybe p
                      ; case mx of
                          Nothing  -> if n < min' then P.parserZero else return []
                          Just x   -> do{ xs <- go (n + 1)
                                        ; return (x:xs)
                                        } 
                      }