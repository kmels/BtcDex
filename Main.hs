{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Lens
import Network.Wreq
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as C8
import Text.HTML.TagSoup

download :: IO ()
download = do
  r <- get "http://coinmarketcap.com/currencies/bitcoin/#markets"
  let body = r ^. responseBody
      price = weightedPrice body      
  putStrLn "Done."
  putStrLn $ "Weighted price: $" ++ (show price)

weightedPrice :: L.ByteString -> Float 
weightedPrice html = 
  let
    tags = parseTags html
    tables = sections (isTagOpenName "table") tags
    markets = tables !! 1
    rows = tail $ sections (isTagOpenName "tr") markets
    
    byte2float b = read (C8.unpack b) :: Float
    
    name row = fromTagText $ row !! 8
    btc_24hvolume row = byte2float $ fromAttrib "data-btc" $ row !! 17
    usd_24hvolume row = byte2float $ fromAttrib "data-usd" $ row !! 17  
    usd_price row = byte2float $ fromAttrib "data-usd" $ row !! 20

    btc_weight row = usd_price row * btc_24hvolume row 
    usd_weight row = usd_price row * usd_24hvolume row    

    btc_weights = map btc_weight rows
    usd_weights = map usd_weight rows
    total_usdvolume = sum $ map usd_24hvolume rows
    total_btcvolume = sum $ map btc_24hvolume rows
    in (sum usd_weights) / total_usdvolume

main = download
