{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Main where

import Control.Lens
import Network.Wreq
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as C8
import qualified Data.Text as T 
import qualified Data.String.Utils as Utils

import Text.HTML.TagSoup
import GHC.Generics

import Data.Aeson
import Text.Read (readEither)
import Data.Maybe(catMaybes)
data Currency = Currency {
  c_name :: T.Text,
  c_code :: T.Text,
  cap_usd :: Maybe Float,
  cap_btc :: Maybe Float,
  price_usd :: Maybe Float,
  price_btc :: Maybe Float,
  supply :: Maybe Float,
  vol_24h_usd :: Maybe Float,
  vol_24h_btc :: Maybe Float,
  pchange_1h_usd :: Maybe Float,
  pchange_1h_btc :: Maybe Float,
  pchange_24h_usd :: Maybe Float,
  pchange_24h_btc :: Maybe Float,
  pchange_7d_usd :: Maybe Float,
  pchange_7d_btc :: Maybe Float
} deriving (Generic, Show)

instance ToJSON Currency where
    toEncoding = genericToEncoding defaultOptions
    
downloadCurrencyList :: IO [Currency]
downloadCurrencyList = do
  r <- get "http://coinmarketcap.com/all/views/all/"
  
  let body = r ^. responseBody
      html = parseTags body
      rows = tail $! sections (isTagOpenName "tr") html

      -- name 
      currency_name row = fromTagText $ row !! 11
      -- symbol
      currency_code row = fromTagText $ row !! 17
      -- market capital
      currency_cap_usd row = byte2float $ fromAttrib "data-usd" $ row !! 20
      currency_cap_btc row = byte2float $ fromAttrib "data-btc" $ row !! 20
      -- price      
      currency_price_usd row = byte2float $ fromAttrib "data-usd" $ row !! 26
      currency_price_btc row = byte2float $ fromAttrib "data-btc" $ row !! 26
      -- supply
      currency_supply row = byte2float . C8.pack . Utils.replace "," "" . Utils.strip . C8.unpack . fromTagText $ row !! 35
      -- volume
      currency_24hvol_usd row = byte2float $ fromAttrib "data-usd" $ row !! 42
      currency_24hvol_btc row = byte2float $ fromAttrib "data-btc" $ row !! 42
      -- 1h change
      currency_1hchange_usd row = byte2float $ fromAttrib "data-usd" $ row !! 48
      currency_1hchange_btc row = byte2float $ fromAttrib "data-btc" $ row !! 48
      -- 24h change
      currency_24hchange_usd row = byte2float $ fromAttrib "data-usd" $ row !! 52
      currency_24hchange_btc row = byte2float $ fromAttrib "data-btc" $ row !! 52
      -- 7day change
      currency_7dchange_usd row = byte2float $ fromAttrib "data-usd" $ row !! 56
      currency_7dchange_btc row = byte2float $ fromAttrib "data-btc" $ row !! 56
      
    in return [ Currency {
                   c_name = T.pack . C8.unpack $ currency_name row, 
                   c_code = T.pack . C8.unpack $ currency_code row,
                   cap_usd = currency_cap_usd row,
                   cap_btc = currency_cap_btc row,
                   price_usd = currency_price_usd row,
                   price_btc = currency_price_btc row, 
                   supply = currency_supply row,
                   vol_24h_usd = currency_24hvol_usd row,
                   vol_24h_btc = currency_24hvol_btc row,
                   pchange_1h_usd = currency_1hchange_usd row,
                   pchange_1h_btc = currency_1hchange_btc row,
                   pchange_24h_usd = currency_24hchange_usd row,
                   pchange_24h_btc = currency_24hchange_btc row,
                   pchange_7d_usd = currency_7dchange_usd row,
                   pchange_7d_btc = currency_7dchange_btc row
              } | row <- rows ]

saveCurrencies :: IO ()
saveCurrencies = do
  clist <- downloadCurrencyList
  let cjsons = map encode clist
  putStrLn "Saved currencies"

  
downloadBitcoin :: IO ()
downloadBitcoin = downloadCurrency "bitcoin"

downloadCurrency :: String -> IO ()
downloadCurrency c = do
  r <- get ("http://coinmarketcap.com/currencies/"++c++"/#markets")
  let body = r ^. responseBody
      wprice = weightedPrice body
      okprice = okcoinFuturesSpotIndex body      
  putStrLn "Done."
  putStrLn $ "Weighted price: $" ++ (show wprice)
  putStrLn $ "OKCoin Futures Spot: $" ++ (show okprice)

name row = fromTagText $ row !! 8

byte2float :: C8.ByteString -> Maybe Float
byte2float = either (\l -> Nothing) Just . readEither . C8.unpack
--byte2float b = read (C8.unpack b) :: Float

btc_24hvolume row = byte2float $ fromAttrib "data-btc" $ row !! 17
usd_24hvolume row = byte2float $ fromAttrib "data-usd" $ row !! 17  
usd_price row = byte2float $ fromAttrib "data-usd" $ row !! 20

okcoinFuturesSpotIndex :: L.ByteString -> Float
okcoinFuturesSpotIndex html = let
    tags = parseTags html
    tables = sections (isTagOpenName "table") tags
    markets = tables !! 1
    rows = tail $ sections (isTagOpenName "tr") markets
    
    fn = (\r -> any ((==) (name r)) ["OKCoin.cn","Huobi","BTCChina","Bitstamp", "OkCoin Intl.", "Bitfinex"])
    okspot_rowz = filter fn rows
    prices = map usd_price okspot_rowz    
  in (sum . catMaybes $ prices) / (fromIntegral $ length prices)

weightedPrice :: L.ByteString -> Float 
weightedPrice html = 
  let
    tags = parseTags html
    tables = sections (isTagOpenName "table") tags
    markets = tables !! 1
    rows = tail $ sections (isTagOpenName "tr") markets   

    btc_weight row = usd_price row >>= \p -> btc_24hvolume row >>= \v -> return (p * v)
    usd_weight row = usd_price row >>= \p -> usd_24hvolume row >>= \v -> return (p * v)

    btc_weights = map btc_weight rows
    usd_weights = map usd_weight rows
    total_usdvolume = sum . catMaybes $ map usd_24hvolume rows
    total_btcvolume = sum . catMaybes $ map btc_24hvolume rows
    in (sum . catMaybes $ usd_weights) / total_usdvolume

main = downloadBitcoin
