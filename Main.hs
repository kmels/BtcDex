{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Main where

import Control.Lens
import Network.Wreq
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as C8
import qualified Data.Text as T 
import qualified Data.String.Utils as Utils
import qualified Database.Bloodhound as ELS
import qualified Network.HTTP.Client as HttpCli

import Text.HTML.TagSoup
import GHC.Generics

import Data.Aeson
import Text.Read (readEither)
import Data.Maybe(catMaybes)
import Data.Time.Clock


import System.Environment

data Currency = Currency {
  c_name :: T.Text,
  c_code :: T.Text,
  cap_usd :: T.Text,
  cap_btc :: T.Text,
  price_usd :: T.Text,
  price_btc :: T.Text,
  supply :: T.Text,
  vol_24h_usd :: T.Text,
  vol_24h_btc :: T.Text,
  pchange_1h_usd :: T.Text,
  pchange_1h_btc :: T.Text,
  pchange_24h_usd :: T.Text,
  pchange_24h_btc :: T.Text,
  pchange_7d_usd :: T.Text,
  pchange_7d_btc :: T.Text
} deriving (Generic, Show)

instance ToJSON Currency where
    toEncoding = genericToEncoding defaultOptions

data CryptoMarket = CryptoMarket {
  time :: UTCTime,
  currencies :: [Currency]
} deriving (Generic, Show)

instance ToJSON CryptoMarket where
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
      currency_cap_usd row = fromAttrib "data-usd" $ row !! 20
      currency_cap_btc row = fromAttrib "data-btc" $ row !! 20
      -- price      
      currency_price_usd row = fromAttrib "data-usd" $ row !! 26
      currency_price_btc row = fromAttrib "data-btc" $ row !! 26
      -- supply
      currency_supply row = C8.pack . Utils.replace "," "" . Utils.strip . C8.unpack . fromTagText $ row !! 35
      -- volume
      currency_24hvol_usd row = fromAttrib "data-usd" $ row !! 42
      currency_24hvol_btc row = fromAttrib "data-btc" $ row !! 42
      -- 1h change
      currency_1hchange_usd row = fromAttrib "data-usd" $ row !! 48
      currency_1hchange_btc row = fromAttrib "data-btc" $ row !! 48
      -- 24h change
      currency_24hchange_usd row = fromAttrib "data-usd" $ row !! 52
      currency_24hchange_btc row = fromAttrib "data-btc" $ row !! 52
      -- 7day change
      currency_7dchange_usd row = fromAttrib "data-usd" $ row !! 56
      currency_7dchange_btc row = fromAttrib "data-btc" $ row !! 56
      
    in return [ Currency {
                   c_name = T.pack . C8.unpack $ currency_name row, 
                   c_code = T.pack . C8.unpack $ currency_code row,
                   cap_usd = T.pack . C8.unpack $ currency_cap_usd row,
                   cap_btc = T.pack . C8.unpack $ currency_cap_btc row,
                   price_usd = T.pack . C8.unpack $ currency_price_usd row,
                   price_btc = T.pack . C8.unpack $ currency_price_btc row, 
                   supply = T.pack . C8.unpack $ currency_supply row,
                   vol_24h_usd = T.pack . C8.unpack $ currency_24hvol_usd row,
                   vol_24h_btc = T.pack . C8.unpack $ currency_24hvol_btc row,
                   pchange_1h_usd = T.pack . C8.unpack $ currency_1hchange_usd row,
                   pchange_1h_btc = T.pack . C8.unpack $ currency_1hchange_btc row,
                   pchange_24h_usd = T.pack . C8.unpack $ currency_24hchange_usd row,
                   pchange_24h_btc = T.pack . C8.unpack $ currency_24hchange_btc row,
                   pchange_7d_usd = T.pack . C8.unpack $ currency_7dchange_usd row,
                   pchange_7d_btc = T.pack . C8.unpack $ currency_7dchange_btc row
              } | row <- rows ]
  
saveCurrencies :: T.Text -> IO ()
saveCurrencies elsHost = do
  clist <- downloadCurrencyList
  now <- getCurrentTime
  
  let    
    --cjsons = map encode clist
   
    snapshot = CryptoMarket { time = now, currencies = clist }
    server   = ELS.Server elsHost
    runELS'  = ELS.withBH HttpCli.defaultManagerSettings server
    idx      = ELS.IndexName "crypto_currencies"
    doctype  = ELS.MappingName "markets"
    stgs     = ELS.defaultIndexDocumentSettings
    id       = ELS.DocId . T.pack . show $ now

  putStrLn . C8.unpack $ encode snapshot
  putStr $ "Indexing " ++ (show now) ++ " ... "

  reply <- runELS' $ ELS.indexDocument idx doctype stgs snapshot id

  putStrLn $ (show reply)
  
  --http://root:morral-tipico@eratostenes:9200
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

byte2float :: C8.ByteString -> Maybe Double
byte2float = either (\l -> Nothing) (Just . (*) 1.0) . readEither . C8.unpack
--byte2float b = read (C8.unpack b) :: Double

btc_24hvolume row = byte2float $ fromAttrib "data-btc" $ row !! 17
usd_24hvolume row = byte2float $ fromAttrib "data-usd" $ row !! 17  
usd_price row = byte2float $ fromAttrib "data-usd" $ row !! 20

okcoinFuturesSpotIndex :: L.ByteString -> Double
okcoinFuturesSpotIndex html = let
    tags = parseTags html
    tables = sections (isTagOpenName "table") tags
    markets = tables !! 1
    rows = tail $ sections (isTagOpenName "tr") markets
    
    fn = (\r -> any ((==) (name r)) ["OKCoin.cn","Huobi","BTCChina","Bitstamp", "OkCoin Intl.", "Bitfinex"])
    okspot_rowz = filter fn rows
    prices = map usd_price okspot_rowz    
  in (sum . catMaybes $ prices) / (fromIntegral $ length prices)

weightedPrice :: L.ByteString -> Double 
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

main = do
  elsHost <- getArgs >>= return . head
  putStrLn $ "Using server: " ++ elsHost

  saveCurrencies (T.pack elsHost)
  
  downloadBitcoin
