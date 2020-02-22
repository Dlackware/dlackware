{-# LANGUAGE OverloadedStrings #-}
module Slackware.Download ( get
                          , filename
                          ) where

import Conduit (ZipSink(..), getZipSink, sinkFile)
import Crypto.Hash (Digest, MD5)
import Crypto.Hash.Conduit (sinkHash)
import Data.Conduit ((.|), runConduitRes)
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Text as Text
import Network.HTTP.Req (GET (..), MonadHttp, NoReqBody(..), reqBr, useURI)
import Network.HTTP.Req.Conduit (responseBodySource)
import Text.URI (URI(..), unRText)

filename :: URI -> String
filename = maybe "" (Text.unpack . unRText . NonEmpty.last . snd) . uriPath

get :: MonadHttp m => URI -> Maybe (m (Digest MD5))
get url = do
    parsedUrl <- useURI url

    let get' = getTo $ filename url
     in return $ either get' get' parsedUrl
  where
    getTo destination (method, options)
      = reqBr GET method NoReqBody options $ \r ->
          runConduitRes
              $ responseBodySource r
              .| getZipSink (ZipSink (sinkFile destination) *> ZipSink sinkHash)
