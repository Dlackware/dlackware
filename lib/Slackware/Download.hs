{-# LANGUAGE OverloadedStrings #-}
module Slackware.Download ( get
                          , filename
                          ) where

import Conduit (ZipSink(..), getZipSink, sinkFile)
import Crypto.Hash (Digest, MD5)
import Crypto.Hash.Conduit (sinkHash)
import Data.Conduit ((.|), runConduitRes)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Text (Text)
import qualified Data.Text as Text
import Network.HTTP.Req (GET (..), MonadHttp, NoReqBody(..), reqBr, useURI)
import Network.HTTP.Req.Conduit (responseBodySource)
import Text.URI (URI(..), mkURI, unRText)

filename :: Text -> Text
filename url = snd $ Text.breakOnEnd "/" url

get :: MonadHttp m => Text -> Maybe (m (Digest MD5))
get url = do
    uri <- mkURI url
    parsedUrl <- useURI uri
    (_, path') <- uriPath uri

    let destination = Text.unpack $ unRText $ NonEmpty.last path'
        get' = getTo destination
     in return $ either get' get' parsedUrl
  where
    getTo destination (method, options)
      = reqBr GET method NoReqBody options $ \r ->
          runConduitRes
              $ responseBodySource r
              .| getZipSink (ZipSink (sinkFile destination) *> ZipSink sinkHash)
