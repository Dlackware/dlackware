{-# LANGUAGE OverloadedStrings #-}
module Slackware.Download ( get
                          , filename
                          ) where
import Conduit ( ZipSink(..)
               , getZipSink
               , sinkFile
               )
import Crypto.Hash ( Digest
                   , MD5
                   )
import Crypto.Hash.Conduit (sinkHash)
import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import Data.Conduit ( (.|)
                    , runConduitRes
                    )
import Network.HTTP.Req ( GET (..)
                        , MonadHttp
                        , NoReqBody(..)
                        , reqBr
                        , parseUrl
                        )
import Network.HTTP.Req.Conduit (responseBodySource)

filename :: T.Text -> T.Text
filename url = snd $ T.breakOnEnd "/" url

get :: MonadHttp m => T.Text -> Maybe (m (Digest MD5))
get url = either get' get' <$> parseUrl (E.encodeUtf8 url)
        where
            destination = T.unpack $ filename url
            get' (method, options)
              = reqBr GET method NoReqBody options $ \r ->
                  runConduitRes
                      $ responseBodySource r
                     .| getZipSink (ZipSink (sinkFile destination) *> ZipSink sinkHash)
