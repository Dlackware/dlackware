module Slackware.Download ( get
                          , filename
                          ) where

import           Control.Applicative ((<|>))
import qualified Data.ByteString.Char8 as C8
import           Data.Conduit ( (.|)
                              , runConduitRes
                              )
import           Data.Conduit.Binary (sinkFile)
import           Network.HTTP.Req ( NoReqBody (..)
                                  , GET (..)
                                  , reqBr
                                  , parseUrlHttp
                                  , parseUrlHttps
                                  , MonadHttp
                                  )
import           Network.HTTP.Req.Conduit (responseBodySource)

filename :: C8.ByteString -> C8.ByteString
filename url = snd $ C8.breakEnd ('/' ==) url

get :: MonadHttp m => C8.ByteString -> Maybe (m ())
get url = get' <$> parseUrlHttps url <|> get' <$> parseUrlHttp url
        where get' (method, options) =
                reqBr GET method NoReqBody options $ \r ->
                    runConduitRes $
                        responseBodySource r .| sinkFile (C8.unpack $ filename url)
