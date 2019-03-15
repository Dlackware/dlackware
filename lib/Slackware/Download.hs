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
import qualified Data.ByteString.Char8 as C8
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

filename :: C8.ByteString -> C8.ByteString
filename url = snd $ C8.breakEnd ('/' ==) url

get :: MonadHttp m => C8.ByteString -> Maybe (m (Digest MD5))
get url = either get' get' <$> parseUrl url
        where
            destination = C8.unpack $ filename url
            get' (method, options)
              = reqBr GET method NoReqBody options $ \r ->
                  runConduitRes
                      $ responseBodySource r
                     .| getZipSink (ZipSink (sinkFile destination) *> ZipSink sinkHash)
