module Network.Email.Read where

import Data.Maybe
import Control.Monad
import qualified Data.Map as M
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.ByteString.Builder as BL
import qualified Data.Attoparsec.ByteString.Lazy as BL
import qualified Data.Attoparsec.Text as T
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.ByteString.Base64.Lazy as B64
import Control.Monad.Catch

import Network.Email.Header.Types
import qualified Network.Email.Header.Read as R
import qualified Network.Email.Charset as C

import Network.Email.Parser
import Network.Email.Types

-- | Decode binary body from 'Mail' using headers.
decodeBinary :: forall m. MonadThrow m => Mail -> m BL.ByteString
decodeBinary mail = do
  enc <- encoder
  case enc $ mailBody mail of
   Left e -> throwM $ DecodingError e
   Right r -> return $ BL.toLazyByteString r

  where encoding = fromMaybe "8bit" $ R.contentTransferEncoding $ mailHeaders mail
        encoder :: m (BL.ByteString -> Either String BL.Builder)
        encoder = case encoding of
          "quoted-printable" -> return $ BL.eitherResult . BL.parse (quotedPrintable <* BL.endOfInput)
          "base64" -> return $ fmap BL.lazyByteString . B64.decode
          "8bit" -> return $ Right . BL.lazyByteString
          "7bit" -> return $ Right . BL.lazyByteString
          _ -> throwM $ UnknownEncoding encoding

-- | Decode text body from 'Mail' using headers.
decodeText :: MonadThrow m => Mail -> m TL.Text
decodeText mail = do
  dat <- decodeBinary mail
  unless (typ == "text") $ throwM NotAText
  cs <- maybe (throwM $ UnknownCharset charset) return $ C.lookupCharset charset
  let txt = C.toUnicode cs $ BL.toStrict dat
  case T.parseOnly (crlfText <* T.endOfInput) txt of
   Left e -> throwM $ DecodingError e
   Right r -> return r

  where (MimeType typ _, pars) = fromMaybe (MimeType "text" "plain", M.empty) $ R.contentType $ mailHeaders mail
        charset = maybe "us-ascii" T.unpack $ M.lookup "charset" pars

-- | Find all parts with given mime type in 'Mail'.
findMime :: MimeType -> Mail -> [Path]
findMime typ ml = (if typ == typ' then [[]] else []) ++ parts 
  where typ' = case R.contentType $ mailHeaders ml of
                Nothing -> MimeType "text" "plain"
                Just (t, _) -> t
        parts = concatMap (\(i, x) -> map (i:) $ findMime typ x) $ zip [0..] $ mailParts ml
