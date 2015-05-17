module Network.Email.Read where

import Data.Maybe
import Control.Monad
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.ByteString.Builder as BL
import qualified Data.Attoparsec.ByteString.Lazy as BL
import qualified Data.Attoparsec.Text as T
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Network.Email.Header.Types
import qualified Network.Email.Header.Read as R
import qualified Network.Email.Charset as C
import qualified Data.Map as M
import Control.Monad.Catch

import Network.Email.Parser
import Network.Email.Types

-- | Decode binary body from 'Mail' using headers.
decodeBinary :: MonadThrow m => Mail -> m BL.ByteString
decodeBinary mail = do
  enc <- encoder
  case BL.parse (enc <* BL.endOfInput) $ mailBody mail of
   BL.Fail _ _ e -> throwM $ DecodingError e
   BL.Done _ r -> return $ BL.toLazyByteString r

  where encoding = fromMaybe "8bit" $ R.contentTransferEncoding $ mailHeaders mail
        encoder = case encoding of
          "quoted-printable" -> return quotedPrintable
          "base64" -> return base64
          "8bit" -> return BL.takeLazyByteString
          "7bit" -> return BL.takeLazyByteString
          _ -> throwM $ UnknownEncoding encoding

-- | Decode text body from 'Mail' using headers.
decodeText :: MonadThrow m => Mail -> m TL.Text
decodeText mail = do
  dat <- decodeBinary mail
  unless (typ == "text") $ throwM NotAText
  cs <- maybe (throwM $ UnknownCharset charset) return $ C.lookupCharset charset
  let txt = C.toUnicode $ BL.toStrict dat
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
