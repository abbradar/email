module Network.Email.Render where

import Data.Monoid
import Data.Maybe
import Control.Monad
import Data.Bits
import qualified Data.Map as M
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.ByteString.Builder as BL
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TB
import qualified Data.Text.Lazy.Encoding as TL
import qualified Data.ByteString.Base64.Lazy as B64
import Control.Monad.Catch

import Network.Email.Header.Types
import qualified Network.Email.Header.Read as H
import qualified Network.Email.Header.Render as R
import qualified Network.Email.Charset as C
import Network.Email.Types

mail :: Mail -> BL.ByteString
mail = BL.toLazyByteString . mailPart
  where mailPart ml = hdrs <> BL.byteString "\r\n" <> body <> parts
          where hdrs = BL.lazyByteString $ TL.encodeUtf8 $ TB.toLazyText $ R.outputHeaders $ mailHeaders ml
                body = BL.lazyByteString $ mailBody ml
                parts = case H.boundary (mailHeaders ml) of
                  Nothing -> mempty
                  Just x ->
                    let bnd = BL.byteString "\r\n--" <> BL.byteString x
                        ps = mconcat $ map (\p -> bnd <> mailPart p) $ mailParts ml
                    in ps <> bnd <> BL.byteString "--\r\n"

-- | Encode binary body using headers, return 'Mail'.
encodeBinary :: MonadThrow m => Headers -> BL.ByteString -> m Mail
encodeBinary hdrs body = do
  enc <- encoder
  return SimpleMail { mailHeaders = hdrs
                    , mailBody = BL.toLazyByteString $ enc body
                    }

  where encoding = fromMaybe "8bit" $ H.contentTransferEncoding hdrs
        encoder = case encoding of
          "quoted-printable" -> return quotedPrintable
          "base64" -> return $ BL.lazyByteString . B64.encode
          "8bit" -> return BL.lazyByteString
          "7bit" -> return BL.lazyByteString
          _ -> throwM $ UnknownEncoding encoding

        quotedPrintable :: BL.ByteString -> BL.Builder
        quotedPrintable = mconcat . map qp . BL.unpack
          where qp c | isGood c = BL.char8 c
                     | otherwise = BL.byteString "=" <> hex c
                isGood c = c /= '=' && (c == '\r' || c == '\n' || (c >= '\33' && c <= '\166'))
                hex (fromEnum -> c) = table (c `shiftR` 8) <> table (c .&. 0xFF)
                table n = BL.char8 $ "0123456789ABCDEF" `B.index` n

-- | Encode text body using headers, return 'Mail'.
encodeText :: MonadThrow m => Headers -> TL.Text -> m Mail
encodeText hdrs body = do
  unless (typ == "text") $ throwM NotAText
  cs <- maybe (throwM $ UnknownCharset charset) return $ C.lookupCharset charset
  let bin = toCrlf $ C.fromUnicode cs $ TL.toStrict body
  encodeBinary hdrs bin
  
  where (MimeType typ _, pars) = fromMaybe (MimeType "text" "plain", M.empty) $ H.contentType hdrs
        charset = maybe "us-ascii" T.unpack $ M.lookup "charset" pars

        toCrlf :: B.ByteString -> BL.ByteString
        toCrlf = BL.toLazyByteString . mconcat . map conv . B.unpack
          where conv '\n' = BL.byteString "\r\n"
                conv a = BL.char8 a
