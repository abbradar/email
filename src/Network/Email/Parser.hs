module Network.Email.Parser
       ( -- | Encodings
         quotedPrintable
       , base64
       , crlfText
       , -- | Mail
         headers
       , mail
       ) where

import Prelude hiding (takeWhile)
import Data.List hiding (takeWhile)
import Data.Maybe
import Data.Monoid
import Control.Applicative
import Control.Monad.Catch
import qualified Data.Map as M
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Builder as BL
import qualified Data.ByteString.Base64.Lazy as B64
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import qualified Data.Text.Encoding.Error as T
import qualified Data.Text.Lazy.Builder as TB
import qualified Data.Attoparsec.Text.Lazy as TL
import Data.Attoparsec.ByteString.Char8
import qualified Data.Attoparsec.Text as T
import qualified Network.Email.Header.Parser as E
import qualified Network.Email.Header.Read as E

import Network.Email.Types

eof :: Parser ()
eof = () <$ string "\r\n" <|> endOfInput

-- | Parses lines delimited by @delim@. The builder is concatenated to each string.
--   When @delim@ returns Nothing, read the next line, otherwise return its last
--   state.
delimited :: Parser (BL.Builder, Maybe a) -> Parser (BL.Builder, a)
delimited delim = do
  this <- line
  (del, r') <- delim
  case r' of
   Nothing -> do
     (next, r) <- delimited delim
     return (this <> del <> next, r)
   Just r -> return (this <> del, r)

  where line = do
          this <- BL.byteString <$> takeTill (/= '\r')
          next <- mempty <$ eof
                 <|> (<>) <$> (BL.char8 <$> anyChar) <*> line
          return $ this <> next

-- | Run a 'Text' parser for a UTF-8 encoded 'ByteString' inside a 'ByteString' 'Parser'.
parseUtf8 :: TL.Parser a -> BL.ByteString -> Parser a
parseUtf8 p s = do
  t <- case TL.decodeUtf8' s of
    Left (T.DecodeError e _) -> fail e
    Left (T.EncodeError _ _) -> error "parseUtf8: can't receive EncodeError"
    Right r -> return r
  case TL.parse (p <* T.endOfInput) t of
    TL.Fail _ ctx e -> let e' = fromMaybe e (stripPrefix "Failed reading: " e)
                      in foldl (<?>) (fail e') ctx
    TL.Done _ r -> return r

-- | Parse e-mail headers in bulk. Does not support obsolete quoting style
--   (which allows \r\n to be quoted).
headers :: Parser BL.Builder
headers = fst <$> delimited newline
  where newline = (,) <$> (BL.byteString <$> string "\r\n") <*> optional (string "\r\n")

-- | Parses MIME boundary. Returns 'True' if the end of multipart message was
--   met, 'False' otherwise.
boundary :: B.ByteString -> Parser (BL.Builder, Maybe Bool)
boundary delim = do
  r <- string "\r\n"
  (mempty, ) <$> Just <$> bnd
    <|> return (BL.byteString r, Nothing)

  where bnd = do
          _ <- string "--"
          _ <- string delim
          False <$ eof
            <|> True <$ string "--" <* eof

-- | Parses a part of a multi-part e-mail, reading body and final delimiter with @body@.
mailPart :: Parser (BL.Builder, a) -> Parser (Mail, a)
mailPart body = do
  hdr' <- BL.toLazyByteString <$> headers
  mailHeaders <- parseUtf8 E.headers hdr'
  case E.boundary mailHeaders of
   Nothing -> do
     (dat, r) <- body
     let mail = Mail { mailParts = []
                     , mailBody = BL.toLazyByteString dat
                     , ..
                     }
     return (mail, r)
   Just bnd -> do
     let parser = delimited $ boundary bnd
         read True = return []
         read False = do
           (part, r) <- mailPart parser
           (part:) <$> read r
     (_, r') <- parser
     mailParts <- read r'
     (_, r) <- body
     let mail = Mail { mailBody = ""
                     , ..
                     }
     return (mail, r)

-- | Parses e-mail according to RFC 5322.
mail :: Parser Mail
mail = fst <$> mailPart ((, ()) <$> BL.lazyByteString <$> takeLazyByteString)

-- | Parses quoted-printable encoded bytestring.
quotedPrintable :: Parser BL.Builder
quotedPrintable = mconcat <$> intersperse (BL.byteString "\r\n") <$> line `sepBy` string "\r\n"
  where line = mconcat <$> (padding >> body) `sepBy` (char '=' >> padding >> string "\r\n")
        padding = skipWhile (inClass " \t")
        body = BL.byteString <$> takeWhile1 (/= '=')
               <|> BL.word8 <$ char '=' <*> E.hexPair

-- | Parses base64 encoded bytestring.
base64 :: Parser BL.ByteString
base64 = do
  str <- BL.toLazyByteString <$> mconcat <$> (BL.byteString <$> takeWhile (/= '\r')) `sepBy` string "\r\n"
  E.parseEither $ B64.decode str

-- | Parses Unicode text with CRLF endings, converting them to LF.
crlfText :: T.Parser TL.Text
crlfText = TB.toLazyText <$> mconcat <$> intersperse (TB.singleton '\n') <$> line `sepBy` T.string "\r\n"
  where line = TB.fromText <$> T.takeWhile1 (/= '\r')
