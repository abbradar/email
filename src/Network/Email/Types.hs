module Network.Email.Types where

import Control.Applicative
import Control.Exception (Exception)
import Data.Typeable
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Text.Lazy as TL
import Data.CaseInsensitive (CI)

import Network.Email.Header.Types

-- | Mail type, essentially a tree of mails with headers and bodies.
data Mail = SimpleMail { mailHeaders :: Headers
                       , mailBody :: BL.ByteString
                       }
          | MultipartMail { mailHeaders :: Headers
                          , mailParts :: [Mail]
                          }
          deriving (Show, Eq)

-- | Email exceptions.
data MailException
  -- | A header field could not be parsed.
  = DecodingError String
  -- | Unknown Content-Transfer-Encoding.
  | UnknownEncoding (CI B.ByteString)
  -- | Unknown charset for text/* Content-Type.
  | UnknownCharset String
  -- | Tried to parse binary content as a text.
  | NotAText
  deriving (Show, Typeable)

instance Exception MailException

-- | Path to one part of multipart message.
type Path = [Int]

-- | Focus onto part of the 'Mail'.
mailPath :: Applicative f => Path -> (Mail -> f Mail) -> Mail -> f Mail
mailPath [] f m = f m
mailPath (h:t) f (SimpleMail hs b) = SimpleMail <$> pure hs <*> pure b
mailPath (h:t) f (MultipartMail hs ps) = MultipartMail <$> pure hs <*> (listLens h . mailPath t) f ps
  where listLens _ _ [] = pure []
        listLens 0 f (h:t) = (:) <$> f h <*> pure t
        listLens n f (h:t) = (:) <$> pure h <*> listLens (n - 1) f t
