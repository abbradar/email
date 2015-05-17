module Network.Email.Render where

mail :: Mail -> BL.Builder
mail ml = hdrs <> BL.byteString "\r\n" <> body <> parts
  where hdrs = BL.lazyByteString $ TL.encodeUtf8 $ TL.toLazyText $ E.outputHeaders $ mailHeaders ml
        body = BL.lazyByteString $ mailBody ml
        parts = case R.boundary (mailHeaders ml) of
          Nothing -> mempty
          Just x -> let bnd = BL.byteString "\r\n--" <> BL.byteString x
                       ps = mconcat $ map (\p -> bnd <> mail p) mailParts ml
                   in ps <> bnd <> BL.byteString "--\r\n"

-- | Encode binary body using headers, return new 'Mail'.
encodeBinary :: Mail -> BL.ByteString -> Mail

-- | Encode text body using headers, return new 'Mail'.
encodeText :: Mail -> TL.Text -> Mail
