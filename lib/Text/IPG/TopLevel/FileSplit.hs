{-# LANGUAGE OverloadedStrings #-}
module Text.IPG.TopLevel.FileSplit ( splitFile ) where
import qualified Data.ByteString as BS -- bytestring
import qualified Data.ByteString.Lazy.Char8 as LBS -- bytestring

import Data.ByteString.Lazy.Search ( breakOn ) -- stringsearch

splitAround :: BS.ByteString -> LBS.ByteString -> Maybe (LBS.ByteString, LBS.ByteString)
splitAround pattern s
    = if LBS.null after then Nothing
                        else Just (before, LBS.drop (fromIntegral $ BS.length pattern) after)
  where (before, after) = breakOn pattern s

splitFile :: LBS.ByteString -> (LBS.ByteString, LBS.ByteString, LBS.ByteString)
splitFile input' = (preamble, input, postamble)
    where (preamble, rest) = case splitAround "\n%preamble_end" input' of
            Nothing -> ("", input')
            Just (x, p) -> (x, LBS.drop 1 (LBS.dropWhile ('\n' /=) p))
          (input, postamble) = case splitAround "\n%postamble_begin" rest of
            Nothing -> (rest, "")
            Just (x, p) -> (x, LBS.dropWhile isSpace p)
          isSpace c = c `elem` (" \n\r\t" :: String)
