module Main where

import           Codec.Archive.Zip
import           Control.Monad
import qualified Data.ByteString.Char8      as B
import qualified Data.ByteString.Lazy.Char8 as BL8
import           Data.Monoid
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as T
import           Data.Text.ICU
import qualified Data.Text.IO               as T
import           Network.HTTP.Simple
import           Network.HTTP.Types

main :: IO ()
main = do
    Archive { zEntries = [ entryMsime@Entry {eRelativePath = "nicoime_msime.txt"}, _ ] } <-
        toArchive . getResponseBody <$> httpLBS "http://tkido.com/data/nicoime.zip"
    l <- filterM (\(_, w) -> (\r -> getResponseStatusCode r == 200) <$>
                     httpNoBody
                     (parseRequest_
                      (B.unpack ("http://dic.pixiv.net/a/" <> urlEncode False (T.encodeUtf8 w))))) .
         map (\[y, w, _] -> (normalize NFKC y, normalize NFKC w)) .
         map (T.split ('\t' ==)) . drop 8 . T.lines . T.decodeUtf16LE . BL8.toStrict $
         fromEntry entryMsime
    mapM_ (\(y, w) -> T.putStrLn $ y <> "\t" <> w <> "\t" <> "固有一般") l
