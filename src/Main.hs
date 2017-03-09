module Main where

import           Codec.Archive.Zip
import qualified Data.ByteString.Lazy.Char8 as L8
import           Data.Text.ICU
import           Data.Text.ICU.Convert
import           Network.HTTP.Simple

main :: IO ()
main = do
    Archive { zEntries = [ entryAtok@Entry {eRelativePath = "nicoime_atok.txt"}, _ ] } <-
        toArchive . getResponseBody <$> httpLBS "http://tkido.com/data/nicoime.zip"
    conv <- open "Shift-JIS" Nothing
    let nicoimeAtok = toUnicode conv $ L8.toStrict $ fromEntry entryAtok
