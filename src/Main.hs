{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Main where

import           Codec.Archive.Zip
import           Data.Maybe
import           Data.Monoid
import qualified Data.Set                as S
import           Data.String.Transform
import qualified Data.Text               as T
import qualified Data.Text.IO            as T
import qualified Data.Text.Lazy.Encoding as TL
import           Data.Text.Normalize
import           Data.Time.Format
import           Data.Time.LocalTime
import           Network.HTTP.Simple
import           Network.HTTP.Types
import           Text.XML
import           Text.XML.Cursor
import           Text.XML.Scraping
import           Text.XML.Selector.TH

main :: IO ()
main = do
    dicInfo <- getDicInfo
    dicNico <- getDicNico
    dicPixiv <- getDicPixiv

    T.putStrLn dicInfo
    mapM_ (\(y, w) -> T.putStrLn $ y <> "\t" <> w <> "\t" <> "固有名詞") $
        S.filter (\(y, w) -> dictionaryWord y w && S.member w dicPixiv) dicNico

getDicInfo :: IO T.Text
getDicInfo = do
    time <- formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S%z" <$> getZonedTime
    return $ T.unlines
        [ "#name: dic-nico-intersection-pixiv"
        , "#description: ニコニコ大百科とピクシブ百科事典の共通部分の辞書"
        , "#github: https://github.com/ncaq/dic-nico-intersection-pixiv"
        , "#createdAt: " <> toTextStrict time
        , "#copying:"
        , "#nicovideo: http://dic.nicovideo.jp/"
        , "#nicoime: http://tkido.com/blog/1019.html"
        , "#pixiv: https://dic.pixiv.net/"
        ]

getDicNico :: IO (S.Set (T.Text, T.Text))
getDicNico = do
    Archive{zEntries = [_, msimeEntry@Entry{eRelativePath = "nicoime_msime.txt"}]} <-
        toArchive . getResponseBody <$> httpLBS "http://tkido.com/data/nicoime.zip"
    return . S.map (\[y, w, _] -> (normalize NFKC y, replaceSymbol $ normalize NFKC w)) .
        S.map (T.split ('\t' ==)) . S.fromList . drop 8 . T.lines . toTextStrict . TL.decodeUtf16LE $
        fromEntry msimeEntry

getDicPixiv :: IO (S.Set T.Text)
getDicPixiv = do
    sitemap <- fromDocument . parseLBS_ def . getResponseBody <$>
        httpLBS "https://dic.pixiv.net/sitemap/"
    sitemaps <- mapM (\loc ->
                          fromDocument . parseLBS_ def . getResponseBody <$>
                          httpLBS (parseRequest_ (toString (innerText loc)))) $
        queryT [jq|loc|] sitemap
    return $ S.fromList $
        map (replaceSymbol . normalize NFKC . toTextStrict . urlDecode False . toByteStringStrict) $
        mapMaybe (T.stripPrefix "https://dic.pixiv.net/a/") $
        concatMap (map (toTextStrict . innerText) . queryT [jq|loc|]) sitemaps

replaceSymbol :: T.Text -> T.Text
replaceSymbol = T.replace "···" "…" . T.replace "・" "·"

-- | 読みで遊んでいたり曖昧さ回避の結果など辞書に適さない単語を排除する
dictionaryWord :: T.Text -> T.Text -> Bool
dictionaryWord y w
    = T.length y < 20 &&             -- 読みが異様に長くない
      T.length w < T.length y * 3 && -- 単語が読みに比べて異様に異様に長くない
      T.length y < T.length w * 4 && -- 読みが単語に比べて異様に長くない
      T.all ('(' /=) w               -- 括弧を含まない
