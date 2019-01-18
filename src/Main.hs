{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Main where

import           Codec.Archive.Zip
import           Data.Maybe
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
import           Text.HTML.DOM
import           Text.XML.Cursor
import           Text.XML.Scraping
import           Text.XML.Selector.TH

main :: IO ()
main = do
  dicInfo <- getDicInfo
  dicNico <- getDicNico
  dicPixiv <- getDicPixiv

  T.putStrLn dicInfo
  mapM_ (\(yomi, word) -> T.putStrLn $ yomi <> "\t" <> word <> "\t" <> "固有名詞") $
    S.filter
    (\(yomi, word) -> dictionaryWord yomi word && S.member word dicPixiv) dicNico

getDicInfo :: IO T.Text
getDicInfo = do
  time <- formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S%z" <$> getZonedTime
  return $ T.unlines
    [ "#name: dic-nico-intersection-pixiv"
    , "#description: ニコニコ大百科とピクシブ百科事典の共通部分の辞書"
    , "#github: https://github.com/ncaq/dic-nico-intersection-pixiv"
    , "#createdAt: " <> toTextStrict time
    , "#copying:"
    , "#nicovideo: https://dic.nicovideo.jp/"
    , "#nicoime: http://tkido.com/blog/1019.html"
    , "#nicodicYomi: https://dic.nicovideo.jp/id/4652210"
    , "#pixiv: https://dic.pixiv.net/"
    ]

getDicNico :: IO (S.Set (T.Text, T.Text))
getDicNico = do
  Archive{zEntries = [_, msimeEntry@Entry{eRelativePath = "nicoime_msime.txt"}]} <-
    toArchive . getResponseBody <$> httpLBS "http://tkido.com/data/nicoime.zip"
  excludeWords <- getSpecialYomiViaNicoVideo
  return .
    S.filter (\(_, word) -> word `notElem` excludeWords) .
    S.map (\[yomi, word, _] -> (normalize NFKC yomi, replaceSymbol $ normalize NFKC word)) .
    S.map (T.split ('\t' ==)) . S.fromList . drop 8 . T.lines . toTextStrict . TL.decodeUtf16LE $
    fromEntry msimeEntry

getDicPixiv :: IO (S.Set T.Text)
getDicPixiv = do
  sitemap <- fromDocument . parseLBS . getResponseBody <$>
    httpLBS "https://dic.pixiv.net/sitemap/"
  sitemaps <- mapM (\loc ->
              fromDocument . parseLBS . getResponseBody <$>
              httpLBS (parseRequest_ (toString (innerText loc)))) $
    queryT [jq|loc|] sitemap
  return $ S.fromList $
    map (replaceSymbol . normalize NFKC . toTextStrict . urlDecode False . toByteStringStrict) $
    mapMaybe (T.stripPrefix "https://dic.pixiv.net/a/") $
    concatMap (map (toTextStrict . innerText) . queryT [jq|loc|]) sitemaps

-- | [読みが通常の読み方とは異なる記事の一覧とは (ニコチュウジチョウシロとは) [単語記事] - ニコニコ大百科](http://dic.nicovideo.jp/id/4652210)
-- による読みが異なる単語の一覧
-- 括弧を含む単語をうまく扱えないですがどうせ括弧入りの単語は除外するから考慮しない
getSpecialYomiViaNicoVideo :: IO (S.Set T.Text)
getSpecialYomiViaNicoVideo = do
  doc <- fromDocument . parseLBS . getResponseBody <$> httpLBS "http://dic.nicovideo.jp/id/4652210"
  return $ S.fromList $
    replaceSymbol . normalize NFKC . T.takeWhile (/= '（') . toTextStrict . innerText <$>
    queryT [jq|#article > ul > li|] doc

replaceSymbol :: T.Text -> T.Text
replaceSymbol = T.replace "···" "…"

-- | 読みで遊んでいたり曖昧さ回避の結果など辞書に適さない単語を排除する
dictionaryWord :: T.Text -> T.Text -> Bool
dictionaryWord yomi word = and
  [ T.length yomi < 25                -- 読みが異様に長くない
  , T.length word < T.length yomi * 3 -- 単語が読みに比べて異様に長くない
  , T.length yomi < T.length word * 6 -- 読みが単語に比べて異様に長くない
  , T.all ('(' /=) word               -- 括弧を含まない
    -- 単語の最後が兄貴ではないもしくは含んでも読みが最後に設定されている
  , not ("兄貴" `T.isSuffixOf` word) || ("あにき" `T.isSuffixOf` word)
    -- 単語の最後が姉貴ではないもしくは含んでも読みが最後に設定されている
  , not ("姉貴" `T.isSuffixOf` word) || ("あねき" `T.isSuffixOf` word)
  ]
