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
import           System.Directory
import           Text.HTML.DOM
import           Text.XML.Cursor
import           Text.XML.Scraping
import           Text.XML.Selector.TH

main :: IO ()
main = do
  createDirectoryIfMissing False "cache"

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
    S.map (\x -> case x of
              [yomi, word, _] -> (normalize NFKC yomi, replaceSymbol $ normalize NFKC word)
              _ -> error "ニコニコ大百科IME辞書の単語分割に失敗しました"
          ) .
    S.map (T.split ('\t' ==)) . S.fromList . drop 8 . T.lines . toTextStrict . TL.decodeUtf16LE $
    fromEntry msimeEntry

getDicPixiv :: IO (S.Set T.Text)
getDicPixiv = do
  let path = "cache/net-pixiv-dic.txt"
  exist <- doesFileExist path
  if exist
    then read . toString <$> T.readFile path
    else do
    sitemap <- fromDocument . parseLBS . getResponseBody <$>
      httpLBS "https://dic.pixiv.net/sitemap/"
    sitemaps <- mapM (\loc ->
                fromDocument . parseLBS . getResponseBody <$>
                httpLBS (parseRequest_ (toString (innerText loc)))) $
      queryT [jq|loc|] sitemap
    let dic = S.fromList $
          map (replaceSymbol . normalize NFKC . toTextStrict . urlDecode False . toByteStringStrict) $
          mapMaybe (T.stripPrefix "https://dic.pixiv.net/a/") $
          concatMap (map (toTextStrict . innerText) . queryT [jq|loc|]) sitemaps
    T.writeFile path $ toTextStrict $ show dic
    return dic

-- | [読みが通常の読み方とは異なる記事の一覧とは (ニコチュウジチョウシロとは) [単語記事] - ニコニコ大百科](https://dic.nicovideo.jp/a/%E8%AA%AD%E3%81%BF%E3%81%8C%E9%80%9A%E5%B8%B8%E3%81%AE%E8%AA%AD%E3%81%BF%E6%96%B9%E3%81%A8%E3%81%AF%E7%95%B0%E3%81%AA%E3%82%8B%E8%A8%98%E4%BA%8B%E3%81%AE%E4%B8%80%E8%A6%A7)
-- による読みが異なる単語の一覧
-- 括弧を含む単語をうまく扱えないですがどうせ括弧入りの単語は除外するから考慮しない
getSpecialYomiViaNicoVideo :: IO (S.Set T.Text)
getSpecialYomiViaNicoVideo = do
  let path = "cache/jp-nicovideo-dic-id-4652210.txt"
  exist <- doesFileExist path
  if exist
    then read . toString <$> T.readFile path
    else do
    doc <- fromDocument . parseLBS . getResponseBody <$> httpLBS "https://dic.nicovideo.jp/id/4652210"
    let dic = S.fromList $
          replaceSymbol . normalize NFKC . T.takeWhile (/= '（') . toTextStrict . innerText <$>
          queryT [jq|#article > ul > li|] doc
    T.writeFile path $ toTextStrict $ show dic
    return dic

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
    -- マジで?など読みが3文字以下で単語が?で終わるやつ
  , not (T.length yomi <= 3 && T.last word == '?')
    -- 曖昧さ回避用の記事
  , not ("あいまいさ" `T.isInfixOf` yomi)
    -- けものフレンズの記事はご丁寧に何故か読みにけものフレンズとつけているので排除
  , not ("けものふれんずの" `T.isPrefixOf` yomi)
    -- アズールレーン
  , not ("あずれんの" `T.isPrefixOf` yomi)
    -- 戦国BASARA
  , not ("せんごくばさら" `T.isSuffixOf` yomi)
    -- 記事名に実況を含まないのにも関らず読みで実況者を表現しようとしている記事を除外
  , not (not ("実況" `T.isInfixOf` word) && "じっきょう" `T.isInfixOf` yomi)
    -- 映画
  , not (not ("映画" `T.isInfixOf` word) && "えいが" `T.isInfixOf` yomi)
    -- アニメ
  , not (not ("アニメ" `T.isInfixOf` word) && "あにめ" `T.isInfixOf` yomi)
    -- 絵師
  , not (not ("絵師" `T.isInfixOf` word) && "えし" `T.isInfixOf` yomi)
  ]
