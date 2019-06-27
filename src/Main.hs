{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Main where

import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Monad
import           Control.Parallel.Strategies
import           Data.Hashable
import qualified Data.HashSet                as S
import           Data.List                   hiding (words)
import qualified Data.Map                    as M
import           Data.Maybe
import           Data.String                 hiding (words)
import           Data.String.Transform
import qualified Data.Text                   as T
import           Data.Text.ICU.Translit
import qualified Data.Text.IO                as T
import qualified Data.Text.Lazy              as TL
import           Data.Text.Normalize
import           Data.Time.Format
import           Data.Time.LocalTime
import           GHC.Generics                (Generic)
import           Network.HTTP.Simple
import           Network.HTTP.Types
import           Prelude                     hiding (words)
import           System.Directory
import           Text.HTML.DOM
import           Text.XML                    hiding (parseLBS)
import           Text.XML.Cursor
import           Text.XML.Scraping
import           Text.XML.Selector.TH

-- | 名前が雑すぎますが
-- これはライブラリじゃなくてアプリケーションなので汎用的な名前を付けてしまう
data Entry
  = Entry
  { entryYomi     :: !T.Text
  , entryWord     :: !T.Text
  , entryRedirect :: !Bool
  } deriving (Eq, Ord, Show, Read, Generic, NFData)

instance Hashable Entry

main :: IO ()
main = do
  createDirectoryIfMissing False "cache"

  dicInfo <- getDicInfo
  dicNico <- getDicNico
  dicNicoSpecialYomi <- getDicNicoSpecialYomi
  dicPixiv <- getDicPixiv

  T.putStrLn dicInfo

  let dictionaryFilter = S.filter $ dictionaryWord dicNico dicNicoSpecialYomi dicPixiv
      dictionarySet = dictionaryFilter dicNico
      -- 焼け石に水ですが一応並列評価する
      dictionaryList = sortOn entryYomi (S.toList dictionarySet) `using` parList rdeepseq

  mapM_ (\Entry{entryYomi, entryWord} -> T.putStrLn $ entryYomi <> "\t" <> entryWord <> "\t" <> "固有名詞") dictionaryList

-- | 生成日を含めたこのデータの情報を表示する
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

-- | [50音順単語記事一覧 - ニコニコ大百科](https://dic.nicovideo.jp/m/a/a)
-- から単語と読み一覧を取得する
getDicNico :: IO (S.HashSet Entry)
getDicNico = do
  let path = "cache/jp-nicovideo-dic.txt"
  exist <- doesFileExist path
  if exist
    then read <$> Prelude.readFile path
    else do
    doc <- fromDocument . parseLBS . getResponseBody <$> httpLBS "https://dic.nicovideo.jp/m/a/a"
    let chars = map TL.head $ filter (\text -> TL.length text == 1) $ map innerText $
          queryT [jq|.st-box_contents > table > tr > td > a|] doc
    dic <- mconcat <$> mapM getDicNicoTitle chars
    Prelude.writeFile path $ show dic
    return dic

-- | [「ア」から始まる50音順単語記事タイトル表示 - ニコニコ大百科](https://dic.nicovideo.jp/m/yp/a/%E3%82%A2)
-- のような記事からページャを辿って再帰的にデータを取得する
getDicNicoTitle :: Char -> IO (S.HashSet Entry)
getDicNicoTitle c = getDicNicoPage $ "https://dic.nicovideo.jp/m/yp/a/" <> toString (urlEncode False (toByteStringStrict [c]))

-- | ページャを辿っていくのでURLから取得したほうが都合が良いので別関数化して再帰する
getDicNicoPage :: String -> IO (S.HashSet Entry)
getDicNicoPage href = do
  -- BAN回避のため0.1秒スリープ
  threadDelay $ 100 * 1000
  response <- do
    response0 <- httpLBS (fromString href)
    if getResponseStatus response0 == status200
      then return response0
      else do
      -- ニコニコ大百科のサーバはしばしば壊れてランダムに通信に失敗するので,失敗した場合もう一度だけリトライする
      -- 1秒スリープ
      threadDelay $ 1000 * 1000
      response1 <- httpLBS (fromString href)
      unless (getResponseStatus response1 == status200) $
        error $ "ニコニコ大百科 " <> href <> " が取得できませんでした: " <> show response1
      return response1
  let doc = fromDocument $ parseLBS $ getResponseBody response
      articles = queryT [jq|.article ul ul li|] doc
      texts = map (TL.strip . innerText) articles
      words = map (TL.strip . innerText . queryT [jq|a|]) articles
      extras = map (\(word, text) -> fromJust $ TL.strip <$> TL.stripPrefix word text) $ zip words texts
      dic = S.fromList $ map
            (\(word, extra) ->
                let yomi = TL.takeWhile (/= ')') $ fromJust $ TL.stripPrefix "(" extra
                    -- icuで変換,長音記号が変換されてしまうので誤魔化す
                    hiraganaYomi = T.replace "!" "ー" $ transliterate (trans "Katakana-Hiragana") $
                      T.replace "ー" "!" $ toTextStrict yomi
                in Entry
                   { entryWord = normalizeWord $ toTextStrict word
                   , entryYomi = normalizeWord hiraganaYomi
                   , entryRedirect = "(リダイレクト)" `TL.isInfixOf` extra
                   }) $ zip words extras
      nodes = node <$> queryT [jq|div.st-pg div.st-pg_contents a.navi|] doc
  when (S.null dic) $ error $ "ニコニコ大百科 " <> href <> " で単語が取得できませんでした: " <> show dic
  nextDic <-
        case find (\case
                      NodeElement Element{elementNodes} -> elementNodes == [NodeContent "次へ »"]
                      _ -> False
                  ) nodes of
          Just (NodeElement (Element _ attrs _)) ->
            case M.lookup "href" attrs of
              Nothing -> return Nothing
              Just newHref -> Just <$> getDicNicoPage ("https://dic.nicovideo.jp" <> toString newHref)
          _ -> return Nothing
  return $ dic <> fromMaybe S.empty nextDic

-- | [読みが通常の読み方とは異なる記事の一覧とは (ニコチュウジチョウシロとは) [単語記事] - ニコニコ大百科](https://dic.nicovideo.jp/a/%E8%AA%AD%E3%81%BF%E3%81%8C%E9%80%9A%E5%B8%B8%E3%81%AE%E8%AA%AD%E3%81%BF%E6%96%B9%E3%81%A8%E3%81%AF%E7%95%B0%E3%81%AA%E3%82%8B%E8%A8%98%E4%BA%8B%E3%81%AE%E4%B8%80%E8%A6%A7)
-- による読みが異なる単語の一覧
-- 括弧を含む単語をうまく扱えないですがどうせ括弧入りの単語は除外するから考慮しない
getDicNicoSpecialYomi :: IO (S.HashSet T.Text)
getDicNicoSpecialYomi = do
  let path = "cache/jp-nicovideo-dic-id-4652210.txt"
  exist <- doesFileExist path
  if exist
    then read <$> Prelude.readFile path
    else do
    doc <- fromDocument . parseLBS . getResponseBody <$> httpLBS "https://dic.nicovideo.jp/id/4652210"
    let dic = S.fromList $ normalizeWord . T.takeWhile (/= '（') . toTextStrict . innerText <$>
              queryT [jq|#article ul li|] doc
    Prelude.writeFile path $ show dic
    return dic

-- | Pixiv百科時点のサイトマップから記事一覧データを取得する
getDicPixiv :: IO (S.HashSet T.Text)
getDicPixiv = do
  let path = "cache/net-pixiv-dic.txt"
  exist <- doesFileExist path
  if exist
    then read <$> Prelude.readFile path
    else do
    sitemap <- fromDocument . parseLBS . getResponseBody <$> httpLBS "https://dic.pixiv.net/sitemap/"
    sitemaps <- mapConcurrently
      (\loc -> fromDocument . parseLBS . getResponseBody <$> httpLBS (parseRequest_ (toString (innerText loc)))) $
      queryT [jq|loc|] sitemap
    let dic = S.fromList $
          map (normalizeWord . toTextStrict . urlDecode False . toByteStringStrict) $
          mapMaybe (TL.stripPrefix "https://dic.pixiv.net/a/") $
          concatMap (map innerText . queryT [jq|loc|]) sitemaps
    Prelude.writeFile path $ show dic
    return dic

-- | 一致しているかで判定を行う箇所が多数存在するのでなるべく正規化する
normalizeWord :: T.Text -> T.Text
normalizeWord = replaceSymbol . normalize NFKC

-- | 中黒で三点リーダを表現しようとしているのを変換
replaceSymbol :: T.Text -> T.Text
replaceSymbol = T.replace "···" "…"

-- | 辞書に適している単語を抽出する
dictionaryWord :: S.HashSet Entry -> S.HashSet T.Text -> S.HashSet T.Text -> Entry -> Bool
dictionaryWord dicNico dicNicoSpecialYomi dicPixiv Entry{entryYomi, entryWord, entryRedirect} = and
  [ entryWord `S.member` dicPixiv                 -- Pixiv百科時点にも存在する
  , not (entryWord `S.member` dicNicoSpecialYomi) -- 特殊な読みではない
    -- 読みが異様に短くない
  , 1 < T.length entryYomi
    -- 読みが異様に長くない
  , T.length entryYomi < 25
    -- 単語が読みに比べて異様に長くない
  , T.length entryWord < T.length entryYomi * 3
    -- 読みが単語に比べて異様に長くない
  , T.length entryYomi < T.length entryWord * 6
    -- 括弧を含まない
  , T.all ('(' /=) entryWord
    -- マジで?など読みが3文字以下で単語が?で終わるやつは排除
  , not (T.length entryYomi <= 3 && T.last entryWord == '?')
    -- いま!など読みが3文字以下で単語が!で終わるやつは排除
  , not (T.length entryYomi <= 3 && T.last entryWord == '!')
    -- 曖昧さ回避用
  , not ("あいまいさ" `T.isInfixOf` entryYomi)
    -- 一覧
  , not ("一覧" `T.isSuffixOf` entryWord)
    -- 画像集
  , not ("画像集" `T.isSuffixOf` entryWord)
    -- けものフレンズ
  , not ("けものふれんずの" `T.isPrefixOf` entryYomi)
    -- アズールレーン
  , not ("あずれんの" `T.isPrefixOf` entryYomi)
    -- 戦国BASARA
  , not ("せんごくばさら" `T.isSuffixOf` entryYomi)
    -- 単語の最後が兄貴の場合読みも兄貴で終わる
  , not ("兄貴" `T.isSuffixOf` entryWord) || ("あにき" `T.isSuffixOf` entryWord)
    -- 単語の最後が姉貴の場合読みも姉貴で終わる
  , not ("姉貴" `T.isSuffixOf` entryWord) || ("あねき" `T.isSuffixOf` entryWord)
    -- 記事名に実況を含まないのにも関らず読みで実況者を表現しようとしている記事を除外
  , not (not ("実況" `T.isInfixOf` entryWord) && "じっきょう" `T.isInfixOf` entryYomi)
    -- 映画
  , not (not ("映画" `T.isInfixOf` entryWord) && "えいが" `T.isInfixOf` entryYomi)
    -- アニメ
  , not (not ("アニメ" `T.isInfixOf` entryWord) && "あにめ" `T.isInfixOf` entryYomi)
    -- 絵師
  , not (not ("絵師" `T.isInfixOf` entryWord) && "えし" `T.isInfixOf` entryYomi)
    -- 誤変換指摘対策,同一のリダイレクト記事ではない読みが他に存在するリダイレクト項目は出力しない
  , not entryRedirect ||
    not (S.null (S.filter (\Entry{entryYomi = otherYomi, entryRedirect = otherRedirect} ->
                             not otherRedirect && otherYomi == entryYomi) dicNico))
  ]
