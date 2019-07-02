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
import qualified Data.ByteString             as B
import           Data.Hashable
import qualified Data.HashSet                as H
import           Data.List                   hiding (words)
import qualified Data.Map.Strict             as M
import           Data.Maybe
import           Data.Store
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
  } deriving (Eq, Show, Generic)

instance Hashable Entry
instance Store Entry

main :: IO ()
main = do
  -- キャッシュディレクトリがなければ作成する
  createDirectoryIfMissing False "cache"

  dicInfo <- getDicInfo
  -- ニコニコ大百科とPixiv百科時点は読み込み元が違うので安全に並行で取得できる
  (dicNico, dicPixiv) <- concurrently getDicNico getDicPixiv
  -- ニコニコ大百科同士で並列化すると読み込みすぎになりかねないのでこれは単独で読み込む
  -- キャッシュされている場合もそんなに容量が大きくないのでデシリアライズに時間を使わない
  dicNicoSpecialYomi <- getDicNicoSpecialYomi

  -- 参考情報をプリント
  T.putStrLn dicInfo

      -- 最適化がかかることを期待するのとghciでのデバッグを楽にするために部分適用
  let dictionaryWordPred = dictionaryWord dicNico dicNicoSpecialYomi dicPixiv
      -- フィルタリング処理関数を生成
      dictionaryFilter = filter dictionaryWordPred
      -- リスト化して並列フィルタリング処理(気休め)
      -- HashSetは順番バラバラなので最終的にソートする
      dictionarySorted = sortOn entryYomi (dictionaryFilter (H.toList dicNico)) `using` parList rseq

  -- 辞書本体をプリント
  mapM_ (\Entry{entryYomi, entryWord} -> T.putStrLn $ entryYomi <> "\t" <> entryWord <> "\t" <> "固有名詞") dictionarySorted

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
    , "#pixiv: https://dic.pixiv.net/"
    ]

-- | [50音順単語記事一覧 - ニコニコ大百科](https://dic.nicovideo.jp/m/a/a)
-- から単語と読み一覧を取得する
getDicNico :: IO (H.HashSet Entry)
getDicNico = do
  let path = "cache/jp-nicovideo-dic"
  exist <- doesFileExist path
  if exist
    then B.readFile path >>= decodeIO
    else do
    doc <- fromDocument . parseLBS . getResponseBody <$> httpLBS "https://dic.nicovideo.jp/m/a/a"
    let chars = map TL.head $ filter (\text -> TL.length text == 1) $ map innerText $
          queryT [jq|.st-box_contents > table > tr > td > a|] doc
    dic <- mconcat <$> mapM getDicNicoTitle chars
    B.writeFile path $ encode dic
    return dic

-- | [「ア」から始まる50音順単語記事タイトル表示 - ニコニコ大百科](https://dic.nicovideo.jp/m/yp/a/%E3%82%A2)
-- のような記事からページャを辿って再帰的にデータを取得する
getDicNicoTitle :: Char -> IO (H.HashSet Entry)
getDicNicoTitle c = getDicNicoPage $ "https://dic.nicovideo.jp/m/yp/a/" <> toString (urlEncode False (toByteStringStrict [c]))

-- | ページャを辿っていくのでURLから取得したほうが都合が良いので別関数化して再帰する
getDicNicoPage :: String -> IO (H.HashSet Entry)
getDicNicoPage href = do
  -- BAN回避のため0.01秒スリープ
  threadDelay $ 10 * 1000
  response <- do
    response0 <- httpLBS (fromString href)
    if getResponseStatus response0 == status200
      then return response0
      else do
      -- ニコニコ大百科のサーバはしばしば壊れてランダムに通信に失敗するので失敗した場合もう一度だけリトライする
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
      dic = H.fromList $ map
            (\(word, extra) ->
                let yomi = TL.takeWhile (/= ')') $ fromJust $ TL.stripPrefix "(" extra
                    -- icuで変換
                    -- 長音記号が変換されてしまうのでカタカナには使われない文字を使って誤魔化す
                    hiraganaYomi = T.replace "!" "ー" $ transliterate (trans "Katakana-Hiragana") $
                      T.replace "ー" "!" $ toTextStrict yomi
                in Entry
                   { entryWord = normalizeWord $ toTextStrict word
                   , entryYomi = normalizeWord hiraganaYomi
                   , entryRedirect = "(リダイレクト)" `TL.isInfixOf` extra
                   }) $ zip words extras
      nodes = node <$> queryT [jq|div.st-pg div.st-pg_contents a.navi|] doc
  when (H.null dic) $ error $ "ニコニコ大百科 " <> href <> " で単語が取得できませんでした: " <> show dic
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
  return $ dic <> fromMaybe H.empty nextDic

-- | [読みが通常の読み方とは異なる記事の一覧とは (ニコチュウジチョウシロとは) [単語記事] - ニコニコ大百科](https://dic.nicovideo.jp/a/%E8%AA%AD%E3%81%BF%E3%81%8C%E9%80%9A%E5%B8%B8%E3%81%AE%E8%AA%AD%E3%81%BF%E6%96%B9%E3%81%A8%E3%81%AF%E7%95%B0%E3%81%AA%E3%82%8B%E8%A8%98%E4%BA%8B%E3%81%AE%E4%B8%80%E8%A6%A7)
-- による読みが異なる単語の一覧
-- 括弧を含む単語をうまく扱えないですがどうせ括弧入りの単語は除外するから考慮しません
-- 実は取得が雑で"概要"とかも入ってしまっていますが別に除外されて問題ないので放置しています
getDicNicoSpecialYomi :: IO (H.HashSet T.Text)
getDicNicoSpecialYomi = do
  let path = "cache/jp-nicovideo-dic-id-4652210"
  exist <- doesFileExist path
  if exist
    then B.readFile path >>= decodeIO
    else do
    let href = "https://dic.nicovideo.jp/id/4652210"
    response <- httpLBS $ fromString href
    when (getResponseStatus response /= status200) $
      error $ "ニコニコ大百科 " <> href <> " が取得できませんでした: " <> show response
    let doc = fromDocument $ parseLBS $ getResponseBody response
        dic = H.fromList $ normalizeWord . T.takeWhile (/= '（') . toTextStrict . innerText <$>
              queryT [jq|#article ul li|] doc
    B.writeFile path $ encode dic
    return dic

-- | Pixiv百科時点のサイトマップから記事一覧データを取得する
getDicPixiv :: IO (H.HashSet T.Text)
getDicPixiv = do
  let path = "cache/net-pixiv-dic"
  exist <- doesFileExist path
  if exist
    then B.readFile path >>= decodeIO
    else do
    sitemap <- fromDocument . parseLBS . getResponseBody <$> httpLBS "https://dic.pixiv.net/sitemap/"
    sitemaps <- mapM (\loc -> fromDocument . parseLBS . getResponseBody <$> httpLBS (parseRequest_ (toString (innerText loc)))) $
      queryT [jq|loc|] sitemap
    let dic = H.fromList $
          map (normalizeWord . toTextStrict . urlDecode False . toByteStringStrict) $
          mapMaybe (TL.stripPrefix "https://dic.pixiv.net/a/") $
          concatMap (map innerText . queryT [jq|loc|]) sitemaps
    B.writeFile path $ encode dic
    return dic

-- | 一致しているかで判定を行う箇所が多数存在するのでなるべく正規化する
normalizeWord :: T.Text -> T.Text
normalizeWord = replaceSymbol . normalize NFKC

-- | 中黒で三点リーダを表現しようとしているのを変換
replaceSymbol :: T.Text -> T.Text
replaceSymbol = T.replace "···" "…"

-- | 辞書に適している単語を抽出する
dictionaryWord :: H.HashSet Entry -> H.HashSet T.Text -> H.HashSet T.Text -> Entry -> Bool
dictionaryWord dicNico dicNicoSpecialYomi dicPixiv Entry{entryYomi, entryWord, entryRedirect} = and
  [ entryWord `H.member` dicPixiv                 -- Pixiv百科時点にも存在する単語のみを使う
  , not (entryWord `H.member` dicNicoSpecialYomi) -- 記事に載っている特殊な読みではない
    -- 読みが異様に短くない
  , 1 < T.length entryYomi
    -- 読みが異様に長くない
  , T.length entryYomi < 25
    -- 単語が読みに比べて異様に長くない
  , T.length entryWord < T.length entryYomi * 3
    -- 読みが単語に比べて異様に長くない
  , T.length entryYomi < T.length entryWord * 6
    -- 読みと単語が違うこと
    -- 読みと単語が一致しているエントリーはサジェストに役立つぐらいですが
    -- 一致しているのは短いものばかりなのでサジェストにすら役に立たないので辞書容量のため排除
  , entryYomi /= entryWord
    -- 括弧を含まない
  , T.all ('(' /=) entryWord
    -- マジで?など読みが3文字以下で単語が?で終わるやつは排除
  , not (T.length entryYomi <= 3 && T.last entryWord == '?')
    -- いま!など読みが3文字以下で単語が!で終わるやつは排除
  , not (T.length entryYomi <= 3 && T.last entryWord == '!')
    -- 曖昧さ回避などわかりやすい非単語記事ではない
  , not ("あいまいさ" `T.isInfixOf` entryYomi)
  , not ("一覧" `T.isSuffixOf` entryWord)
  , not ("画像集" `T.isSuffixOf` entryWord)
    -- 読みにけものフレンズなど曖昧さ回避を含むと辞書としては使い物にならないので除外
  , not ("けものふれんずの" `T.isPrefixOf` entryYomi)
  , not ("あずれんの" `T.isPrefixOf` entryYomi)
  , not ("せんごくばさら" `T.isSuffixOf` entryYomi)
    -- 単語の最後が兄貴か姉貴の場合読みも兄貴で終わることを保証
    -- 一般単語で一般単語の読みなのに単語本体は兄貴とついていて勝手に変換結果に｢兄貴｣がついてくるのを防止
  , not ("兄貴" `T.isSuffixOf` entryWord) || ("あにき" `T.isSuffixOf` entryWord)
  , not ("姉貴" `T.isSuffixOf` entryWord) || ("あねき" `T.isSuffixOf` entryWord)
    -- 記事名にその単語を含まないのにも関らず読みそれを表現しようとしている記事を除外
  , not (not ("実況" `T.isInfixOf` entryWord) && "じっきょう" `T.isInfixOf` entryYomi)
  , not (not ("映画" `T.isInfixOf` entryWord) && "えいが" `T.isInfixOf` entryYomi)
  , not (not ("アニメ" `T.isInfixOf` entryWord) && "あにめ" `T.isInfixOf` entryYomi)
  , not (not ("絵師" `T.isInfixOf` entryWord) && "えし" `T.isInfixOf` entryYomi)
    -- ｢1月1日｣のような単語はあっても辞書として意味がなく容量を食うだけなので除外
  , not ("月" `T.isInfixOf` entryWord && "日" `T.isSuffixOf` entryWord)
    -- 年号だけの記事を除外
  , not ("年" `T.isSuffixOf` entryWord)
    -- 誤変換指摘対策
    -- 同一読みのリダイレクトではない記事が他に存在するリダイレクト項目は除外します
  , not entryRedirect ||
    H.null (H.filter (\Entry{entryYomi = otherYomi, entryRedirect = otherRedirect} ->
                        not otherRedirect && otherYomi == entryYomi) dicNico)
  ]
