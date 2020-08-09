{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Main where

import           Control.Applicative
import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Monad
import           Control.Parallel.Strategies
import           Data.Attoparsec.Text
import qualified Data.ByteString             as B
import           Data.Char
import           Data.Either                 (isLeft)
import           Data.Hashable
import qualified Data.HashMap.Strict         as M
import qualified Data.HashSet                as S
import           Data.List
import qualified Data.Map.Strict             as OM
import           Data.Maybe                  (fromJust, fromMaybe, mapMaybe)
import           Data.Store
import           Data.String
import           Data.String.Transform
import qualified Data.Text                   as T
import           Data.Text.ICU.Char
import           Data.Text.ICU.Translit
import qualified Data.Text.IO                as T
import qualified Data.Text.Lazy              as TL
import           Data.Text.Metrics
import           Data.Text.Normalize
import           Data.Time.Format
import           Data.Time.LocalTime
import           GHC.Generics                (Generic)
import           Network.HTTP.Simple
import           Network.HTTP.Types
import           Prelude
import           System.Directory
import           Text.HTML.DOM
import           Text.XML                    hiding (parseLBS)
import           Text.XML.Cursor
import           Text.XML.Scraping
import           Text.XML.Selector.TH

-- | 記事エントリーを表すデータ構造
-- 名前が短すぎますが,これはライブラリじゃなくてアプリケーションなので汎用的な名前を付けてしまう
data Entry
  = Entry
  { entryYomi     :: !T.Text -- ^ 読み, ひらがなに限定
  , entryWord     :: !T.Text -- ^ 単語, オリジナルのものがそのまま入ります
  , entryRedirect :: !Bool   -- ^ リダイレクト記事か?
  } deriving (Eq, Ord, Read, Show, Generic)

instance Hashable Entry
instance Store Entry

-- | 辞書を得て標準出力にプリントアウトする
main :: IO ()
main = do
  -- キャッシュディレクトリがなければ作成する
  createDirectoryIfMissing False "cache"

  dicInfo <- getDicInfo
  dictionary <- getDictionary

  -- 参考情報をプリント
  T.putStrLn dicInfo

  -- 辞書本体をプリント
  T.putStr $ T.unlines $ toMozcLine <$> dictionary

-- | エントリー1つをMozcの辞書データ1行に変換する
toMozcLine :: Entry -> T.Text
toMozcLine Entry{entryYomi, entryWord} = entryYomi <> "\t" <> entryWord <> "\t" <> "固有名詞" <> "\t" <> "nico-pixiv"

-- | 辞書(エントリーの列)を得る
getDictionary :: IO [Entry]
getDictionary = do
  (dicNico, (dicNicoSpecialYomi, dicPixiv)) <- getDicNico `concurrently` (getDicNicoSpecialYomi `concurrently` getDicPixiv)
  -- ghciでのデバッグを楽にするために部分適用
  let dictionaryWordPred = dictionaryWord dicNicoSpecialYomi dicPixiv
      -- リスト化して1段階目のフィルタを通す
      dictionaryFiltered = filter dictionaryWordPred (S.toList dicNico) `using` parList rseq
      -- 読みがなキーマップ作成
      dicNicoYomiMap = mkDicNicoYomiMapNonRedirect dictionaryFiltered
      -- 誤変換指摘除外フィルタをかける
      dicFinalFiltered = filter (notMisconversion dicNicoYomiMap) dictionaryFiltered `using` parList rseq
      -- HashSetは順番バラバラなので最終的にソートする
      dictionarySorted = sortOn entryYomi $ sortOn entryWord dicFinalFiltered
  return dictionarySorted

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
getDicNico :: IO (S.HashSet Entry)
getDicNico = do
  let path = "cache/jp-nicovideo-dic"
  exist <- doesFileExist path
  if exist
    then B.readFile path >>= decodeIO
    else do
    doc <- fromDocument . parseLBS . getResponseBody <$> httpLBS "https://dic.nicovideo.jp/m/a/a"
    let chars = map TL.head $ filter (\text -> TL.length text == 1) $ map innerText $
          queryT [jq|.st-box_contents > table > tr > td > a|] doc
    dic <- mconcat <$> mapConcurrently getDicNicoTitle chars
    B.writeFile path $ encode dic
    return dic

-- | [「ア」から始まる50音順単語記事タイトル表示 - ニコニコ大百科](https://dic.nicovideo.jp/m/yp/a/%E3%82%A2)
-- のような記事からページャを辿って再帰的にデータを取得する
getDicNicoTitle :: Char -> IO (S.HashSet Entry)
getDicNicoTitle c = getDicNicoPage $ "https://dic.nicovideo.jp/m/yp/a/" <> toString (urlEncode False (toByteStringStrict [c]))

-- | ページャを辿っていくのでURLから取得したほうが都合が良いので別関数化して再帰する
getDicNicoPage :: String -> IO (S.HashSet Entry)
getDicNicoPage href = do
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
      ts = map (TL.strip . innerText) articles
      ws = map (TL.strip . innerText . queryT [jq|a|]) articles
      extras = zipWith (\word text -> fromJust $ TL.strip <$> TL.stripPrefix word text) ws ts
      dic = S.fromList $ zipWith
            (\word extra ->
               let yomi = TL.takeWhile (/= ')') $ fromJust $ TL.stripPrefix "(" extra
                   hiraganaYomi = katakanaToHiragana $ toTextStrict yomi
               in Entry
                  { entryWord = normalizeWord $ toTextStrict word
                  , entryYomi = normalizeWord hiraganaYomi
                  , entryRedirect = "(リダイレクト)" `TL.isInfixOf` extra
                  }) ws extras
      navis = node <$> queryT [jq|div.st-pg div.st-pg_contents a.navi|] doc
  when (S.null dic) $ error $ "ニコニコ大百科 " <> href <> " で単語が取得できませんでした: " <> show dic
  nextDic <-
        case find (\case
                      NodeElement Element{elementNodes} -> elementNodes == [NodeContent "次へ »"]
                      _ -> False
                  ) navis of
          Just (NodeElement (Element _ attrs _)) ->
            case OM.lookup "href" attrs of
              Nothing -> return Nothing
              Just newHref -> Just <$> getDicNicoPage ("https://dic.nicovideo.jp" <> toString newHref)
          _ -> return Nothing
  return $ dic <> fromMaybe S.empty nextDic

-- | カタカナをひらがなに変換
-- 長音記号を変換しない
-- icuで変換
-- 長音記号が変換されてしまうのでカタカナには使われない文字を使って誤魔化す
katakanaToHiragana :: T.Text -> T.Text
katakanaToHiragana = T.replace "!" "ー" . transliterate (trans "Katakana-Hiragana") . T.replace "ー" "!"

-- | ICUのblockCodeは純粋関数なのに例外を出すので自前実装
isKatakana :: Char -> Bool
isKatakana c =
  let o = ord c
  in 0x30A1 < o && o < 0x30FA

-- | ひらがなの捨て仮名を普通のかなにする
toUpHiragana :: T.Text -> T.Text
toUpHiragana word =
  let lower = ['ぁ', 'ぃ', 'ぅ', 'ぇ', 'ぉ', 'っ', 'ゃ', 'ゅ', 'ょ', 'ゎ', 'ゕ', 'ゖ']
      upper = ['あ', 'い', 'う', 'え', 'お', 'つ', 'や', 'ゆ', 'よ', 'わ', 'か', 'け']
      lowerUpper = M.fromList $ zip lower upper
  in T.map (\c -> fromMaybe c (M.lookup c lowerUpper)) word

-- | [読みが通常の読み方とは異なる記事の一覧とは (ニコチュウジチョウシロとは) [単語記事] - ニコニコ大百科](https://dic.nicovideo.jp/id/4652210)
-- による読みが異なる単語の一覧
-- 括弧を含む単語をうまく扱えないですがどうせ括弧入りの単語は除外するから考慮しません
-- 実は取得が雑で"概要"とかも入ってしまっていますが別に除外されて問題ないので放置しています
getDicNicoSpecialYomi :: IO (S.HashSet T.Text)
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
        dic = S.fromList $ normalizeWord . T.takeWhile (/= '（') . toTextStrict . innerText <$>
              queryT [jq|#article ul li|] doc
    B.writeFile path $ encode dic
    return dic

-- | Pixiv百科時点のサイトマップから記事一覧データを取得します
-- toFuzzyによって曖昧になっています
getDicPixiv :: IO (S.HashSet T.Text)
getDicPixiv = do
  let path = "cache/net-pixiv-dic"
  exist <- doesFileExist path
  if exist
    then B.readFile path >>= decodeIO
    else do
    sitemap <- fromDocument . parseLBS . getResponseBody <$> httpLBS "https://dic.pixiv.net/sitemap/"
    sitemaps <- mapM (\loc -> fromDocument . parseLBS . getResponseBody <$> httpLBS (parseRequest_ (toString (innerText loc)))) $
      queryT [jq|loc|] sitemap
    let dic = S.fromList $
          map (toFuzzy . normalizeWord . toTextStrict . urlDecode True . toByteStringStrict) $
          mapMaybe (TL.stripPrefix "https://dic.pixiv.net/a/") $
          concatMap (map innerText . queryT [jq|loc|]) sitemaps
    B.writeFile path $ encode dic
    return dic

-- | 一致しているかで判定を行う箇所が多数存在するのでなるべく正規化する
normalizeWord :: T.Text -> T.Text
normalizeWord = replaceEllipsis . normalize NFKC

-- | 中黒などで三点リーダを表現しようとしているのを変換
replaceEllipsis :: T.Text -> T.Text
replaceEllipsis word =
  let pseudoEllipsisList
        = [ "···"
          , "・・・"
          , "..."
          , "．．．"
          ]
  in foldr (`T.replace` "…") word pseudoEllipsisList

-- | 単語を曖昧比較します
-- toFuzzyに加え編集距離を考慮します
fuzzyEqual :: T.Text -> T.Text -> Bool
fuzzyEqual x y = levenshtein (toFuzzy x) (toFuzzy y) <= 2

-- | 単語を大雑把に量子化します
toFuzzy :: T.Text -> T.Text
toFuzzy w =
  let dropNotLetter = T.filter (\c -> isLetter c || isDigit c) w
      -- 単語から記号を消去するか?
      useDropNotLetter =
        -- 記号が大半を占める単語は記号を除かない
        not (T.length w /= 0 && (fromIntegral (T.length dropNotLetter) / fromIntegral (T.length w)) < (0.7 :: Double)) &&
        -- 消去した記号がプレフィクスだけの場合は無効
        not (dropNotLetter `T.isSuffixOf` w) &&
        -- 消去した記号がサフィックスだけの場合は無効
        not (dropNotLetter `T.isPrefixOf` w)
      useWord = if useDropNotLetter then dropNotLetter else w
  in T.toCaseFold $ katakanaToHiragana useWord

-- | 辞書に適している単語を抽出する(1段階目), リダイレクト関係は考慮しない
dictionaryWord :: S.HashSet T.Text -> S.HashSet T.Text -> Entry -> Bool
dictionaryWord dicNicoSpecialYomi dicPixiv Entry{entryYomi, entryWord} = and
    -- 読みが異様に短くない
  [ 1 < yomiLength
    -- 単語が読みに比べて異様に長くない
  , wordLength < yomiLength * 3
    -- 読みが単語に比べて異様に長くない
  , yomiLength < wordLength * 6
    -- 括弧を含まない
  , T.all ('(' /=) entryWord
    -- 単語が全てカタカナである場合
    -- ひらがなにして読みと一致する場合のみ許可
    -- 全てカナカナである場合ひらがなにしたもののみが遊んでいないと特定できるため
    -- 大文字小文字の揺れは許容
  , T.any (not . isKatakana) entryWord || toUpHiragana (katakanaToHiragana entryWord) == toUpHiragana entryYomi
    -- マジで? いま! など読みが4文字以下で単語が感嘆符で終わるやつは除外
  , not (yomiLength <= 4 && (T.last entryWord == '?' || T.last entryWord == '!'))
    -- ちょw など先頭のひらがな部分だけを読みに含む単語は誤爆危険性が高いため除外
    -- ! で終わる場合などは作品名のことが多いので除外しない
  , maybe True (\suf -> T.null suf || not (T.all (\c -> isAsciiUpper c || isAsciiLower c) suf)) $
    T.stripPrefix entryYomi entryWord
    -- 曖昧さ回避などわかりやすく単語記事ではないものを除外
  , not ("あいまいさ" `T.isInfixOf` entryYomi)
  , not ("一覧" `T.isSuffixOf` entryWord)
  , not ("画像集" `T.isSuffixOf` entryWord)
    -- 読みがなについて読みがなで言及しているものは特殊な読みであることが多いので除外
  , not ("よみかた" `T.isSuffixOf` entryYomi)
  , not ("よみがな" `T.isSuffixOf` entryYomi)
  -- 読みがなで分岐を示している単語を除外
  , not ("または" `T.isInfixOf` entryYomi)
  , not ("もしくは" `T.isInfixOf` entryYomi)
    -- 単語の最後が兄貴か姉貴の場合読みも兄貴で終わることを保証
    -- 一般単語で一般単語の読みなのに単語本体は兄貴とついていて勝手に変換結果に 兄貴 がついてくるのを防止
  , not ("兄貴" `T.isSuffixOf` entryWord) || ("あにき" `T.isSuffixOf` entryYomi)
  , not ("姉貴" `T.isSuffixOf` entryWord) || ("あねき" `T.isSuffixOf` entryYomi)
    -- 記事名にその単語を含まないのにも関らず読みでそれを表現しようとしている記事を除外
  , not (not ("実況" `T.isInfixOf` entryWord) && "じっきょう" `T.isInfixOf` entryYomi)
  , not (not ("映画" `T.isInfixOf` entryWord) && "えいが" `T.isInfixOf` entryYomi)
  , not (not ("アニメ" `T.isInfixOf` entryWord) && "あにめ" `T.isInfixOf` entryYomi)
  , not (not ("絵師" `T.isInfixOf` entryWord) && "えし" `T.isInfixOf` entryYomi)
    -- ※ で始まる記事は変換には使いづらい
  , not ("※" `T.isPrefixOf` entryWord)
    -- 制御文字による偽装タグを除去
  , T.all (/= '\803') entryWord
    -- 誕生祭, 生誕祭を除去
  , not ("誕生祭" `T.isInfixOf` entryWord)
  , not ("生誕祭" `T.isInfixOf` entryWord)
    -- オリジナル曲タグを除去
  , not ("オリジナル曲" `T.isSuffixOf` entryWord)
    -- 読みにけものフレンズなど曖昧さ回避を含むと辞書としては使い物にならないので除外
  , not ("あずれんの" `T.isPrefixOf` entryYomi)
  , not ("けものふれんずの" `T.isPrefixOf` entryYomi)
  , not ("じつざいのじんぶつとはあまりかんけいのない" `T.isPrefixOf` entryYomi)
  , not ("せんごくばさら" `T.isSuffixOf` entryYomi)
    -- 特定の遊戯王カードを除外. 何故ならば読みがなが なんばーず とかで始まるのは変換の役には立たないため
  , not ("かおすいまじなりーなんばーず" `T.isPrefixOf` entryYomi)
  , not ("かおすなんばーず" `T.isPrefixOf` entryYomi)
  , not ("しゃいにんぐなんばーず" `T.isPrefixOf` entryYomi)
  , not ("なんばーず" `T.isPrefixOf` entryYomi)
  , not ("ふゅーちゃーなんばーず" `T.isPrefixOf` entryYomi)
    -- 大事なことなので系統が多すぎるので除外
  , not ("だいじなことなので" `T.isPrefixOf` entryYomi)
    -- SCP記事は大抵メタタイトルが読みがなになっているのでIME辞書として使えない
    -- 覚えにくいナンバーをメタタイトルから出せる辞書として役に立つかもしれないですが作るなら包括的にscp wikiをスクレイピングする
  , isLeft $ parseOnly (string "SCP-") entryWord
    -- 第1回シンデレラガール選抜総選挙 のような単語は辞典では意味はあってもIME辞書では意味がないので除外
  , isLeft $ parseOnly (char '第' *> many1 digit *> char '回') entryWord
    -- 1月1日 のような単語はあっても辞書として意味がなく容量を食うだけなので除外
    -- 本当はパーサーコンビネータで真面目に処理したいのですが漢数字や毎月とかの処理が面倒な割に利益が無かったのでやめました
  , not ("月" `T.isInfixOf` entryWord && "日" `T.isSuffixOf` entryWord)
    -- 年を示す単語で始まるのは連番記事であることが多いし変換やサジェストの役にも立たないので除外
    -- ただ 3年B組金八先生 などがあるため2桁以上要求する
  , isLeft $ parseOnly (count 2 digit *> many' digit *> char '年') entryWord
    -- 元号も現代のものは除外
  , isLeft $ parseOnly
    ((string "明治" <|> string "大正" <|> string "昭和" <|> string "平成" <|> string "令和") *>
     many1 digit *> char '年')
    entryWord
    -- HOT7000系みたいなラテン数文字と数字だけのエントリーは直接打ったほうが速いので除外
  , isLeft $ parseOnly
    (skipMany1 (satisfy isAscii) *> (char '系' <|> char '形') *> endOfInput)
    entryWord
    -- 数字だけの記事を除外
  , not (T.all isNumber entryWord)
    -- 定義済みの特殊な読みではない
  , not (entryWord `S.member` dicNicoSpecialYomi)
    -- Pixiv百科時点にも存在する単語のみを使う
  , toFuzzy entryWord `S.member` dicPixiv
  ]
  where yomiLength = T.length entryYomi
        wordLength = T.length entryWord

-- | 誤変換指摘対策
-- リダイレクト記事であり
-- 元の記事の読みと同一であるか単語が曖昧的に一致するリダイレクト記事ではない記事が存在する場合
-- 誤変換の指摘であることが多いため除外します
-- ニコニコ大百科のページ一覧からはリダイレクトであることは読み取れますがリダイレクト先の記事が何かが分からないので
-- 単純に読みだけを見ると
-- 妖夢 → ヨウムが誤変換指摘と認識されてしまいます
-- しかし全てのリダイレクト記事を許可してしまうと表記揺れで単語数が膨らんでしまうので
-- ファジーマッチによってリダイレクト先を妥協予測します
notMisconversion :: M.HashMap T.Text (S.HashSet T.Text) -> Entry -> Bool
notMisconversion dicNicoYomiMap Entry{entryYomi, entryWord, entryRedirect}
  -- リダイレクトではなければ無条件で問題ない
  = not entryRedirect
  -- ひらがなのみの場合漢字を溶かしたものである可能性が高いので除外
  || (not (T.all ((== Hiragana) . blockCode) entryWord)
     -- ファジーマッチでリダイレクト先っぽい記事を探索してあったらリダイレクトがあるとする
      && Just False /= (S.null . S.filter (entryWord `fuzzyEqual`) <$> M.lookup entryYomi dicNicoYomiMap))

-- | 読みがなをキーとした非リダイレクトの単語のマップを作ります
mkDicNicoYomiMapNonRedirect :: [Entry] -> M.HashMap T.Text (S.HashSet T.Text)
mkDicNicoYomiMapNonRedirect dictionaryFiltered
  = M.fromListWith (<>)
  [(entryYomi, S.singleton entryWord) | Entry{entryYomi, entryWord, entryRedirect} <- dictionaryFiltered, not entryRedirect]
