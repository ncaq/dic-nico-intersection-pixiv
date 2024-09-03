{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import           Control.Applicative
import           Control.Concurrent.Async
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Parallel.Strategies
import           Control.Retry
import           Data.Attoparsec.Text        as P
import qualified Data.ByteString             as B
import           Data.Char
import           Data.Convertible
import           Data.Either                 (isLeft)
import           Data.Hashable
import qualified Data.HashMap.Strict         as M
import qualified Data.HashSet                as S
import qualified Data.List                   as L
import           Data.Maybe
import           Data.Store
import           Data.String
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import           Data.Text.ICU.Translit
import qualified Data.Text.IO                as T
import           Data.Text.Metrics
import           Data.Text.Normalize
import           Data.Time.Format
import           Data.Time.LocalTime
import           GHC.Generics                (Generic)
import           Network.HTTP.Simple
import           Network.HTTP.Types
import           System.Directory
import           Text.HTML.Scalpel

-- | 記事エントリーを表すデータ構造。
-- 名前が短すぎますが、これはライブラリじゃなくてアプリケーションなので、一般名詞的な名前を付けても構わないと判断しました。
data Entry
  = Entry
  { entryYomi     :: !Text -- ^ 読み、ひらがなに限定。
  , entryWord     :: !Text -- ^ 単語、オリジナルのものがそのまま入ります。
  , entryRedirect :: !Bool -- ^ リダイレクト記事か?
  }
  deriving (Eq, Ord, Read, Show, Generic)
instance Hashable Entry
instance Store Entry

-- | 辞書を得て、標準出力にプリントアウトします。
main :: IO ()
main = do
  -- キャッシュディレクトリがなければ作成します
  createDirectoryIfMissing False "cache"

  dicInfo <- getDicInfo
  dictionary <- getDictionary

  -- Mozc/Google日本語入力形式の辞書データを保存します。
  T.writeFile "public/dic-nico-intersection-pixiv-google.txt" $ T.unlines $ dicInfo : (toMozcLine <$> dictionary)

-- | エントリー1つをMozcの辞書データ1行に変換します。
toMozcLine :: Entry -> Text
toMozcLine Entry{entryYomi, entryWord} = T.intercalate "\t" [entryYomi, entryWord, kind, "nico-pixiv"]
  where kind | T.all isAscii entryWord = "アルファベット"
             | otherwise = "固有名詞"

-- | 辞書(エントリーの列)を得ます。
getDictionary :: IO [Entry]
getDictionary = do
  (dicNico, (dicNicoSpecialYomi, dicPixiv)) <- getDicNico `concurrently` (getDicNicoSpecialYomi `concurrently` getDicPixiv)
  -- ghciでのデバッグを楽にするために部分適用しておきます
  let dictionaryWordFn = dictionaryWord dicNicoSpecialYomi dicPixiv
      -- リスト化して1段階目のフィルタを通します
      dictionaryFiltered = filter dictionaryWordFn (S.toList dicNico)
      -- 辞書単語の集合です
      dicWordSet = S.fromList $ map entryWord dictionaryFiltered
      -- シリーズ除外フィルタをかけます
      dicNotSeries = filter (notSeries dicWordSet) dictionaryFiltered
      -- 読みがなキーマップを作成します
      dicNicoYomiMap = mkDicNicoYomiMap dicNotSeries
      -- 非リダイレクト読みがなキーマップを作成します
      dicNicoYomiMapNotRedirect = mkDicNicoYomiMapNotRedirect dicNotSeries
      -- リンク用除外フィルタ関数です
      notLinkFriendlyFn = notLinkFriendly dicNicoYomiMap
      -- 誤変換指摘除外フィルタ関数です
      notMisconversionFn = notMisconversion dicNicoYomiMapNotRedirect
      -- 誤変換とリンク用フィルタをかけます
      dicNotMisAndLink = filter (\e -> notMisconversionFn e && notLinkFriendlyFn e) dicNotSeries
      -- 順番がバラバラになるので、読み/単語の優先度で最終的にソートします。
      -- また最後にリストの評価を並列で行うことで速度向上を狙います。
      dictionarySorted = L.sortOn entryYomi (L.sortOn entryWord dicNotMisAndLink) `using` parList rseq
  return dictionarySorted

-- | 生成日を含めたこのデータの情報を表示します。
getDicInfo :: IO Text
getDicInfo = do
  time <- formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S%z" <$> getZonedTime
  return $ T.unlines
    [ "# name: dic-nico-intersection-pixiv"
    , "# description: ニコニコ大百科とピクシブ百科事典の共通部分の辞書"
    , "# github: https://github.com/ncaq/dic-nico-intersection-pixiv"
    , "# createdAt: " <> convert time
    , "# copying:"
    , "# nicovideo: https://dic.nicovideo.jp/"
    , "# pixiv: https://dic.pixiv.net/"
    ]

-- | [50音順単語記事一覧 - ニコニコ大百科](https://dic.nicovideo.jp/m/a/a)
-- から単語と読み一覧を取得します。
getDicNico :: IO (S.HashSet Entry)
getDicNico = do
  let path = "cache/jp-nicovideo-dic"
  exist <- doesFileExist path
  if exist
    then B.readFile path >>= decodeIO
    else do
    Just doc <-
      scrapeURL
      "https://dic.nicovideo.jp/m/a/a"
      (texts $ "div" @: [hasClass "st-box_contents"] // "table" // "tr" // "td" // "a")
    let chars = map T.head $ filter (\t -> T.length t == 1) doc
    dic <- mconcat <$> mapConcurrently getDicNicoTitle chars
    B.writeFile path $ encode dic
    return dic

-- | [「ア」から始まる50音順単語記事タイトル表示 - ニコニコ大百科](https://dic.nicovideo.jp/m/yp/a/%E3%82%A2)
-- のような記事からページャを辿って再帰的にデータを取得します。
getDicNicoTitle :: Char -> IO (S.HashSet Entry)
getDicNicoTitle c = getDicNicoPage $ "https://dic.nicovideo.jp/m/yp/a/" <> T.singleton c

-- | ページャを順番に辿っていく方が実装が楽で、サーバとの通信速度を考えても、過度に並列化しても意味がないので別関数で取得します。
getDicNicoPage :: Text -> IO (S.HashSet Entry)
getDicNicoPage href = recoverAll backoffLimitRetryPolicy $ \_ -> do
  response <- do
    res <- httpBS $ fromString $ convert href
    if getResponseStatus res == status200
      then return res
      else do
      -- ニコニコ大百科のサーバはしばしば一時的に壊れてランダムに通信に失敗するので、
      -- 失敗した場合リトライするために例外を投げます。
      error $ "ニコニコ大百科 " <> convert href <> " が取得できませんでした: " <> show res
  let doc = convert $ getResponseBody response
      articles = join $ maybeToList $ scrapeStringLike doc (htmls $ "div" @: [hasClass "article"] // "ul" // "ul" // "li")
      ts = (\article -> T.strip <$> scrapeStringLike article (text anySelector)) `mapMaybe` articles
      ws = (\article -> T.strip <$> scrapeStringLike article (text "a")) `mapMaybe` articles
      extras = zipWith (\w t -> T.strip (fromJust (T.stripPrefix w t))) ws ts
      dic = S.fromList $ zipWith
            (\w extra ->
               let yomi = T.takeWhile (/= ')') $ fromJust $ T.stripPrefix "(" extra
                   hiraganaYomi = katakanaToHiragana yomi
               in Entry
                  { entryWord = normalizeWord w
                  , entryYomi = normalizeWord hiraganaYomi
                  , entryRedirect = "(リダイレクト)" `T.isInfixOf` extra
                  }) ws extras
      naviHrefs = join $ maybeToList $ scrapeStringLike doc (htmls ("a" @: [hasClass "navi"]))
      mNextHref = listToMaybe $ mapMaybe (\x -> scrapeStringLike x (attr "href" "a")) $
        filter (\x -> scrapeStringLike x (text anySelector) == Just "次へ »") naviHrefs
  when (S.null dic) $ error $ "ニコニコ大百科 " <> convert href <> " で単語が取得できませんでした: " <> show dic
  nextDic <- case mNextHref of
    Just nextHref -> Just <$> getDicNicoPage ("https://dic.nicovideo.jp" <> nextHref)
    _             -> return Nothing
  return $ dic <> fromMaybe S.empty nextDic

-- | ネットワーク通信に使うリトライ制限。
-- 1/10秒からタイムアウトを乱数で増やして、10回までリトライします。
backoffLimitRetryPolicy :: MonadIO m => RetryPolicyM m
backoffLimitRetryPolicy = fullJitterBackoff (1 * 1000) <> limitRetries 10

-- | [読みが通常の読み方とは異なる記事の一覧とは (ニコチュウジチョウシロとは) [単語記事] - ニコニコ大百科](https://dic.nicovideo.jp/id/4652210)
-- による読みが異なる単語の一覧を取得します。
-- 括弧を含む単語をうまく扱えないですがどうせ括弧入りの単語は除外するから考慮しません。
-- 実は取得が雑で"概要"とかも入ってしまっていますが、最終的に除外されるので実害がないので放置しています。
getDicNicoSpecialYomi :: IO (S.HashSet Text)
getDicNicoSpecialYomi = do
  let path = "cache/jp-nicovideo-dic-id-4652210"
  exist <- doesFileExist path
  if exist
    then B.readFile path >>= decodeIO
    else do
    Just liTexts <- scrapeURL "https://dic.nicovideo.jp/id/4652210" (texts $ "div" @: [hasClass "article"] // "ul" // "li")
    let dic = S.fromList $ normalizeWord . T.takeWhile (/= '（') <$> liTexts
    B.writeFile path $ encode dic
    return dic

-- | Pixiv百科時点のサイトマップから記事一覧データを取得します。
-- toFuzzyによって量子化が行われています。
getDicPixiv :: IO (S.HashSet Text)
getDicPixiv = do
  let path = "cache/net-pixiv-dic"
  exist <- doesFileExist path
  if exist
    then B.readFile path >>= decodeIO
    else do
    Just sitemaps <- scrapeURL "https://dic.pixiv.net/sitemap.xml" (texts "loc")
    pageURLs <- join . catMaybes <$> (\sitemap -> scrapeURL sitemap (texts "loc")) `mapM` sitemaps
    let dic = S.fromList $
          map (toFuzzy . normalizeWord . convert . urlDecode True . convert) $
          mapMaybe (T.stripPrefix "https://dic.pixiv.net/a/") pageURLs
    B.writeFile path $ encode dic
    return dic

-- | 一致しているかで判定を行う箇所が多数存在するのでなるべく正規化します。
normalizeWord :: Text -> Text
normalizeWord = replaceEllipsis . normalize NFKC

-- | 中黒などで三点リーダを表現しようとしているのを正規の三点リーダに変換します。
replaceEllipsis :: Text -> Text
replaceEllipsis w =
  let pseudoEllipsisList
        = [ "···"
          , "・・・"
          , "..."
          , "．．．"
          ]
  in foldr (`T.replace` "…") w pseudoEllipsisList

-- | 単語を曖昧比較します。
-- toFuzzyに加え編集距離を考慮します。
fuzzyEqual :: Text -> Text -> Bool
fuzzyEqual x y = levenshtein (toFuzzy x) (toFuzzy y) <= 2

-- | 単語を大雑把に量子化します。
toFuzzy :: Text -> Text
toFuzzy w =
  let dropNotLetter = T.filter (\c -> isLetter c || isDigit c) w
      -- 単語から記号を消去するか?
      useDropNotLetter =
        -- 記号が大半を占める単語は記号を除かない
        not (T.length w /= 0 && fromIntegral (T.length dropNotLetter) / fromIntegral (T.length w) < (0.7 :: Double)) &&
        -- 消去した記号がプレフィクスだけの場合は無効
        not (dropNotLetter `T.isSuffixOf` w) &&
        -- 消去した記号がサフィックスだけの場合は無効
        not (dropNotLetter `T.isPrefixOf` w)
      useWord = if useDropNotLetter then dropNotLetter else w
  in T.toCaseFold $ katakanaToHiragana useWord

-- | カタカナをひらがなに変換します。
-- 長音記号を文字に変換しません。
-- icuで変換します。
-- 普通に呼び出すと長音記号が変換されてしまうので、カタカナには使われない文字を使って誤魔化しています。
katakanaToHiragana :: Text -> Text
katakanaToHiragana = T.replace "!" "ー" . transliterate (trans "Katakana-Hiragana") . T.replace "ー" "!"

-- | `Char`がひらがなであることを判定します。
-- Unicodeの平仮名ブロックとは一致せず、読みを持つものだけに限定しています。
isReadableHiragana :: Char -> Bool
isReadableHiragana c =
  let o = ord c
  in 0x3041 <= o && o <= 0x3096

-- | `Char`がカタカナであることを判定します。
-- ICUのblockCodeは純粋関数なのに例外を出すので自前実装しています。
-- Unicodeの片仮名ブロックとは一致せず、読みを持つものだけに限定しています。
isReadebleKatakana :: Char -> Bool
isReadebleKatakana c =
  let o = ord c
  in 0x30A1 <= o && o <= 0x30FA

-- | `isReadableHiragana` or `isReadebleKatakana`
isReadebleHiraganaOrKatakana :: Char -> Bool
isReadebleHiraganaOrKatakana c = isReadableHiragana c || isReadebleKatakana c

-- | ひらがなの捨て仮名を普通のかなにします。
toUpHiragana :: Text -> Text
toUpHiragana w =
  let lower = "ぁぃぅぇぉっゃゅょゎゕゖ"
      upper = "あいうえおつやゆよわかけ"
      lowerUpper = M.fromList $ zip lower upper
  in T.map (\c -> fromMaybe c (M.lookup c lowerUpper)) w

-- | 読みが明瞭に分かるひらがなであることを判定します。
isClearHiragana :: Char -> Bool
isClearHiragana c
  | c `elem`
    (("ぁぃぅぇぉっゃゅょゎゕゖ" :: String) -- 捨て仮名の読みは分からない
     <> "ほはへ" -- 現代仮名遣いの欠点で変則的な読みをすることがある ほ→おなど
     <> "ゔ"-- 外来語に多いので無理
     <> "ゑを" -- 現代仮名遣いへの以降が中途半端になったので曖昧性が高い
     <> "ゝゞゟ" -- 踊り字は頑張ればコンテキスト由来で解析出来るかもしれませんが、難しいし、許容する分にはさほど困らないので放置
    ) = False
  | otherwise = isReadableHiragana c

-- | 読みが明瞭に分かるカタカナであることを判定します。
isClearKatakana :: Char -> Bool
isClearKatakana 'ケ' = False -- 阿佐ケ谷駅とかが判別不可能なので無理
isClearKatakana c   = isReadebleKatakana c

-- | 読みが明瞭にわかる平仮名に変換することを判定します。
toClearHiragana :: Char -> Char
toClearHiragana 'ヶ' = 'が'
toClearHiragana c
  -- katakanaToHiraganaのためにTextに変換して戻すのすごい無駄ですが、他に良い方法が思いつきませんでした
  | isClearKatakana c = T.head $ katakanaToHiragana $ T.singleton c
  | otherwise = c

-- | 単語をある程度正確に推定出来る範囲で読み(ひらがな)に変換します。
-- 今の所カタカナ → ひらがなのみの返還です。
toYomiEffortGroup :: Text -> [Text]
toYomiEffortGroup w = if " ゙" `T.isInfixOf` w -- アネ゙デパミ゙みたいなのを解析するのは不可能でした
  then []
  else filter (/= "") $ T.split (not . isClearHiragana) $ T.map toClearHiragana w

-- | 辞書に適している単語を抽出する(1段階目)、リダイレクト関係は考慮しません。
dictionaryWord :: S.HashSet Text -> S.HashSet Text -> Entry -> Bool
dictionaryWord dicNicoSpecialYomi dicPixiv Entry{entryYomi, entryWord} = and
  -- Pixiv百科時点にも存在する単語のみを使う
  [ toFuzzy entryWord `S.member` dicPixiv
    -- 定義済みの特殊な読みではない
  , not (entryWord `S.member` dicNicoSpecialYomi)
    -- 読みが異様に短くない
  , 1 < yomiLength
    -- 単語が読みに比べて異様に長くない
  , wordLength < yomiLength * 3
    -- 読みが単語に比べて異様に長くない
    -- 記号などを含めたいので単語の長さで基準を大幅に変える
  , yomiLength < wordLength *
    if | wordLength <= 2  -> 6
       | wordLength <= 4  -> 5
       | wordLength <= 10 -> 3
       | otherwise        -> 2
    -- 括弧を含まない
  , T.all ('(' /=) entryWord
  , T.all ('〔' /=) entryWord
    -- 制御文字による偽装タグを除去
  , T.all (/= '\803') entryWord
    -- 数字だけの記事を除外
  , not (T.all isNumber entryWord)
    -- 全てAscii文字で読みが2文字以下もしくは単語が3文字以下なら直接入力したほうが速いのと誤爆危険性が高いので除外
    -- Ascii可読文字だけではなくスペースや記号も除外しているのはAsciiの範囲ならキーボードから直接入力出来るため
  , not (T.all isAscii entryWord && (yomiLength <= 2 || wordLength <= 3))
    -- ひらがなカタカナが記事に入っている場合読みがなにも同じものが入っていることを保証する
    -- 要するに記事名をコンピュータが判断できる範囲でひらがな化して比較する
    -- 本当はパーサーコンビネータなどで順序を保証するべきなのですが実装が面倒なので手を抜いています
  , all (`T.isInfixOf` toUpHiragana entryYomi) $ toYomiEffortGroup entryWord
    -- 単語が全てひらがなかカタカナ(特定の記号で区切ってある場合も含む)である場合
    -- 記号を除いて全文一致することを求めます
  , not (T.all isReadebleHiraganaOrKatakana entryWord &&
         T.all (\c -> c `notElem` ("ゑをヱヲ" :: String) && isReadebleHiraganaOrKatakana c ||
                 c `elem` ("・= ー" :: String)) entryWord) ||
    (T.filter isClearHiragana . katakanaToHiragana) entryWord == T.filter isClearHiragana entryYomi
    -- マジで? いま! など読みが4文字以下で単語が感嘆符で終わるやつは除外
    -- 変換で誤爆危険性が高いのと感嘆符をつけ足すだけなので変換する意味がない
    -- サジェストの役に立つかもしれないので5文字異常は許可します
  , not (yomiLength <= 4 && (T.last entryWord == '?' || T.last entryWord == '!'))
    -- ちょw など先頭のひらがな部分だけを読みに含む単語は誤爆危険性が高いため除外
    -- 感嘆符で終わる場合などは作品名のことが多いので除外しません
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
  -- 本当に記事名に入っているものは除外しない
  , not ("あるいは" `T.isInfixOf` entryYomi && not ("あるいは" `T.isInfixOf` entryWord))
  , not ("または" `T.isInfixOf` entryYomi && not ("または" `T.isInfixOf` entryWord))
  , not ("もしくは" `T.isInfixOf` entryYomi && not ("もしくは" `T.isInfixOf` entryWord))
    -- 単語の最後が兄貴か姉貴の場合読みも兄貴で終わることを保証
    -- 一般単語で一般単語の読みなのに単語本体は兄貴とついていて勝手に変換結果に 兄貴 がついてくるのを防止
  , not ("兄貴" `T.isSuffixOf` entryWord) || ("あにき" `T.isSuffixOf` entryYomi)
  , not ("姉貴" `T.isSuffixOf` entryWord) || ("あねき" `T.isSuffixOf` entryYomi)
    -- 事件で終わる場合読みもじけんで終わることを保証
  , not ("事件" `T.isSuffixOf` entryWord) || ("じけん" `T.isSuffixOf` entryYomi)
    -- 記事名にその単語を含まないのにも関らず読みでそれを表現しようとしている記事を除外
  , not (not ("実況" `T.isInfixOf` entryWord) && "じっきょう" `T.isInfixOf` entryYomi)
  , not (not ("映画" `T.isInfixOf` entryWord) && "えいが" `T.isInfixOf` entryYomi)
  , not (not ("アニメ" `T.isInfixOf` entryWord) && "あにめ" `T.isInfixOf` entryYomi)
  , not (not ("絵師" `T.isInfixOf` entryWord) && "えし" `T.isInfixOf` entryYomi)
    -- 読みにかわいいと書いているのに単語にかわいいと書かれていないのを除外
  , not ("かわいい" `T.isInfixOf` entryYomi
         && not (any (`T.isInfixOf` entryWord) ["かわいい", "カワイイ", "可愛い", "KAWAII", "Kawaii", "kawaii"]))
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
    -- ※ で始まる記事は変換には使いづらい
  , '※' /= T.head entryWord
    -- SCP記事は大抵メタタイトルが読みがなになっているのでIME辞書として使えない
    -- 覚えにくいナンバーをメタタイトルから出せる辞書として役に立つかもしれないですが作るなら包括的にscp wikiをスクレイピングする
  , not ("SCP-" `T.isPrefixOf` entryWord)
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
    -- HOT7000系みたいな英数字と数字だけのエントリーは直接打ったほうが速いので除外
  , isLeft $ parseOnly
    (skipMany1 (satisfy isAscii) *> (char '系' <|> char '形') *> endOfInput)
    entryWord
  ]
  where yomiLength = T.length entryYomi
        wordLength = T.length entryWord

-- | 読みがなをキーとした単語のマップを作ります。
mkDicNicoYomiMap :: [Entry] -> M.HashMap Text (S.HashSet Text)
mkDicNicoYomiMap dictionaryFiltered =
  M.fromListWith (<>)
  [(entryYomi, S.singleton entryWord) | Entry{entryYomi, entryWord} <- dictionaryFiltered]

-- | 読みがなをキーとした非リダイレクトの単語のマップを作ります。
mkDicNicoYomiMapNotRedirect :: [Entry] -> M.HashMap Text (S.HashSet Text)
mkDicNicoYomiMapNotRedirect dictionaryFiltered =
  M.fromListWith (<>)
  [(entryYomi, S.singleton entryWord) | Entry{entryYomi, entryWord, entryRedirect} <- dictionaryFiltered, not entryRedirect]

-- | `ドラゴンクエストビルダーズ2` のような、シリーズ元の単語がある単純な続編タイトルを抽出して除外するための関数です。
-- `Splatoon 2` のような間にスペースが入っている単語を除外していないのは諦めています。
-- `Windows 10` がWindowsのシリーズ扱いで除外されてしまう割に、あまり除外できる単語がないためです。
-- そもそも除外しなくても良いぐらいのノイズ数です。
notSeries :: S.HashSet Text -> Entry -> Bool
notSeries dicWord Entry{entryWord} =
  -- 長い数字は入力するの面倒なので除外しないようにする
  case parseOnly ((,) <$> P.takeWhile (not . isDigit) <*> (rational :: Parser Double) <* endOfInput) entryWord of
    Left _          -> True
    Right (base, r) -> not $ base `S.member` dicWord &&
      -- 小数点数なので雑な長さ比較になっている
      -- 整数なら3文字なら除外されないはず
      length (show r) <= 4

-- | `identityv` のような正式名称を小文字に潰して、スペースを消して、リンクを繋ぎやすくした単語を除外するための関数です。
notLinkFriendly :: M.HashMap Text (S.HashSet Text) -> Entry -> Bool
notLinkFriendly dicNicoYomiMap Entry{entryYomi, entryWord, entryRedirect} =
  -- 同じ読みの記事セット、自分自身は除く
  let equalYomiEntrySet = S.filter (/= entryWord) $ fromMaybe S.empty $ M.lookup entryYomi dicNicoYomiMap
  -- 同じ読みの記事単語空白を除いたもののセット
      equalYomiEntryWordThinSet = S.map (T.filter (not . isSpace)) equalYomiEntrySet
  -- 同じ読みの記事単語空白を除いて小文字にしたもののセット
      equalYomiEntryWordThinAndLeterSet = S.map (T.filter (not . isSpace) . T.toLower) equalYomiEntrySet
  -- リンクのための記事はリダイレクトになっているのでリダイレクトで無ければリンクのための記事と判断しなくて良い
  in not entryRedirect ||
  -- 空白を除いて小文字化したら同じ記事名に同じ読みになる単語が存在すればリンクのための記事だとわかる
     notElem entryWord equalYomiEntryWordThinSet && notElem entryWord equalYomiEntryWordThinAndLeterSet

-- | 誤変換指摘記事対策です。
-- リダイレクト記事であり、
-- 元の記事の読みと同一であるか単語が曖昧的に一致するリダイレクト記事ではない記事が存在する場合、
-- 誤変換の指摘であることが多いため除外します。
-- ニコニコ大百科のページ一覧からはリダイレクトであることは読み取れますが、
-- リダイレクト先の記事が何かが分からないので、
-- 単純に読みだけを見ると、
-- 妖夢 → ヨウムが誤変換指摘と認識されてしまいます。
-- しかし全てのリダイレクト記事を許可してしまうと表記揺れで単語数が膨らんでしまうので、
-- ファジーマッチによってリダイレクト先を妥協予測します。
notMisconversion :: M.HashMap Text (S.HashSet Text) -> Entry -> Bool
notMisconversion dicNicoYomiMap Entry{entryYomi, entryWord, entryRedirect} =
  -- リダイレクトではなければ無条件で問題ない
  not entryRedirect
  -- ひらがなのみの場合漢字を溶かしたものである可能性が高いので除外
  || not (T.all isReadableHiragana entryWord)
     -- ファジーマッチでリダイレクト先っぽい記事を探索してあったらリダイレクトがあるとする
      && Just False /= (S.null . S.filter (entryWord `fuzzyEqual`) <$> M.lookup entryYomi dicNicoYomiMap)
