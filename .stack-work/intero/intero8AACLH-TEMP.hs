{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
{-# LANGUAGE DisambiguateRecordFields #-}

module WaiLib where

{-| 
    The Module contains all the functios for __wai.hs__

    * Use Aeson to serialize record to Json
    * Record: Person
    * Insert data to MySqlit-simple file-based database
    * Upload file to server.
    * Use Redis(memcached) to store snippet and query snippet.
-} 
import Data.Typeable (typeOf)
import Network.Wai
import Network.HTTP.Types
import Network.Wai.Handler.Warp (run)
import Control.Monad
import Data.Char
import Data.Maybe
import Data.List
import Data.Time
import Data.IORef 
import Data.Time.Clock.POSIX
import System.Directory
import System.Environment
import System.Exit
import System.FilePath.Posix
import System.IO
import System.Posix.Files
import System.Posix.Unistd
import System.Process
import Text.Read
import Text.Regex
import Text.Regex.Base
import Text.Regex.Base.RegexLike
import Text.Regex.Posix
import Text.RE.TDFA.String
import Network.Wai.Parse
import Blaze.ByteString.Builder.Char.Utf8 (fromString)
import Data.ByteString.Builder (byteString, Builder)




import qualified Text.Email.Validate as EM 
import qualified Data.Word8 as DW
import Data.Text (Text)  -- strict Text
import qualified Data.Text as TS               -- strict Text         
import qualified Data.Text.Lazy                 as DL 
import qualified Data.Text.IO                   as TIO 

import qualified Control.Concurrent             as Concurrent
import qualified Data.List as L
import qualified Data.HashMap.Strict as M 
import qualified Control.Exception              as Exception
import qualified Safe

import qualified Data.ByteString.UTF8 as BU
import qualified Data.ByteString.Lazy.Internal as IN (ByteString)
import qualified Data.ByteString.Char8      as S8 (unpack,pack, putStrLn)   -- strict ?
import qualified Data.ByteString.Lazy       as LA (writeFile, fromChunks, fromStrict)
import qualified Data.ByteString.Lazy.Char8 as LC 
import qualified Data.ByteString            as BS
import qualified Data.ByteString.Internal   as BI (c2w, w2c)

import PortableLines
import AronModule                hiding(run, cmd)
import AronHtml 
import qualified AronModule                 as A
import qualified GenePDFHtmlLib             as PDF
import qualified WaiConstant                as WC 

import qualified Turtle as TUR -- (empty, shellStrictWithErr, ExitCode)
-- import Data.Text.Lazy -- lazy Text

import Network.HTTP.Types (status200)
import Network.Wai
import Network.Wai.Handler.Warp (run)
import Network.Wai.Util
import Network.URI
import Network.HTTP.Types.Status

import qualified Network.Wai.Handler.WebSockets as WS
import qualified Network.WebSockets             as WS

-- {-# LANGUAGE QuasiQuotes       #-}
import Text.RawString.QQ         -- Need QuasiQuotes too 

-- remove it since there is issue to build in stack
-- copy the source code and create a module called PortableLines
-- import qualified Text.PortableLines as POR   -- (lines replace window newline '\r\n' with '\n')

import           Data.Int (Int64)
import           Database.SQLite.Simple
import           Database.SQLite.Simple.FromRow
import           Database.SQLite.Simple.FromField
import           Database.SQLite.Simple.ToField
import           Database.SQLite.Simple.Internal
import           Database.SQLite.Simple.Ok

import GHC.Generics
import qualified Data.Aeson as DA

toBS = strToStrictByteString
pdfdir = "pdf"

data Block = Block{bblock::[DL.Text]} deriving (Generic, Show)
data MBlock = MBlock{mblock::[Integer]} deriving (Generic, Show)
data GeneMatrix = GeneMatrix{
                             cmd :: TS.Text,
                             ncol :: Integer,
                             nrow :: Integer 
                            } deriving (Generic, Show)

instance DA.FromJSON GeneMatrix 
instance DA.ToJSON GeneMatrix where
    toEncoding = DA.genericToEncoding DA.defaultOptions

data ReplyCode = ReplyCode{ 
                            rcmd :: TS.Text,
                            rerror :: TS.Text,
                            stdout :: TS.Text 
                          } deriving (Generic, Show)

data User = User   {uid::Int64, name::TS.Text, email::TS.Text, password::TS.Text, task::TS.Text, money::Integer} deriving (Show, Eq, Read)
data Image = Image {iid::Int64, imagename::TS.Text, uid::Int64} deriving (Show, Eq, Read)

instance FromRow User where
  fromRow = User <$> field <*> field <*> field <*> field <*> field <*> field

instance FromRow Image where
  fromRow = Image <$> field <*> field <*> field

instance ToRow User where
  toRow (User _uid name email password task money) = toRow (name, email, password, task, money)

instance ToRow Image where
  toRow (Image _iid imagename uid) = toRow (imagename, uid)


instance DA.FromJSON ReplyCode 
instance DA.ToJSON ReplyCode where
    toEncoding = DA.genericToEncoding DA.defaultOptions

data CompileCode = CompileCode{
                                compiler :: TS.Text,
                                option :: TS.Text,
                                code :: TS.Text 
                              } deriving (Generic, Show)

instance DA.FromJSON CompileCode 
instance DA.ToJSON CompileCode where
    toEncoding = DA.genericToEncoding DA.defaultOptions


-- Record MatInt, serialize, deserialize
data MatInt = MatInt{name::TS.Text, matrix::[[Integer]]} deriving (Generic, Show)
instance DA.FromJSON MatInt 
instance DA.ToJSON MatInt where
    toEncoding = DA.genericToEncoding DA.defaultOptions

instance DA.FromJSON Block 
instance DA.ToJSON Block where
    -- No need to provide a toJSON implementation.

    -- For efficiency, we write a simple toEncoding implementation, as
    -- the default version uses toJSON.
    toEncoding = DA.genericToEncoding DA.defaultOptions

instance DA.FromJSON MBlock 
instance DA.ToJSON MBlock where
    -- No need to provide a toJSON implementation.

    -- For efficiency, we write a simple toEncoding implementation, as
    -- the default version uses toJSON.
    toEncoding = DA.genericToEncoding DA.defaultOptions

-- | Person to Json object
data Person =
  Person 
    { personId   :: Int64
    , personName :: TS.Text
    , personAge  :: TS.Text
    } deriving (Eq,Read,Show)


{-| 
    === create UserInput table in Sqlite
    * cmdId = pid
    * xcmd = input command, e.g. "c ls"
-} 
data UserInput =
  UserInput 
    { cmdId :: Int64
    , xcmd :: TS.Text
    } deriving (Eq,Read,Show)

instance FromRow Person where
  fromRow = Person <$> field <*> field <*> field

instance FromRow UserInput where
  fromRow = UserInput <$> field <*> field

-- when inserting a new Person, ignore personId. SQLite will provide it for us.
instance ToRow Person where
  toRow (Person _pId pName pAge) = toRow (pAge, pName)

-- http://hackage.haskell.org/package/sqlite-simple-0.4.16.0/docs/Database-SQLite-Simple.html#v:toRow
instance ToRow UserInput where
  toRow (UserInput _cmdId md) = toRow (Only md)

updir = "/Users/cat/myfile/bitbucket/haskellwebapp2/uploaddir/"

runSh :: TS.Text -> IO (ExitCode, TS.Text, TS.Text)
runSh x' = TUR.shellStrictWithErr x' TUR.empty

styleChar::String->String->Char->Char->String->String
styleChar l r a b s = foldr(\x' y' -> x' ++ y') [] $ map(\x -> if x == a || x == b then l ++ (x:[]) ++ r else (x:[])) s

-- | -------------------------------------------------------------------------------- 
-- | Thu Nov 15 13:18:04 2018 
-- | Simple web server for request and response
-- | Handle search code snippet
-- | -------------------------------------------------------------------------------- 
-- run.sh => ./wai 
-- ghc -i/$b/haskelllib --make wai.hs -o wai 
-- [file path] [desc] [image src]
href::String->String->String->String
href p n m = [r|<a href='|] <> p <> [r|'>|] <> (baseName p) <> 
             [r|<img src='|] <> m <> [r|' width="10%"  height="10%" /></a>|] 

changeSymbol::String -> String
changeSymbol str = s1 
    where
        s = splitStr "<-" str  -- ["dog", "cat"]
        ss = if takeEnd 1 s  == [""] then init s else init s  -- ["dog", "cat"]
        s1 = (concat (map(\x -> x ++ "<span style=\"color:red;\">&lt;-</span>") ss)) ++ la
        la = if takeEnd 1 s == [""] then "" else last s

-- | Convert [[String]] to ByteString
listToByteStr::[[String]]->BS.ByteString
listToByteStr s = S8.pack $ L.foldr(\x y-> x ++ "<br>" ++ y) [] $ L.foldr(\x y -> x ++ ["<br>"] ++ y) [] s

cssStrong::String->String
cssStrong s = if len > 2 then "<strong>" ++ s ++ "</strong>" else s
            where 
                list = splitRegex(mkRegex ":") s
                len = length list

cssHead::[[String]]->[[String]]
cssHead s = map(\x -> let 
                        len = length $ splitRegex(mkRegex ":") (head x) 
                        in if len > 2 then ("<span style=\"color:gray;\">" ++ (head x) ++ "</span>"):(drop 1 x) else x) s

htmlLess::[[String]]->[[String]]
htmlLess s = (map . map)(\x -> (subRegex r x) "&lt;")  s
        where
            r = mkRegex "<"

htmlGreater::[[String]]->[[String]]
htmlGreater s = (map . map)(\x -> (subRegex r x) "&gt;")  s
        where
            r = mkRegex ">"

keyWord::[[String]]->[[String]]
keyWord s = (map . map)(\x -> (subRegex r x) "<span style=\"color:green;\">\\0</span>")  s
        where
            r = mkRegex "CTRL[a-zA-Z_-]*"

-- latex: \begin{document} \end{document}
keyWord1::[[String]]->[[String]]
keyWord1 s = (map . map)(\x -> (subRegex r x) "<span style=\"color:green;\">\\0</span>")  s
        where
            r = mkRegex "\\\\[a-zA-Z0-9]+{[^}]+}"

keyDash::[[String]]->[[String]]
keyDash s = (map . map)(\x -> (subRegex r x) "<span style=\"color:red;\">\\0</span>")  s
        where
            r = mkRegex "[-+]{10,}"

--keySymbol1::[[String]]->[[String]]
--keySymbol1 s = (map . map)(\x -> (subRegex r x) "<span style=\"color:blue;\">\\0</span>")  s
--        where
--            r = mkRegex "=>|=="

keySymbol1::[[String]]->[[String]]
keySymbol1 s = (map . map)(\x -> changeSymbol x)  s

--keyName::[[String]]->[[String]]
--keyName s = (map . map)(\x -> (subRegex r x) "<span style=\"color:pink; background:#CCF7F7;\">\\0</span>")  s
--        where
--            r = mkRegex "buffer|while|if|endif|Emacs|split|goto"

keyName::[[String]]->[[String]]
keyName s = (map . map)(\x -> x *=~/ 
    [ed|${adr}(\<where\>|\<let\>):?///<span style="color:blue;">${adr}</span>|]) s

specialName::[[String]]->[[String]]
specialName s = (map . map)(\x -> x *=~/ 
    [ed|${adr}(\<new\>|::|\<sizeof\>):?///<span style="color:red;">${adr}</span>|]) s

javaClassName::[[String]]->[[String]]
javaClassName s = (map . map)(\x -> x *=~/ [ed|${adr}(\<interface\>|\<abstract\>|\<implements\>|\<class\>|\< = \>):?///<span style="color:#ef82ee;">${adr}</span>|]) s

        -- let s1 = "mydog dog dog (dog)" ?=~/ [ed|${adr}(\<dog\>):?///< div class="dog">${adr}< /div> |]
-------------------------------------------------------------------------------- 
-- Use following package lang extension and package for word boundary 
-- search and replacement
-- {-# LANGUAGE QuasiQuotes       #-}
-- import Text.RE.TDFA.String

-- add more ClassName here
javaFunClass::[[String]]->[[String]]
javaFunClass s = (map . map)(\x -> x *=~/ 
    [ed|${adr}(\<Vector\>|\<List\>|\<Set\>|\<HashSet\>|\<HashMap\>|\<ArrayList\>|\<Integer\>|\<String\>):?///<span style="color:#218e2b;">${adr}</span>|]) s
    -- it is too slow [ed|${adr}(\<[A-Z][a-z_0-9]*\>):?///<span style="color:#218e2b;">${adr}</span>|]) s
-------------------------------------------------------------------------------- 
javaKeyWords::[[String]]->[[String]]
javaKeyWords s = (map . map)(\x -> x *=~/ 
    [ed|${adr}(\<abstract\>|\<assert\>|\<boolean\>|\<break\>|\<byte\>|\<case\>|\<catch\>|\<char\>|\<class\>|\<const\>|\<continue\>|\<default\>|\<do\>|\<double\>|\<else\>|\<enum\>|\<extends\>|\<final\>|\<finally\>|\<float\>|\<for\>|\<goto\>|\<if\>|\<implements\>|\<import\>|\<instanceof\>|\<int\>|\<interface\>|\<long\>|\<native\>|\<new\>|\<package\>|\<private\>|\<protected\>|\<public\>|\<return\>|\<short\>|\<static\>|\<strictfp\>|\<super\>|\<switch\>|\<synchronized\>|\<this\>|\<throw\>|\<throws\>|\<transient\>|\<try\>|\<void\>|\<volatile\>|\<while\>):?///<span style="color:#f50a93;">${adr}</span>|]) s

-------------------------------------------------------------------------------- 
javaCmdKeyWords::[[String]]->[[String]]
javaCmdKeyWords s = (map . map)(\x -> x *=~/ 
    [ed|${adr}(\<java\>|\<javac\>|\<javadoc\>|\<jar\>):?///<span style="color:#35A993;">${adr}</span>|]) s

-------------------------------------------------------------------------------- 

mysqlKeyWords::[[String]]->[[String]]
mysqlKeyWords s = (map . map)(\x -> x *=~/ 
    [ed|${adr}(\<insert\>|\<create\>|\<from\>|\<select\>|\<table\>|\<into\>):?///<span style="color:#FF69B4;">${adr}</span>|]) s
-------------------------------------------------------------------------------- 

keyURL::[[String]]->[[String]]
keyURL s = (map . map)(\x -> (subRegex r x) "<a href=\"\\1\">\\1</a>")  s
        where
            r = mkRegex "(https?://[[:graph:]]+)"

spChar::[[String]]->[[String]]
spChar s = (map . map)(\x -> styleChar l r a b x) s
        where
            l = "<span style=\"color:red;\">"
            r = "</span>"
            a = '{' 
            b = '}' 

bracketChar::[[String]]->[[String]]
bracketChar s = (map . map)(\x -> styleChar l r a b x) s
        where
            l = "<span style=\"color:blue;\">"
            r = "</span>"
            a = '(' 
            b = ')' 

sbChar::[[String]]->[[String]]
sbChar s = (map . map)(\x -> styleChar l r a b x) s
        where 
            l = "<span style=\"color:#e012cd;\">"
            r = "</span>"
            a = '[' 
            b = ']' 

-- compose all Regex subRegex
transform = 
            cssHead.
            spChar.
            bracketChar.
            sbChar.
            specialName.
            javaClassName.
            javaFunClass.
            javaKeyWords.
            javaCmdKeyWords.
            mysqlKeyWords.
            keyWord.keyWord1.
            keyURL.
            keyDash.
            keyName.
            (htmlLess.htmlGreater)

{-| 
    === Hide all the data in TextArea
    @
    <form action="sendConfirmation.php" name="confirmationForm" method="post">
    <textarea id="confirmationText" class="text" cols="86" rows ="20" name="confirmationText"></textarea>

    <input type="submit" value="Email" class="submitButton">
    </form>

    <textarea cols="20" rows="20" id="textArea" style="display:none;font-size:18px;" class="hide"></textarea>

    <textarea autofocus="true" onfocus="textAreaAdjust(this);"></textarea>
    @
-} 
hiddenForm::Integer -> String -> String  
hiddenForm n s = [r|  
       <form action="/update" name="Update" class="hf" id=|] <> cid "f" n <> 
       [r| method="POST"><textarea name="header" rows="20" class="hide"> |] <> (head $ lines s) <> 
       [r| </textarea><textarea name="myblock" spellcheck="false" autofocus="true" onfocus="textAreaAdjust(this);" id= |] <> "t" ++ (sw n) <> 
       [r| class="hide"> |] <> s <> 
       [r| </textarea><div class="butcen">
            <input type="submit" name="update" value="update" id= |] <> cid "b" n <> [r| class="submitButton"> 
            <input type="submit" name="add" value="add" id= |] <> cid "a" n <> [r| class="submitButton"> 
            <input type="submit" name="delete" value="delete" id= |] <> cid "d" n <> [r| class="submitButton"> 
            </div> </form> |]
      where
        sw = show
        cid s n = show $ s ++ show n

-- In Java
-- Function f = x -> x + 1
-- BiFunction f = (x, y) -> x + y
-- 
-- gx /Library/WebServer/Documents/zsurface/image/foldlistimage.jpg 
-- 
-- foldr(\x y -> [div] ++ x ++ [cdiv] ++ brr + y) (0, []) zhtml
-- 
-- The id can be used to for TextArea editor
-- e.g.
--  <TextArea onclick="editfun()" ></TextArea>
-- 
-- <script>
-- function editfun(){
-- 
-- }
-- 
-- </script>
-- 
-- See file gf: /Users/cat/myfile/bitbucket/html/showTextAreaOnClick.html
-- 
-- <div id=\"3\" style=\"kk\"> code1 </div> 
-- <div id=\"4\" style=\"kk\"> code2 </div> 
-- 
-- ([[String]] -> [[String]]) 
-- stylish allBlock
--
foldListList::([[String]]->[[String]])->[[String]]->String
foldListList stylish allBlock = L.foldr(\x y -> x + br + y) []  
               $ L.foldr(\x y -> x + y) [] zhtml    -- f s => [[String]]
               where
                code = zip (stylish allBlock) allBlock -- code => stylish code
                zhtml = zipWith(\n (x, b) ->[hiddenForm n (unlines b)] +
                                 [preT $ (onclick_ $ fun "showandhide" (ts n)) + (class_ $ "co" +| n) + (id_ $ "c" +| n)] +
                                 [div_ ac] + x + [cdiv] + [cpre]) [1..] code -- n is Block id
                br          =  "<br>"
                brr         =  ["<br>"]
                cdiv        =  "</div>"
                cpre        =  "</pre>"
                ao          =  "<"
                ac          =  ">"
                divo        =  "<div "
                div_ s      =  "<div " + s
                ts          =  intToString
                (+)         =  (++)
                (+|) s n    =  s + (ts n)
                fun s arg   =  s + "(" + arg + ")"

foldListListTxt::[[String]]->String
foldListListTxt allBlock = L.foldr(\x y -> x ++ "\n" ++ y) []  
                           $ L.foldr(\x y -> x ++ ["\n"] ++ y) [] allBlock    -- f s => [[String]]





-- /Library/WebServer/Documents/zsurface/pdf
pdfname   = "Very Important File"
img     = "img.png"
pdfPath = "/Library/WebServer/Documents/zsurface/pdf"
docRoot = "/Library/WebServer/Documents/zsurface"
doc     = ""
cmdLog  = "/Users/cat/myfile/bitbucket/testfile/waiCmdLog.txt"

currCmdFile = "/Users/cat/myfile/bitbucket/testfile/currCmd.txt"

logCurrCmd::[String] -> IO()
logCurrCmd cs = writeToFile currCmdFile cs 

readCurrCmd::IO String
readCurrCmd = readFileLatin1 currCmdFile

type HMap = M.HashMap String [[String]] 

genePDF::String->IO() 
genePDF p = do 
    f <- A.lsFile p 
    -- mapM print f
    A.fl
    let list = map(\x -> (href (doc </> x) pdfname img)  ++ "<br>")  f
    -- mapM print list
    A.writeToFile ("./pdf.html") list 

-- type Application = Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived
app::Connection -> IORef HMap->Application
-- app conn1 ref request respond = case rawPathInfo request of
app conn1 ref request respond = case pathInfo request of 
      ("test":_)       -> respond $ responseNothing "test nothing"
      ("raw":_)        -> respond plainIndex
      ("up":_)         -> respond uploadPage
      ("insertinfo":_) -> respond insertinfo
      ("listPage":_)   -> listPage conn1 request respond
      ("insertUser":_) -> respond insertUser
      ("login":_)      -> respond loginHtml
      ("genepdf":_)    -> respond $ responseGenePDFHtml conn1
      ("pdf":fn:_)     -> do
                            let fname = let p = rawPathInfo request 
                                        in last $ filter(\x -> BS.length x > 0 ) $ A.splitBS (c2w_ '/') p  
                            print fname
                            respond $ pdfSent $ strictTextToStrictByteString fn 
      ("loginCheck":_)      -> loginCheck conn1 request respond
      ("insertUserDB":_)    -> insertUserDB conn1 request respond
      ("insert":_)          -> insertDatabase conn1 request respond
      ("upload":_)          -> upload updir request respond
      ("getjson":_)         -> upload updir request respond
      ("snippet":_)         -> respond $ anyRoute conn1 ref request   -- anyRoute => Response, respond (Response) => IO ResponseReceived
      ("json":_)            -> geneRectMat request respond
      ("update":_)          -> updateMap ref request respond -- update textarea data
      ("editor":_)          -> respond replyEditor
      ("search":_)          -> respond searchUI
      ("wordcount":_)       -> respond replyCssButton
      ("wordcount_reply":_) -> respond wordcountReply
      ("matrix":_)          -> respond matrixReply
      ("compiler":_)        -> receiveCode request respond
      ("editcode":_)        -> respond $ responseHtml "compileCode.html"
      ("aronlib.js":_)      -> respond $ responseJavascript "aronlib.js"
      _                     -> respond $ responseHelp

plainIndex::Response
plainIndex = responseFile
    status200
    [("Content-Type", "text/html")]
    "index.html"
    Nothing

pdfFile::Response
pdfFile = responseFile
    status200
    [("Content-Type", "text/html")]
    "pdf.html"
    Nothing
    
-- can not open pdf from browser
pdfSent::BS.ByteString -> Response
pdfSent fn = responseFile
    status200
    [("Content-Type", "application/pdf"), ("Content-Disposition", "inline;filename=" <>  fn)]
    (BU.toString $ pdfdir <> fn)
    Nothing
    where
        pdfdir = "pdf/"

insertinfo::Response
insertinfo = responseFile
    status200
    [("Content-Type", "text/html")]
    "insert.html"
    Nothing

insertUser::Response
insertUser = responseFile
    status200
    [("Content-Type", "text/html")]
    "insertUser.html"
    Nothing

loginHtml::Response
loginHtml = responseFile
    status200
    [("Content-Type", "text/html")]
    "login.html"
    Nothing

searchUI::Response
searchUI = responseFile
    status200
    [("Content-Type", "text/html")]
    "searchUI.html"
    Nothing


notFound :: Response
notFound = responseLBS
    status404
    [("Content-Type", "text/plain")]
    "404 - Not Found"


notFoundStr::IN.ByteString->Response
notFoundStr s = responseLBS
    status404
    [("Content-Type", "text/plain")]
    s 

-- let path = "/Users/cat/myfile/bitbucket/snippets/snippet_test.m
-- snippetPath = "/Users/cat/myfile/bitbucket/snippets/snippet.hs"
snippetP = "myfile/bitbucket/snippets/snippet.hs"
-- snippetPath = "/Users/cat/myfile/bitbucket/snippets/snippet_test.hs"



insertAll::[(String, [[String]])] -> HMap -> HMap
insertAll [] m = m 
insertAll (x:cx) m = insertAll cx (M.insert (fst x) (snd x) m)

mapClear::[String] -> HMap -> HMap
mapClear [] m = m 
mapClear (x:cx) m = mapClear cx (M.delete x m)


{-| 
    === read snippet file
    __NOTE__ The code can be speed up a bit, change [String] [[String]

    >type HMap = M.HashMap String [[String]] => type HMap = M.HashMap String (Set [String])
-} 
snippetMap::[([String], [String])] -> IORef HMap -> IO ()
snippetMap pplist ref = do
        -- let path = "/Users/cat/myfile/bitbucket/snippets/snippet_test.hs"
        -- let path = "/Users/cat/myfile/bitbucket/snippets/snippet.hs"

        -- readSnippet::FilePath->IO [([String], [String])]
        -- pplist <- readSnippet path 
        let keylist = L.map(\x -> 
                                (foldr(++) [] $ L.map(\y -> prefix y) (fst x),
                                 snd x
                                )
                                
                            ) pplist 


        let mymap = map(\cx -> [(x, y) | x <- fst cx, y <- [snd cx]]) keylist              
        let lmap = foldr(++) [] mymap                                                      
        let sortedList = qqsort(\x y -> f x y) lmap                                        
              where f x y = fst x > fst y                                
        let mmap = M.fromList lmap                                                         
        let group= groupBy(\x y -> f x y) sortedList                                       
              where f x y = fst x == fst y                                 
        
        --
        -- unzip::[("dog", "dogs"), ("cat", "cats")] => (["dog", "cat"], ["dogs", "cats"])
        let uzip = map(\x -> unzip x) group

        -- fix bug: unique $ snd x => remove duplicated values
        -- cause duplicated blocks: let tupleList = map(\x -> (head . fst $ x, snd x)) uzip
        let tupleList = map(\x -> (head . fst $ x, unique $ snd x)) uzip

        let hmap = M.fromList tupleList  -- [("haskell", [["dog", "line1"], ["cat", "line2"]])]
        -- return hmap
        modifyIORef ref (insertAll tupleList)
        return () 


-- | http://localhost:8000/snippet?id=keyinput
-- | Conver ByteString to String or vice versa 
-- | span block code: <span>text</span>
-- | 
-- | type in search key
-- | transform a block of code => colourful block of code
-- | each block of code => key -> "block of code"
-- | return a block of colourful code
spanBlock::HMap->(Maybe BS.ByteString)->String
spanBlock hmap mKey = foldListList f $ case (M.lookup (S8.unpack $ fromJust mKey) hmap) of 
                                     Just s -> s 
                                     _      -> [["span Block: nothing"]]
                                where
                                    f = transform -- f = id => if we don't have any style 
                                    
spanBlockX::([[String]]->[[String]])-> HMap->(Maybe BS.ByteString)->String
spanBlockX f hmap mKey = foldListList f $ case (M.lookup (S8.unpack $ fromJust mKey) hmap) of 
                                     Just s -> s 
                                     _      -> [["span Block: nothing"]]
                
spanBlockXX::HMap->(Maybe BS.ByteString)->String
spanBlockXX hmap mKey = foldListListTxt $ case (M.lookup (S8.unpack $ fromJust mKey) hmap) of 
                                     Just s -> s 
                                     _      -> [["span Block: nothing"]]

(∘) = (++)

htmlForm::String -> String
htmlForm s = [r| 
             <div style="text-align:center;"> 
             <form action="/snippet" method="get" target=""> 
                 <input type="text" style="font-size:18pt;height:50px;width:400px;" name="id" list="autocomplete">  
                 <datalist id="autocomplete">" |] <> s <> [r| </datalist><br>  
             </form> 
             </div> |]
            
htmlPre::String -> String
htmlPre s = [r| <pre style="font-size:29px;white-space: pre-wrap;" id="id00"> |] <> s <> [r| </pre> |]
  
-- htmlBody $ (htmlForm s) ++ (htmlPre s1)
-- htmlBody $ htmlForm listCmd $ htmlPre retStr 
htmlBody::String -> String
htmlBody s  = [r|
            <HTML>   
            <HEAD>   
            <meta charset="utf-8">
            <TITLE>Search Code Snippet</TITLE> 
            <LINK rel="stylesheet" type="text/css" href="/style.css"> 
            <style>

            body {
             background-color: #8a9598b5;
             margin: 0 auto;
             max-width: 1200px;
             border: 1pt solid #695b43;
             padding: 4px;
            }    

            .colorclick{
                background-color:#f1e6e6;
            }
            .form{
                margin-block-end:1px;
            }
            .hf{
                display:none;
            }

            pre {
                display: block;
                font-family: monospace;
                font-size: 14pt;
                white-space: pre;
                margin-top: 1px;
                margin-right: 1px;
                margin-bottom: 1px;
                margin-left: 1px;
                background: #fdfbea69;
                border-style: solid;
                border-width: thin;
            }

            textarea {
                border:1px solid #999999;
                font-size: 13pt;
                width:100%;
                height:150px;
                margin:1px 0;
                padding:3px;
                display:none;
                font-family: monospace;
                transition: border-color .15s ease-in-out, box-shadow .15s ease-in-out;
                outline: 0;
                box-shadow: none;
                border-radius: 0;
                box-sizing: content-box;
                background: #e8e4db;
                padding: 1px 0;
                resize: yes;
            }
            .submitButton{
                color: red;
                font-size:22pt;
                font-family: monospace;
                font-weight:bold;
                display:none;
            }

            .butcen{
                display: inline-flex;
                float:right;
            }

            .butshow{
                /* display: inline-flex; */
                /* text-align: center; */
                font-size: 14pt;
                margin: 2px 2px;
                cursor: pointer;
                background-color: #ffffff;
                color: #000000;
                border: 1px solid #212921; 

            }
            </style>
            <script>

            function textAreaAdjust(o) {
                o.style.height = "1px";
                o.style.height = (25+o.scrollHeight)+"px";
            }

            function showandhide(id) {

                 var formobj=document.getElementById('f' + id);
                 if(formobj.className == 'hf'){  //check if classname is hide 
                    formobj.style.display = 'block';
                    formobj.className ='hfshow';
                 }else if(formobj.className == 'hfshow'){
                    formobj.style.display = 'none';
                    formobj.className ='hf';
                 }

                 var selectedobj=document.getElementById('t' + id);
                 if(selectedobj.className == 'hide'){  //check if classname is hide 
                    selectedobj.style.display = 'block';
                    selectedobj.className ='show';
                 }else if(selectedobj.className == 'show'){
                    selectedobj.style.display = 'none';
                    selectedobj.className ='hide';
                 }

                 var butobj=document.getElementById('b' + id);
                 if(butobj.className == 'submitButton'){  //check if classname is hide 
                    butobj.style.display = 'inline-flex';
                    butobj.className ='butshow';
                 }else if(butobj.className == 'butshow'){
                    butobj.style.display = 'none';
                    butobj.className ='submitButton';
                 }

                 var butobj=document.getElementById('a' + id);
                 if(butobj.className == 'submitButton'){  //check if classname is hide 
                    butobj.style.display = 'inline-flex';
                    butobj.className ='butshow';
                 }else if(butobj.className == 'butshow'){
                    butobj.style.display = 'none';
                    butobj.className ='submitButton';
                 }

                 var butobj=document.getElementById('d' + id);
                 if(butobj.className == 'submitButton'){  //check if classname is hide 
                    butobj.style.display = 'inline-flex';
                    butobj.className ='butshow';
                 }else if(butobj.className == 'butshow'){
                    butobj.style.display = 'none';
                    butobj.className ='submitButton';
                 }

                 var butobj=document.getElementById('c' + id);
                 var cname = 'co' + id;
                 if(butobj.className == cname){  
                    butobj.className = 'colorclick';
                 }else if(butobj.className == 'colorclick'){
                    butobj.className = cname; 
                 }

            }
            </script>

            </HEAD>
            <BODY> |] <> s <> [r| </BODY></HTML> |]


replyHtml::String->String->String
replyHtml s listCmd = [r|
            <HTML>   
            <HEAD>   
            <meta charset="utf-8">
            <TITLE>Search Code Snippet</TITLE> 
            <LINK rel="stylesheet" type="text/css" href="/style.css"> 
            </HEAD>
            <BODY> 

            <div style="text-align:center;">
            <form action="/snippet" method="get" target=""> 
            <input type="text" style="font-size:16pt;height:40px;width:400px;" name="id" list="autocomplete">  
            <datalist id="autocomplete"> |] <>  listCmd <> [r| </datalist> 
            <br> 
            </form> 
            </div> |] <> s <> [r| </BODY></HTML> |]



{-| 
    snippet?id=queryStr
    S8.unpack: ByteString to String
    type Application = Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived
    anyRoute => Response
-} 
anyRoute::Connection -> IORef HMap -> Request-> Response
anyRoute conn ref req =
    -- get query from client
    -- look up the value of id, e.g. snippet?id=value
    -- Maybe s 
    -- search s from the HMap
    -- replace the format html if any value is found
    -- Otherwise, reply "nothing"
    let query = queryString req :: [(BS.ByteString, Maybe BS.ByteString)]
        idParam = join $ lookup "id" query :: Maybe BS.ByteString
    in case b2s <$> idParam of  
            -- responseBuilder :: Status -> ResponseHeaders -> Builder -> Response
            Just s -> do 
                      -- record command and write to file
                      case s of
                           var | len var > 1 -> case take 2 s of
                                                     var | var == "x " -> responseXCmd s         -- forget what it is for.
                                                         | var == "c " -> responseCmd conn s     -- Shell commands
                                                         | var == "j " -> responseJavaHtml s     -- Java AronLib.java with Html, css.
                                                         | var == "h " -> responseHaskellHtml s  -- Haskell AronModule.hs with Html, css.
                                                         | var == "k " -> queryLibHaskell s    -- Haskell AronModule.hs
                                                         | var == "i " -> queryLibJava s       -- Java $b/javalib/AronLib.java
                                                         | var == "p " -> queryLibJavaPackage "Print." s -- Java $b/javalib/Print.java
                                                         | var == "n " -> responseSnippetTxt s ref  -- Snippet with NO Html, css.
                                                         | otherwise   -> responseSnippetHtml conn s ref  -- Snippet with Html, css.
                               | otherwise   -> responseNothing ""
            _      -> responseNothing ""
        where 
            b2s = strictTextToStr . strictByteStringToStrictText

-- | http://localhost:8000/up/
-- | NOTE: file => /upload dir
-- | Plz see uploadPage.html 
-- | /Users/cat/myfile/bitbucket/haskellwebapp2/uploadPage.html
-- | responseFile :: H.Status -> H.ResponseHeaders -> FilePath -> Maybe FilePart -> Response
uploadPage::Response
uploadPage = responseFile
    status200
    [("Content-Type", "text/html")]
    "uploadPage.html"
    Nothing

data SearchType = CmdT | JavaT | HaskellT | SnippetT

readCmd::FilePath -> SearchType -> IO String
readCmd fn t = do
               -- cmdList  <- readFileToList fn
               cmdList  <- readFileLatin1ToList fn
               -- filter out "va" from list and apply f to each element
               let sortedList = groupCountFilter cmdList 
               let tupList = show <$> groupCount sortedList
               writeToFile "/tmp/aa.x" tupList
               -- let sortedList = ["33", "11", "22", "bb", "aa"]
               let htmlStr = concat $ map(\x -> let left = "<option value=\""; 
                                                    right = "\">" 
                                                in case t of 
                                                        CmdT     -> left ++ x ++ right
                                                        JavaT    -> left ++ x ++ right
                                                        HaskellT -> left ++ x ++ right
                                                        SnippetT -> left ++ x ++ right
                                         ) $ sortedList 
               return htmlStr 


{-| 
    === Filter some commands out from a list.
-} 
groupCountFilter::[String] -> [String]
groupCountFilter cs = fst <$> groupCount (let f::String -> Maybe String
                                              f "va" = Nothing
                                              f s    = Just s
                                          in filtermap ( f . trimBoth) cs)
optionHtml::[String] -> String
optionHtml cs = concat $ map(\x -> [r| <option value=" |] <> x <> [r|">|]) cs 

--optionHtml::[String] -> String
--optionHtml cs = htmlStr 
--            where
--               sortedList =  fst <$> groupCount (let f::String -> Maybe String
--                                                     f "va" = Nothing
--                                                     f s    = Just s
--                                                 in filtermap ( f . trimBoth) cs)
--               htmlStr = concat $ map(\x -> [r| <option value=" |] <> x <> [r|">|])  sortedList 

responseNothing::String -> Response                                                    
responseNothing s = responseStream                                                   
              status200                                                            
              [(hContentType, "text/html")] $ \write flush -> do                   
              write $ byteString $ toBS ("responseNothing : " ++ s)

responseNothingBS::BS.ByteString -> Response                                               
responseNothingBS bs = responseStream                                                   
              status200                                                            
              [(hContentType,  "application/json")] $ \write flush -> do                   
              write $ byteString bs  

responseNothingTest::Response                                               
responseNothingTest = responseStream                                                   
              status200                                                            
              [(hContentType,  "application/pdf"),
               ("Content-Disposition", "inline;filename=kkk.pdf")] $ \write flush -> do                   
              write $ byteString $ toStrictBS "dog"  

replyTaskHtml::BS.ByteString -> BS.ByteString 
replyTaskHtml s = [r|
            <HTML>   
            <HEAD>   
            <meta charset="utf-8">
            <TITLE>Search Code Snippet</TITLE> 
            <LINK rel="stylesheet" type="text/css" href="/style.css"> 
            </HEAD>
            <BODY> 
            <div style="text-align:center;">
            <br>
            <p> |] <> s <> [r|</p><br><a href= |] <> (toBS WC.hostURL) <> [r|>Back</a></div></BODY></HTML> |]


            -- <p> |] <> s <> [r|</p><br><a href="http://localhost:8000">Back</a></div></BODY></HTML> |]
            -- <p> |] <> s <> [r|</p><br><a href=|] <> (url WC.host WC.port) <> [r|>Back</a></div></BODY></HTML> |]

              
listPageHtml::BS.ByteString -> BS.ByteString 
listPageHtml s = [r|
            <HTML>   
            <HEAD>   
            <meta charset="utf-8">
            <TITLE>Search Code Snippet</TITLE> 
            <LINK rel="stylesheet" type="text/css" href="/style.css"> 
            </HEAD>
            <BODY> 
            <div style="text-align:center;">
            <br>
            <p>|] <> s <> [r|</div></BODY></HTML>|]

--listPage::BS.ByteString -> Response                                               
--listPage bs = responseStream                                                   
--              status200                                                            
--              [(hContentType,  "text/html")] $ \write flush -> do                   
--              write $ byteString bs  

listPage::Connection -> Application
listPage conn req response = do
              userList <- query_ conn "SELECT uid, name, email, password, task, money FROM user" :: IO [User]
              let listTask = BS.concat $ map (\x -> [r|<div>|] <> (t2b $ task x) <> [r|</div><br>|]) userList -- => TS.Text
              -- response $ responseTaskBS (task (head userList))
              response $ responseTaskBS $ replyTaskHtml listTask 
        where
            t2b = strictTextToStrictByteString

responseTaskBS::BS.ByteString -> Response                                               
responseTaskBS bs = responseStream                                                   
               status200                                                            
               [(hContentType,  "text/html")] $ \write flush -> do                   
               write $ byteString bs  



--writeSB::String -> Builder
--writeSB = write $ byteString $ strToStrictByteString 

  -- write $ byteString $ toBS $ replyHtml (escapeHtml retStr) listCmd 

{-|
   === Response output from shell command
   1. The maximum number of lines are 200, " | head -200"

   2. Exit code can not be checked, ExitSuccess
   
   Wed May  8 23:10:41 2019 
   3 Error can be checked in stderr  
   >(e, so, si) <- A.runSh $ toText (drop 2 ncmd) 
   > if si is NOT empty then there is stderr

   >type streamBody = (Builder -> IO()) -> IO() -> IO()
 -} 
responseCmd::Connection -> String -> Response
responseCmd conn cmd = responseStream 
              status200
              [(hContentType, "text/html")] $ \write flush -> do
              -- code <- A.run (cmd ++ "\n")
              -- There is problem with top 500 lines
              -- It seems to be working with 200 lines 
              let ccmd = trimBoth cmd
              let ncmd = ccmd ++ topN
                  
              -- Log the current user input. 
              logCurrCmd [ccmd]
              -- runSh :: TS.Text -> IO (ExitCode, TS.Text, TS.Text)    
              (e, so, si) <- runSh $ toText (drop 2 ncmd) 
              -- ExitCode can not capture exit code of cmd1 in "cmd1 | cmd2"
              -- Wed May  8 23:27:04 2019 
              -- Fixed Error: 
              -- If there is error, si will NOT be empty String
              -- Otherwise, there is NO error.
              let ok = isOk si 
              let shellRet = if ok then (toStr so) else ("Invalid Shell Command:" ++ ncmd)
              if ok then writeToFileAppend cmdLog [ccmd] else return () 
              if ok then do
                  execute_ conn "CREATE TABLE IF NOT EXISTS userinput (id INTEGER PRIMARY KEY AUTOINCREMENT, xcmd TEXT)"
                  execute conn "INSERT INTO userinput (xcmd) VALUES (?)" (UserInput 0 (toText ccmd))
                  cmdsql <- query_ conn "SELECT id, xcmd FROM userinput"::IO [UserInput]
                  let cmdList = map toStr (map (xcmd) cmdsql::[Text]) -- [UserInput] => [Text] => [String]
                  pa cmdList 
                  let sortList = groupCountFilter cmdList 
                  write $ byteString $ toBS $ htmlBody $ (htmlForm (optionHtml sortList)) ∘ (htmlPre shellRet)
              else return () 
--               listCmd <- readCmd cmdLog CmdT
              -- write $ byteString $ toBS $ replyHtml (escapeHtml shellRet) listCmd 
              -- write $ byteString $ toBS $ htmlBody $ (htmlForm listCmd) ∘ (htmlPre shellRet)
              flush
        where
            topN = " | head -200"
            toText = strToStrictText
            toStr = strictTextToStr
            s2t = strictByteStringToStrictText
            isOk si = (toStr si) == ""

responseXCmd::String -> Response
responseXCmd cmd = responseStream 
              status200
              [(hContentType, "text/html")] $ \write flush -> do
              -- code <- A.run (cmd ++ "\n")
              -- There is problem with top 500 lines
              -- It seems to be working with 200 lines 
              let ccmd = trimBoth cmd
              let ncmd = ccmd ++ topN
                  
              -- runSh :: TS.Text -> IO (ExitCode, TS.Text, TS.Text)    
              (e, so, si) <- runSh $ toText (drop 2 ncmd) 
              let isError = (toStr si) == ""
              let shellRet = if isError then (toStr so) else ("responseXCmd: Invalid Shell Command:" ++ ncmd)
              if isError then writeToFileAppend cmdLog [ccmd] else return () 
              listCmd <- readCmd cmdLog CmdT
              -- write $ byteString $ toBS $ replyHtml (escapeHtml shellRet) listCmd 
              -- write $ byteString $ toBS $ replyHtml ("try it") listCmd 
              write $ byteString $ toBS $ htmlBody $ (htmlForm listCmd) ∘ (htmlPre shellRet)
              flush
        where
            topN = ""
            toText = strToStrictText
            toStr = strictTextToStr

responseJavaHtml::String -> Response
responseJavaHtml cmd = responseStream 
              status200
              [("Content-Type", "text/html")] $ \write flush -> do
              let tcmd = trimBoth cmd              -- " j  list " => "j  list"
              let hKey  = trimBoth $ drop 2 tcmd   -- "j  list"   => "list"
              let jCmd = redisKey hKey             -- "list"      => "Aron.list"
              ls <- A.run $ query_redis ++ jCmd   
              let lsEscapeHtml = map(\x -> escapeHtml x) ls
              let ls2 = map(\s -> [s]) lsEscapeHtml 
              -- let repStr = foldr(\x y -> x ++ "<br>" ++ y) [] code
              let repStr = table ls2 
              writeToFileAppend cmdLog [tcmd] 
              listCmd <- readCmd cmdLog JavaT
              write $ byteString $ toBS $ replyHtml repStr listCmd 
              flush
              where
                redisKey s = "Aron." ++ s
                query_redis = "/Users/cat/myfile/symbin/redis_query.hs "
              
responseGenePDFHtml::Connection -> Response                                                 
responseGenePDFHtml conn = responseStream                                                
              status200                                                              
              [("Content-Type", "text/html")] $ \write flush -> do
              bs <- PDF.pdfMain conn "pdf/"
              write $ byteString bs  
              flush                                                                  

  
responseHaskellHtml::String -> Response
responseHaskellHtml cmd = responseStream 
              status200
              [("Content-Type", "text/html")] $ \write flush -> do
              -- append "AronModule." Reddis for Haskell lib
              let tcmd = trimBoth cmd
              let hKey = redisKey $ trimBoth $ drop 2 tcmd
              code <- A.run $ query_redis ++ hKey   
              let codeEsc = map(\x -> escapeHtml x) code
              let repStr = foldr(\x y -> x ++ "<br>" ++ y) [] codeEsc
              writeToFileAppend cmdLog [tcmd] 
              listCmd <- readCmd cmdLog HaskellT 
              write $ byteString $ toBS $ replyHtml repStr listCmd 
              flush
              where
                query_redis = "/Users/cat/myfile/symbin/redis_query.hs "
                redisKey s = "AronModule." ++ s

{-| 
    === query function info from redis without Html

    >query_redis = "/Users/cat/myfile/symbin/redis_query.hs "

    >preKey = "AronModule."  AronModule.hs
    >preKey = "Aron."        Aron.java
-}
queryLibHaskell::String -> Response
queryLibHaskell cmd = responseStream 
              status200
              [("Content-Type", "text/html")] $ \write flush -> do
              let tcmd = trimBoth cmd
              putStrLn cmd
              let hKey = preKey ++ (trimBoth $ drop 2 tcmd)
              code <- A.run $ query_redis ++ hKey   

              -- split function, "::" and sort the name according to shortest len
              let tcode = qqsort(\a b -> let la = (len . head) $ splitStr "::" a 
                                             lb = (len . head) $ splitStr "::" b 
                                         in  la < lb ) $ map trimBoth code
              if len tcode > 0 
              then let repStr = init $ foldr(\x y -> x ++ "\n" ++ y) [] tcode in write $ byteString $ toBS repStr 
              else write $ byteString $ toBS [] 
              flush
              where
                query_redis = "/Users/cat/myfile/symbin/redis_query.hs "
                preKey = "AronModule."

queryLibJava::String -> Response
queryLibJava cmd = responseStream 
              status200
              [("Content-Type", "text/html")] $ \write flush -> do
              let tcmd = trimBoth cmd
              putStrLn cmd
              let hKey = preKey ++ (trimBoth $ drop 2 tcmd)
              code <- A.run $ query_redis ++ hKey   
              let tcode = map trimBoth code
              if len tcode > 0 
              then let repStr = init $ foldr(\x y -> x ++ "\n" ++ y) [] tcode in write $ byteString $ toBS repStr 
              else write $ byteString $ toBS [] 
              flush
              where
                query_redis = "/Users/cat/myfile/symbin/redis_query.hs "
                preKey = "Aron."

queryLibJavaPackage::String -> String -> Response
queryLibJavaPackage preKey cmd = responseStream 
              status200
              [("Content-Type", "text/html")] $ \write flush -> do
              let tcmd = trimBoth cmd
              putStrLn cmd
              -- preKey = Print.  tcmd = "p list"
              -- => hKey = Print.list
              let hKey = preKey ++ (trimBoth $ drop 2 tcmd)
              code <- A.run $ query_redis ++ hKey   
              let tcode = map trimBoth code
              if len tcode > 0 
              then let repStr = init $ foldr(\x y -> x ++ "\n" ++ y) [] tcode in write $ byteString $ toBS repStr 
              else write $ byteString $ toBS [] 
              flush
              where
                query_redis = "/Users/cat/myfile/symbin/redis_query.hs "
                -- preKey = "Aron."

{-| 
    Get user input: cmd = "s java regex"
    1. remove spaces from cmd
    2. insert cmd to table: userinput if userinput exists, otherwise create table: userinput
        1. sorted all cmd and create Html form with all cmd
        2. create Html output from cmd query. 
-} 
responseSnippetHtml::Connection -> String -> IORef HMap-> Response
responseSnippetHtml conn cmd ref = responseStream
              status200
              [("Content-Type", "text/html")] $ \write flush -> do
              let sCmd = (trimBoth cmd)

              -- store user input commands in a table: userinput
              -- if table does not exist, create one, otherwise insert data to table: userinput
              logCurrCmd [sCmd]
              execute_ conn sql_create_table 
              execute conn sql_insert (UserInput 0 (toText cmd))
              cmdsql <- query_ conn sql_select ::IO [UserInput]
              let cmdList = let ls = map (xcmd) cmdsql::[Text] in map toStr ls::[String]
              pa cmdList 

              let sortList = groupCountFilter cmdList 
              pa sortList 
              writeToFileAppend cmdLog [sCmd] 
              listCmd <- readCmd cmdLog SnippetT 
              -- write $ byteString $ toBS $ replyHtml (spanBlock hmap (Just (toBS (drop 2 sCmd)) )) listCmd 
              hmap <- readIORef ref
              -- write $ byteString $ toBS $ htmlBody  $ (htmlForm listCmd) ++ (spanBlockX transform hmap (Just (toBS (drop 2 sCmd)) ))
              write $ byteString $ toBS $ htmlBody  $ (htmlForm (optionHtml sortList)) ++ (spanBlockX transform hmap (Just (toBS (drop 2 sCmd)) ))

              -- write $ byteString $ toBS $ htmlBody $ (htmlForm listCmd) ∘ (htmlPre shellRet)
              flush
              where 
                toText = strToStrictText
                toStr = strictTextToStr
                sql_create_table = "CREATE TABLE IF NOT EXISTS userinput (id INTEGER PRIMARY KEY AUTOINCREMENT, xcmd TEXT)"
                sql_insert = "INSERT INTO userinput (xcmd) VALUES (?)" 
                sql_select = "SELECT id, xcmd FROM userinput"


{-| 
    === query snippet from HMap without Html
-} 
responseSnippetTxt::String -> IORef HMap-> Response
responseSnippetTxt cmd ref = responseStream
              status200
              [("Content-Type", "text/html")] $ \write flush -> do
              let sCmd = (trimBoth cmd)
              putStrLn cmd
              writeToFileAppend cmdLog [sCmd] 
              hmap <- readIORef ref 
              write $ byteString $ toBS $ spanBlockXX hmap (Just (toBS (drop 2 sCmd)) )
              flush

-- lazy ByteString
trimBothLBS::IN.ByteString -> IN.ByteString
trimBothLBS s = ss 
    where
        bs = LC.dropWhile isSpace s -- remove leading space
        ss = LC.foldr(\x y -> if (isSpace x && y == e) then e else LC.cons x y) e bs   -- remove trailing space
        e  = LC.empty

geneRectMat::Application
geneRectMat req response = do 
        str <- requestBody req

        let may = DA.decode $ LA.fromStrict str :: Maybe GeneMatrix 
        fw "may"
        print may
        let matJson = case may of 
                    (Just x) -> x 
                    _        -> GeneMatrix{cmd = "", ncol = 0, nrow=0} 
        fw "matJson"
        print matJson
        let gmatrix = case (cmd matJson) of 
                                "genematrix" -> let nc = (ncol matJson) 
                                                    nr = (nrow matJson)
                                                in MatInt{name = "matrix", matrix = geneMatMN nc nr}
                                _            -> MatInt{name = "", matrix = []} 
        let gbs = toStrictBS $ DA.encode $ gmatrix
        fw "gbs"
        print $ strictBSToString gbs 
        let json = toStrictBS $ DA.encode $ GeneMatrix{cmd = "mycmd", ncol=3, nrow=4}
        fw "str"
        S8.putStrLn str
        fw "response gbs"
        response $ responseNothingBS gbs 

receiveCode::Application
receiveCode req response = do 
        str <- requestBody req
        let may = DA.decode $ LA.fromStrict str :: Maybe CompileCode 
        fw "may"
        print may
        let ccode = case may of 
                       (Just x) -> x 
                       _       -> CompileCode{compiler = "", option = "", code= ""} 
        fw "cool" 
        fw "cool" 
        let firstLine = head $ lines $ strictTextToStr (code ccode)
        let lang = last $ splitStr "[[:space:]]+" firstLine
        if | lang == "cpp" -> TIO.writeFile "/tmp/code.cpp" (code ccode)
           | lang == "haskell" -> TIO.writeFile "/tmp/code.hs" (code ccode)
           | otherwise -> return () 
        pp lang 
        let cmd = if | lang == "cpp" -> "g++ -o out /tmp/code.cpp && ./out"
                     | lang == "haskell" -> "runh2 /tmp/code.hs && /tmp/code"
                     | otherwise -> ""
        (e2, so, si2) <- runSh $ strToStrictText (cmd)
        if e2 /= ExitSuccess then let rcode = ReplyCode{rcmd="", rerror = si2, stdout=si2} 
                                      replyJson = toStrictBS $ DA.encode $ rcode 
                                  in response $ responseNothingBS replyJson 
        else do
            pp so     
            let replyCode = ReplyCode{rcmd="", rerror="", stdout= so} 
            let replyJson = toStrictBS $ DA.encode $ replyCode 
            response $ responseNothingBS replyJson

receiveCode2::Application                                                                            
receiveCode2 req response = do                                                                       
        str <- requestBody req                                                                      
        let may = DA.decode $ LA.fromStrict str :: Maybe CompileCode                                
        fw "may"                                                                                    
        print may                                                                                   
        let ccode = case may of                                                                     
                       (Just x) -> x                                                                
                       _       -> CompileCode{compiler = "", option = "", code= ""}                 
        fw "cool"                                                                                   
        fw "cool"                                                                                   
        let firstLine = head $ lines $ strictTextToStr (code ccode)                                 
        let lang = last $ splitStr "[[:space:]]+" firstLine                                         
        if | lang == "cpp" -> TIO.writeFile "./cppcode.cpp" (code ccode)                            
           | lang == "haskell" -> TIO.writeFile "./code.hs" (code ccode)                         
           | otherwise -> return ()                                                                 
        pp lang                                                                                     
        let cmd = if | lang == "cpp" -> "g++ -o cppout ./cppcode.cpp && ./cppout"                         
                     | lang == "haskell" -> "runh2 ./code.hs && /tmp/code"                       
                     | otherwise -> ""                                                              
        sout <- A.run cmd                                              
        let rcode = ReplyCode{rcmd="", rerror = "", stdout= (strToStrictText $ head sout)}          
            replyJson = toStrictBS $ DA.encode $ rcode                    
        response $ responseNothingBS replyJson                         


responseEditor:: Response
responseEditor = responseFile
    status200
    [("Content-Type", "text/html")]
    "compileCode.html"
    Nothing

responseJavascript::FilePath -> Response
responseJavascript fname = responseFile
  status200
  [(hContentType, "text/javascript")]
  fname
  Nothing

responseHtml::FilePath -> Response
responseHtml fname = responseFile
  status200
  [(hContentType, "text/html")]
  fname
  Nothing

{-| 
    @
    <form action="/upload" method="POST" enctype="multipart/form-data">
    Upload File: <input type="file" name="file"><br>
    <input type="submit" value="submit">
    </form> 
    @

    * Handle file uploads, storing the file in the current directory
    * upload :: Application
-} 
updateMap::IORef HMap -> Application
updateMap ref req response = do
    -- Parse the request body. We'll ignore parameters and just look
    -- at the files
    (params, files) <- parseRequestBody lbsBackEnd req
    str <- requestBody req
    case requestMethod req of
        "POST" -> do 
              case lookup "myblock" params of 
                   Just code  -> do 
                      -- read snippet and update the new block
                      home <- getEnv "HOME"
                      pplist <- readSnippet (home </> snippetP) 
                      let block = lines $ b2s code
                      LA.writeFile "/tmp/b1.x" (sToL code)
                      case lookup "header" params of 
                       Just headCode -> do 
                          -- headCode: the first line of code block.
                          -- read snippet and update the new block
                          -- filter out the edited block based on the header which is hidden in TextArea from client side
                          -- replace '\r\n' with '\n' using Text.PortableLines.lines and unlines
                          -- concat the new block to new pplist
                          pplist <- readSnippet (home </> snippetP) 
                        
                          let isAdded = isJust   $ lookup "add" params
                          let isUpdated = isJust $ lookup "update" params
                          let isDeleted = isJust $ lookup "delete" params

                          writeToFileAppend "/tmp/db.x" ["isUpdated=" ++ (show isUpdated), 
                                                         "isAdded=" ++   (show isAdded), 
                                                         "isDeleted=" ++ (show isDeleted)]
                          let cb = map (\x -> BU.fromString <$> snd x) $ filter(\(_, b) ->
                                     if | isAdded -> True    
                                        | isUpdated -> trimBothLBS (s2b $ head b) /= trimBothLBS (sToL headCode)
                                        | isDeleted -> trimBothLBS (s2b $ head b) /= trimBothLBS (sToL headCode)
                                        | otherwise -> trimBothLBS (s2b $ head b) /= trimBothLBS (sToL headCode)) pplist -- [[ByteString]]
                          pp $ typeOf cb -- [[ByteString]]
                          LA.writeFile "/tmp/h1.x" $ sToL headCode
                          -- modifiedCode is the modified block from client
                          let modifiedCode = if isDeleted then "" else
                                unlines $ map(\x -> if len (trimBoth x) > 0 then trimEnd x else x) $ plines $ b2s code
                          let newSnippet =  home </> "myfile/bitbucket/snippets/snippet_new.hs"
                          let mvSnippet  =  home </> "myfile/bitbucket/snippets/snippet_mv.hs"
                          -- listBlock is all the blocks excluding a modified block
                          let listBlock = BS.concat $ map(\x -> BS.cons (BI.c2w '\n') $ BS.concat $ map (BS.cons (BI.c2w '\n')) x) cb 
                          -- write listblock and modified to newSnippet 
                          LA.writeFile newSnippet $ sToL $ BS.concat [listBlock, BS.cons (BI.c2w '\n') $ BS.cons (BI.c2w '\n') $ (toStrictBS . s2b) modifiedCode]
                          -- it does not work here, got some "Terminated 15 error", not sure why 
                          (e2, so, si2) <- runSh $ strToStrictText ("mv " ++ newSnippet ++ " " ++ (home </> snippetP))
                          if e2 /= ExitSuccess then error (strictTextToStr si2) 
                            else do
                            pplist <- readSnippet (home </> snippetP) 
                            -- read the map out from ref
                            -- conver all the keys to list of keys
                            -- empty the map (ref HMap) 
                            -- rehash the map
                            hmap <- readIORef ref 
                            let keys = M.keys hmap
                            modifyIORef ref (mapClear keys)
                            snippetMap pplist ref
                          pp "dog"
                      response =<< let Just uri = parseURI (WC.hostURL ++ "/snippet?id=s%20va") in redirect' status302 [] uri 
                      -- response =<< let Just uri = parseURI "http://localhost:8000/snippet?id=s%20va" in redirect' status302 [] uri 
        _   -> do 
               LA.writeFile "/tmp/b2.x" "no post" 
               response $ responseNothing "b2.x"
               response $ responseNothing "b2.x"
    where
        sToL = strictByteStringToLazyByteString
        -- Strict ByteString to String
        b2s = strictBSToString -- strictTextToStr . strictByteStringToStrictText
        s2b = strToLazyByteString
--    case lookup "id" params of
--        Nothing -> undefined
--        Just x -> undefined

-- http://localhost:8000/up/
-- | NOTE: file => /upload dir
-- | Plz see uploadPage.html 
replyEditor :: Response
replyEditor = responseFile
    status200
    [("Content-Type", "text/html")]
    "indexEditor.html"
    Nothing

responseHelp :: Response
responseHelp = responseFile
    status200
    [("Content-Type", "text/html")]
    "help.html"
    Nothing

replyCssButton :: Response
replyCssButton = responseFile
    status200
    [("Content-Type", "text/html")]
    "cssButton.html"
    Nothing

wordcountReply :: Response
wordcountReply = responseFile
    status200
    [("Content-Type", "text/html")]
    "wordcountReply.html"
    Nothing

matrixReply :: Response
matrixReply = responseFile
    status200
    [("Content-Type", "text/html")]
    "postMatrix.html"
    Nothing

replyJS :: Response
replyJS = responseFile
    status200
    [("Content-Type", "text/javascript")]
    "ace/build/src/ace.js"
    Nothing


{-| 
    === Insert name and age to MySqlite-simple file-based database.

    http://localhost:8000/insert/

    File: insert.html
    <form action="/insert" method="POST" enctype="multipart/form-data">
      Name <input type="text" name="name"><br>
      Age <input type="text" name="age"><br>
      <input type="submit" value="submit">
    </form> 

    >insert data to table: people
    >"INSERT INTO people (name, age) VALUES (?,?)" 
-} 
insertDatabase::Connection -> Application
insertDatabase conn req response = do
    (params, files) <- parseRequestBody lbsBackEnd req
    case requestMethod req of
        "POST" -> do 
              let name = case lookup "name" params of 
                                Just name -> name 
                                _         -> "name nothing"
              let age = case lookup "age" params of 
                                Just age  -> age 
                                _         -> "age nothing" 


              execute_ conn "CREATE TABLE IF NOT EXISTS people (id INTEGER PRIMARY KEY AUTOINCREMENT, name TEXT, age TEXT)"
              execute conn "INSERT INTO people (name, age) VALUES (?,?)" (Person 0 (s2t name) (s2t age))
              people <- query_ conn "SELECT id, name, age from people" :: IO [Person]
              print people
              response =<< let Just uri = parseURI "http://localhost:8000/insertinfo/" in redirect' status302 [] uri 
              -- response $ responseNothing $ b2s $ BS.concat [name, age]
        _      -> response $ responseNothing "post nothing"

    where 
        b2s = strictTextToStr . strictByteStringToStrictText
        s2t = strictByteStringToStrictText

loginCheck::Connection -> Application
loginCheck conn req response = do
    (params, files) <- parseRequestBody lbsBackEnd req
    case requestMethod req of
        "POST" -> do 
              let email_ = case lookup "email" params of 
                                Just email -> s2t email 
                                _          -> "email nothing" 

              let password_ = case lookup "password" params of 
                                -- Just password -> BS.takeWhile (not.DW.isSpace) $ BS.dropWhile (DW.isSpace) password 
                                Just password -> s2t password 
                                _             -> "password nothing" 
              print email_
              print password_ 
              -- row <- queryNamed conn "SELECT * FROM user WHERE uid = :uid" [":uid" := uid] :: IO [User]
              row <- queryNamed conn "SELECT * FROM user WHERE email = :email AND password = :password" [":password" := password_, ":email" := email_] :: IO [User]
              print row
              response $ responseNothing "row nothing"
        _      -> response $ responseNothing "user nothing"

    where 
        b2i = stringToInt . strictTextToStr . s2t
        b2s = strictTextToStr . strictByteStringToStrictText
        s2t = strictByteStringToStrictText
        t2b = strictTextToStrictByteString


{-| 
    validate user input and santize me
-} 
securityValidate:: BS.ByteString -> 
                   BS.ByteString -> 
                   BS.ByteString -> 
                   BS.ByteString -> 
                   BS.ByteString -> 
                   Bool
securityValidate name email password task money = nameBool && passwordBool && emailBool
        where
            nameT = s2t name
            passwordT = s2t password
            nameBool = if (TS.length nameT) > 0 && (TS.length nameT) < 40 && TS.all (isAlphaNum) nameT then True else False
            passwordBool = if TS.all (isAlphaNum) passwordT then True else False
            emailBool = EM.isValid email
            --
            b2i = stringToInt . strictTextToStr . s2t
            b2s = strictTextToStr . strictByteStringToStrictText
            s2t = strictByteStringToStrictText
            t2b = strictTextToStrictByteString

insertUserDB::Connection -> Application
insertUserDB conn req response = do
    (params, files) <- parseRequestBody lbsBackEnd req
    case requestMethod req of
        "POST" -> do 
              let name_ = case lookup "name" params of    -- ByteString
                                Just name -> name 
                                _         -> "name nothing"
              let email_ = case lookup "email" params of 
                                Just email -> email 
                                _          -> "email nothing" 

              let password_ = case lookup "password" params of 
                                Just password -> password 
                                _             -> "password nothing" 

              let task_ = case lookup "task" params of 
                                Just task -> task 
                                _         -> "task nothing" 

              let money_ = case lookup "money" params of 
                                Just money -> money 
                                _         -> "money nothing" 

              -- validate user input
              -- formatValidate::User -> Bool
              execute_ conn "CREATE TABLE IF NOT EXISTS user (uid INTEGER PRIMARY KEY AUTOINCREMENT, name TEXT, email TEXT, password TEXT, task TEXT, money INTEGER)"
              row <- queryNamed conn "SELECT * FROM user WHERE email = :email" [":email" := (s2t email_)] :: IO [User]
              if len row == 0 then do
                  execute conn "INSERT INTO user (name, email, password, task, money) VALUES (?,?,?,?,?)" (User 0 (s2t name_) (s2t email_) (s2t password_) (s2t task_) (b2i money_))
                  changeRow <- changes conn
                  print $ "changeRow=" ++ (show changeRow)
                  if changeRow == 1 then do
                      userList <- query_ conn "SELECT uid, name, email, password, task, money FROM user" :: IO [User]
                      mapM_ print userList
                      -- let listTask = TS.concat $ map (\x -> [r|<div>|] <> (t2b $ task x) <> [r|</div><br>|]) userList -- => TS.Text
                      -- response $ responseTaskBS (task (head userList))
                      response $ responseTaskBS $ replyTaskHtml task_ 
                  else do
                      response $ responseTaskBS "Insect task field" 
                      -- response =<< let Just uri = parseURI "http://localhost:8000/insertUser/" in redirect' status302 [] uri 
              else do
                  response $ responseNothing "email exists"
        _      -> response $ responseNothing "no POST"

    where 
        b2i = stringToInt . strictTextToStr . s2t
        b2s = strictTextToStr . strictByteStringToStrictText
        s2t = strictByteStringToStrictText
        t2b = strictTextToStrictByteString

-- | -------------------------------------------------------------------------------- 
-- | Wed Dec  5 15:06:00 2018 
--   Sat Jun  8 23:42:18 2019 
-- | upload with following POST to upload file to server
-- | -------------------------------------------------------------------------------- 
-- <form action="/upload" method="POST" enctype="multipart/form-data">
--  Upload File: <input type="file" name="file"><br>
--  <input type="submit" value="submit">
-- </form> 
-- | -------------------------------------------------------------------------------- 
--   http://localhost:8000/up/
-- | File is uploaded to => haskell_web/uploaddir 
upload::String -> Application
upload updir req response = do
    -- Parse the request body. We'll ignore parameters and just look
    -- at the files
    (params, files) <- parseRequestBody lbsBackEnd req
--    case lookup "id" params of
--        Nothing -> undefined
--        Just x -> undefined

    -- Look for the file parameter called "file"
    case lookup "file" files of
        -- Not found, so return a 400 response
        Nothing -> response $ responseLBS
            status400
            [("Content-Type", "text/plain; charset=utf-8")]
            "No file parameter found"
        -- Got the file 
        -- take the file name
        -- grab the content
        -- write the file to filesystem
        Just file -> do
            let
                -- Determine the name of the file to write out
                name = takeFileName $ S8.unpack $ fileName file
                -- and grab the content
                content = fileContent file
            -- Write it out
            writeToFile "/tmp/f1.x" [name]
            LA.writeFile (updir ++ name) content
            response $ replyCssButton 

searchMap:: Application
searchMap req response = do
    -- Parse the request body. We'll ignore parameters and just look
    -- at the files
    (_params, files) <- parseRequestBody lbsBackEnd req

    -- Look for the file parameter called "file"
    case lookup "post" _params of
        -- Not found, so return a 400 response
        Nothing -> response $ responseLBS
            status400
            [("Content-Type", "text/plain; charset=utf-8")]
            "No post"
        -- Got it!
        Just "post" -> response $ responseLBS
            status200
            [("Content-Type", "text/text")]
            "Just post" 

type ClientId = Int
type Client   = (ClientId, WS.Connection)
type State    = [Client]

connectClient :: WS.Connection -> Concurrent.MVar State -> IO ClientId
connectClient conn stateRef = Concurrent.modifyMVar stateRef $ \state -> do
  let clientId = nextId state
  return ((clientId, conn) : state, clientId)

withoutClient :: ClientId -> State -> State
withoutClient clientId = L.filter ((/=) clientId . fst)

disconnectClient :: ClientId -> Concurrent.MVar State -> IO ()
disconnectClient clientId stateRef = Concurrent.modifyMVar_ stateRef $ \state ->
  return $ withoutClient clientId state

nextId :: State -> ClientId
nextId = maybe 0 ((+) 1) . Safe.maximumMay . L.map fst


httpApp :: Application
httpApp _ respond = respond $ responseLBS status400 [] "Not a websocket request"

listen :: WS.Connection -> ClientId -> Concurrent.MVar State -> IO ()
listen conn clientId stateRef = forever $ do
  WS.receiveData conn >>= broadcast clientId stateRef

broadcast :: ClientId -> Concurrent.MVar State -> Text -> IO ()
broadcast clientId stateRef msg = do
  clients <- Concurrent.readMVar stateRef
  let otherClients = withoutClient clientId clients
  forM_ otherClients $ \(_, conn) ->
    WS.sendTextData conn msg

wsApp :: Concurrent.MVar State -> WS.ServerApp
wsApp stateRef pendingConn = do
  conn <- WS.acceptRequest pendingConn
  clientId <- connectClient conn stateRef
  WS.forkPingThread conn 30
  Exception.finally
    (listen conn clientId stateRef)
    (disconnectClient clientId stateRef)
