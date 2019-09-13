-- {{{ begin_fold
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE QuasiQuotes       #-} -- RawString QQ 

module GenePDFHtmlLib where

-- script
-- #!/usr/bin/env runhaskell -i/Users/cat/myfile/bitbucket/haskelllib
-- {-# LANGUAGE OverloadedStrings #-}
-- {-# LANGUAGE DuplicateRecordFields #-} 
-- import Turtle
-- echo "turtle"


-- import Data.Set   -- collide with Data.List 
import Control.Monad
import Data.Char
import Data.Default
import Data.Typeable (typeOf) -- runtime type checker, typeOf "k"
import qualified Data.List as L
import Data.List.Split
import Data.Time
import Data.Time.Clock.POSIX
import qualified Data.ByteString as BS
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
import Data.IORef 
import Control.Monad (unless, when)
import Control.Concurrent 

import           Data.Int (Int64)
import           Data.Text (Text)
import qualified Data.Text                 as TS
import           Database.SQLite.Simple
import           Database.SQLite.Simple.FromRow
import           Database.SQLite.Simple.FromField
import           Database.SQLite.Simple.ToField
import           Database.SQLite.Simple.Internal
import           Database.SQLite.Simple.Ok
import qualified Text.Regex.TDFA as TD

-- {-# LANGUAGE QuasiQuotes       #-} -- RawString QQ, Quasi 
import Text.RawString.QQ          -- QuasiQuotes needs for Text.RawString.QQ 

import AronModule 
import AronHtml

--import Data.Array

-- import Graphics.Rendering.OpenGL as GL 
-- import Graphics.Rendering.OpenGL.GLU.Matrix as GM  
-- import qualified Graphics.UI.GLFW as G
-- import Data.Set(Set) 
-- import qualified Data.Set as S 

--if (length argList) == 2 
--then case head argList of 
--    "svg" -> run cmd >> run "ls" >>= \x -> pp x 
--            where 
--                cmd = "pwd" 
--    "png" ->run cmd >> run ("ls " ++ fn) >>= \x -> pp x  
--            where 
--                cmd = "pwd" 
--    _     -> print "more arg" 
--else print "Need more arguments" 

--    takeFileName gives "file.ext"
--    takeDirectory gives "/directory"
--    takeExtension gives ".ext"
--    dropExtension gives "/directory/file"
--    takeBaseName gives "file"
--    "/directory" </> "file.ext".
--    "/directory/file" <.> "ext".
--    "/directory/file.txt" -<.> "ext".
-- |  end_fold ,}}}


-- zo - open
-- za - close


data LHead = LHead {xtitle::String, desc::String} deriving(Show, Eq)

instance Default LHead where
   def = LHead{xtitle=""::String, desc=""::String}

data PDFInfo = PDFInfo 
    { pdfId :: Int64
    , title :: Text
    , pdesc :: Text
    , path  :: Text
    } deriving (Eq,Read,Show)



instance FromRow PDFInfo where
  fromRow = PDFInfo <$> field <*> field <*> field <*> field

instance ToRow PDFInfo where
  toRow (PDFInfo _pId title pdesc path) = toRow (title, pdesc, path)

data SqliteMaster = SqliteMaster {
        x_type::Text,
        x_name::Text,
        x_tbl_name::Text,
        x_rootpage::Integer,
        x_sql::Text
      } deriving(Eq, Read, Show)
instance FromRow SqliteMaster where
  fromRow = SqliteMaster <$> field <*> field <*> field <*> field <*> field

instance ToRow SqliteMaster where
  toRow (SqliteMaster x_type x_name x_tbl_name x_rootpage x_sql) = toRow (x_type, x_name, x_tbl_name, x_rootpage, x_sql)


-- test.tex
-- read the first two lines from Latex file
-- % title : nice title
-- % desc : nice description
-------------------------------------------------------------------------------
pdfFile = "/Library/WebServer/Documents/zsurface/pdf"        
dbpath = "myfile/bitbucket/database/sqlite3_pdfhtml.db"

{-| 
    === parse first two lines inside .tex file

    * If title and desc are found, return an titleDesc with the title and desc
    * Otherwise return an default titleDesc which is empty title and empty desc.
    
    * Example inside latex file.
    @
    % title : This is an awesome.
    % desc  : The document is doing some fantastic thing. 
    @
-} 
titleDesc::[String] ->  LHead 
titleDesc [] = def LHead
titleDesc cx = he 
        where
         ls = map (splitStrChar "[:]") cx  -- " title : awesome doc " => ["title", "awesome doc"]
         t = if (containStr "title" ((head.head) ls)) then (last.head) ls else xtitle (def LHead) 
         d = if (containStr "desc" ((head.last) ls))  then (last.last) ls else desc  (def LHead) 
         he = LHead{xtitle = trim t, desc = trim d}

hempty = LHead {xtitle="", desc=""}
_PDFTABLE = "pdftable"


td_ f d = [r|<td id="notme"><a href="|] <> f <> [r|">|] <> d <> [r|</a></td> |]

toT = strToStrictText
toS = strictTextToStr 

--baseNameT s = if (length $ toS s) <= maxlen then toT $ baseName $ toS s else toT $ baseName $ take maxlen (toS s)
--    where
--        toS = strictTextToStr 
--        maxlen = 30

styleT cs    =  " style='" <> (TS.concat cs) <> "' "

ahrefT::TS.Text -> TS.Text -> TS.Text
ahrefT link desc = [r| <a href='|] <> link <> [r|'>|] <> desc <> [r|</a>|]

tdTT::TS.Text -> TS.Text -> TS.Text
tdTT att a = [r|<td |] <> att <> [r|>|] <> a <> [r|</td>|]

trCT s        =  [r|<tr>|] <> s <> [r|</tr>|]


tableTT s     =  [r|<table |] <> s <> [r|>|] 
tableACT a s  =  (tableTT a) <> s <> [r|</table>|]

tableA::[[[TS.Text]]] -> TS.Text 
tableA cs = rc 
    where
        rc = tableACT (styleT st) 
                     (TS.concat $ map(\r -> trCT $ TS.concat $ map(\x -> let h = baseNameT $ x ! 2 
                                                                             p = x ! 2
                                                                         in  tdTT att (ahrefT p h)
                                                                 ) r) cs) 
        st = ["font-size:18px;", "background:#DDDDDD;"]::[TS.Text]
        att = [r| id='notme' |] :: TS.Text
        (!) = (!!)

st = ["font-size:18px;", "background:#DDDDDD;"]::[TS.Text]
tableAA::[[[TS.Text]]] -> [TS.Text] -> TS.Text 
tableAA cs st = rc 
    where
        rc = tableACT (styleT st) 
                     (TS.concat $ map(\r -> trCT $ TS.concat $ map(\x -> let h = baseNameT $ x ! 2 
                                                                             p = x ! 2
                                                                         in  tdTT att (ahrefT p h)
                                                                 ) r) cs) 
        att = [r| id='notme' |] :: TS.Text
        (!) = (!!)



{-| 
    === Insert tex file 'PDFInfo' to database

    * read file with readFileLatin1ToList
    * read the first two lines from a tex file
    * create 'PDFInfo' record
    * insert into database
    * 
    * texPath = home </> "myfile/bitbucket/math"                              
-} 
insertTexToTable::Connection -> [String] -> IO() 
insertTexToTable conn allTexFile = do
        -- read the first two lines from a tex file
        -- allheads = [(first two lines, full path tex file)]
        allheads <- mapM(\x -> do 
                                ls <- readFileLatin1ToList x
                                let t = take 2 ls
                                return (t, x)
                                ) allTexFile 
        let texfile = filter (\x -> (titleDesc $ fst x) /= hempty) allheads 
        mapM_(\x -> let title = toT $ (head.fst) x
                        pdesc = toT $ (last.fst) x
                        path  = toT $ snd x
                    in execute conn ([r| INSERT INTO |] <> _PDFTABLE <> [r| (title, pdesc, path) VALUES (?,?,?) |]) (PDFInfo 0 title pdesc path)
                       ) texfile
        print "add tex done." 


{-| 
    === Create new table '_PDFTABLE' 
-} 
dropTable::Connection -> Query -> IO()
dropTable conn fname = do
        -- let tableQuery = [r| DROP TABLE |] <> _PDFTABLE
        let tableQuery = Query ([r| DROP TABLE |] <> (fromQuery fname)) 
        execute_ conn tableQuery 
        -- print $ "drop table:" <> _PDFTABLE



{-| 
    === Check whether a talbe exist 

    * sqlite_master schema
    @
    CREATE TABLE sqlite_master(
      type text,
      name text,
      tbl_name text,
      rootpage integer,
      sql text
    );
    @
-}
checkTable::Connection -> Query -> IO Bool
checkTable conn tableName = do 
        -- let query = Query([r| SELECT * FROM sqlite_master WHERE type='table' AND name='pdftable' |])  
        let query = Query([r| SELECT * FROM sqlite_master WHERE type='table' AND name='|] <> (fromQuery tableName) <>[r|'|])  
        listTab <- query_ conn query :: IO [SqliteMaster]
        print listTab
        return $ len listTab > 0
    

{-| 
    === Create new table '_PDFTABLE' 
-} 
createNewTable::Connection -> Query -> IO()
createNewTable conn query = do
        let tableQuery = Query ([r| CREATE TABLE IF NOT EXISTS |] <> (fromQuery query) <> [r| (id INTEGER PRIMARY KEY AUTOINCREMENT, title TEXT, pdesc TEXT, path TEXT) |])
        execute_ conn tableQuery 
        print "done" 
 
{-| 
    === Insert PDF file to database
-} 
insertPDFToTable::Connection -> [String] -> IO() 
insertPDFToTable conn allPDFfile = do
        -- read the first two lines from a tex file
        -- allheads = [(first two lines, full path tex file)]
        -- conn <- open dbstr 
        mapM_(\x -> let title = "" 
                        pdesc  = "" 
                        path  = toT x
                        -- path  = toT x 
                    in execute conn ([r| INSERT INTO |] <> _PDFTABLE <> [r| (title, pdesc, path) VALUES (?,?,?) |]) (PDFInfo 0 title pdesc path)
                       ) allPDFfile 
        print "done" 



queryDBToHtml::Connection -> Query -> String -> IO BS.ByteString
queryDBToHtml conn tableName dir = do
        pdfQuery <- query_ conn ([r| SELECT id, title, pdesc, path from |] <> tableName) :: IO [PDFInfo]
        let plist = partList 4 pdfQuery -- [[PDFInfo]]
        let pdfLL = map (\r -> map(\c -> let t = if (TS.length $ trimT $ title c) == 0 then toT $ "no title" else trimT $ title c
                                             d = if (TS.length $ trimT $ pdesc c) == 0 then baseNameT $ path c else pdesc c
                                             p = (toST dir) <> takeFileNameT (path c) 
                                         in [t, d, p] 
                                   ) r) plist
        let tablea = tableAA pdfLL st
        curr <- getPwd
        print curr
        print pdfLL
        writeFileBS "genepdf.html" (stToBS tablea)
        return $ stToBS tablea
    where
        toBS = strToStrictByteString
        toST = strToStrictText
        stToBS = strictTextToStrictByteString

pdfMain::Connection -> String -> IO BS.ByteString
pdfMain conn p = do
        home <- getEnv "HOME"
        let texFilePath = home </> "myfile/bitbucket/math"                              
        let pdfFilePath = "./pdf"                              
        
        allTexFile <- lsRegexFull texFilePath "\\.tex$"
        allPDFFile <- lsRegexFull texFilePath "\\.pdf$"
        downPDFFile <- lsRegexFull pdfFilePath "\\.pdf$"
        let utexFile = unique allTexFile
        let updfFile = unique (allPDFFile ++ downPDFFile)
        -- dropTable conn _PDFTABLE 
        foundTab <- checkTable conn _PDFTABLE 
        createNewTable conn _PDFTABLE 
        insertTexToTable conn utexFile 
        insertPDFToTable conn updfFile 
        queryDBToHtml conn _PDFTABLE "pdf/"


-- main = do
--     bs <- pdfMain
--     print bs
