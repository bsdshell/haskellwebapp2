-- {-# LANGUAGE QuasiQuotes       #-}
-- {-# LANGUAGE MultiWayIf #-}

module Main where

import PortableLines 

import Data.Typeable (typeOf)
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

import Language.Haskell.Ghcid

import qualified Data.Text.Lazy                 as DL 

import qualified Control.Concurrent             as Concurrent
import qualified Data.List as L
import qualified Data.HashMap.Strict            as M 
import qualified Control.Exception              as Exception
import qualified Safe

import qualified Data.ByteString.UTF8 as BU
import qualified Data.ByteString.Lazy.Internal as IN (ByteString)
import qualified Data.ByteString.Char8      as S8 (unpack,pack, putStrLn)   -- strict ?
import qualified Data.ByteString.Lazy       as LA (writeFile, fromChunks, fromStrict)
import qualified Data.ByteString.Lazy.Char8 as LC 
import qualified Data.ByteString            as BS
import qualified Data.ByteString.Internal   as BI (c2w, w2c)

import AronModule                hiding(run, cmd)
import WaiLib 
import qualified AronModule                 as A

import qualified Turtle as TUR -- (empty, shellStrictWithErr, ExitCode)
import Data.Text (Text)
import qualified Data.Text as DT 

import Network.HTTP.Types (status200)
import Network.Wai
import Network.Wai.Handler.Warp (run)
import Network.Wai.Util
import Network.Wai.Session (withSession)
import Network.URI
import Network.HTTP.Types.Status


import qualified Network.Wai.Handler.WebSockets as WS
import qualified Network.WebSockets             as WS
import qualified WaiConstant                    as WC 

import Text.RawString.QQ

import           Data.Int (Int64)
import           Database.SQLite.Simple
import           Database.SQLite.Simple.FromRow
import           Database.SQLite.Simple.FromField
import           Database.SQLite.Simple.ToField
import           Database.SQLite.Simple.Internal
import           Database.SQLite.Simple.Ok

import GHC.Generics
import qualified Data.Aeson as DA

-- from config.txt
-- OS -> darwin
--       darwin -> [("", ""), ("", "") ]
--       
config =[ [("os", "darwin")],
            [
              ("testdb",          "myfile/bitbucket/testfile/test.db"),
              ("webappdb",        "myfile/bitbucket/testfile/haskellwebapp2.db"),
              ("host",            "http://localhost"),
              ("snippetpath",     "myfile/bitbucket/snippets/snippet.hs"),
              ("port",            "8000"),
              ("readSnippetFile", "False")
            ]
        ]
-- [("a", "b") [("a", "b"), ("c", "d")]]
         
-- NOTE: the project does not use the shared Haskell lib in $b/haskelllib
-- FIX it ASAP
dbname = "webappdb"
configFile = "./config.txt"

lookupJust s m = fromJust $ M.lookup s m


                 
main :: IO ()
main = do
    home <- getEnv "HOME"
    configMap <- readConfig configFile 
    os <- getOS
    pre os
    let userinputdb = lookupJust dbname $ lookupJust os configMap
    pp userinputdb
    let host = lookupJust "host" $ lookupJust os configMap
    let snippet = lookupJust "snippetpath" $ lookupJust os configMap
    let portStr = lookupJust "port" $ lookupJust os configMap
    let useSnippet = lookupJust "readSnippetFile" $ lookupJust os configMap

    let port = read portStr :: Int 
    conn <- open $ home </> userinputdb 
    createCodeBlockTable conn
    pplist <- readSnippet (home </> snippet) 

    -- See src/config.txt 
    -- Whether to read $b/snippets/snippt.hs file to database or not
    when (read useSnippet :: Bool) $ readSnippetToDatabase (home </> snippet) conn

    pdfRef <- newIORef M.empty
    ref <- newIORef M.empty 
    -- newList <- readDatabaseCodeBlock2 conn
    newList <- readDatabaseCodeBlock3 conn 
    pre newList
    -- snippetMap pplist ref
    -- snippetMap newList ref
    listToPrefixMap newList ref
    hmap <- readIORef ref

    lstup <- resourceList
    logFile2 "/tmp/x.x" $ map show lstup
    let rmap = M.fromList lstup
    
    fw "hmap"

    let f::Stream -> String -> IO()
        f a b = return ()

    -- (g, _) <- startGhciProcess (shell "ghci_stack.sh") f
    -- pre hmap 
    fw "main.hs newList end"
    putStrLn host 
    pp "http starting"
    pp "test it"
    pp WC.hostURL 
    pp $ "NOTE port => " ++ show port
        
                       
    -- run WC.port (app conn ref)
    run port (app2 undefined conn ref pdfRef rmap)
    close conn
    
