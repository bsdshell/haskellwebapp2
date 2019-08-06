{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE MultiWayIf #-}

module Main where

import PortableLines 
import Text.RawString.QQ

import Data.Typeable (typeOf)
import Network.Wai
import Network.HTTP.Types
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



import qualified Data.Text.Lazy                 as DL 

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


dbname = "webappdb"
configFile = "./config.txt"

lookupJust s m = fromJust $ M.lookup s m

main :: IO ()
main = do
    home <- getEnv "HOME"
    configMap <- readConfig configFile 
    os <- getOS
    let userinputdb = lookupJust dbname $ lookupJust os configMap
    let host = lookupJust "host" $ lookupJust os configMap
    let snippet = lookupJust "snippetpath" $ lookupJust os configMap

    conn1 <- open $ home </> userinputdb 
    pplist <- readSnippet (home </> snippet) 
    ref <- newIORef M.empty 
    snippetMap pplist ref
    pp "dog"
    putStrLn host 
    pp "http starting"
    run 8000 (app conn1 ref)
    close conn1
