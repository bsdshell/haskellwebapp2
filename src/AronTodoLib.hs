{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
{-# LANGUAGE QuasiQuotes #-} -- support raw string [r|<p>dog</p> |]

module AronTodoLib where

import Control.Monad
import Data.Char
import qualified Data.List as L
import Data.List.Split
import Data.Time
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
import Data.IORef 
import Control.Monad (unless, when)
import qualified Data.Aeson      as DA
import qualified GHC.Generics    as GEN
import qualified Data.Text       as TS  -- Strict Text
import Control.Concurrent 
-- {-# LANGUAGE QuasiQuotes       #-}
import Text.RawString.QQ (r)         -- Need QuasiQuotes too 


test44::Int -> Int
test44 x = x + 3

