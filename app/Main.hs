{-# LANGUAGE OverloadedStrings #-}
module Main where

import Undercover
import Network.IRC.Client
import Control.Lens
import qualified Data.Text as T

main :: IO ()
main = do
    database <- fmap T.words . T.lines . T.pack <$> readFile "words.db"
    let conn = plainConnection "irc.epiknet.org" 6667 & logfunc .~ stdoutLogger
        cfg = defaultInstanceConfig "Theodorine-Genevieve" & handlers %~ (undercoverHandler:)
        
    runClient conn (cfg & channels .~ [gamechan]) (Pending [] database)
