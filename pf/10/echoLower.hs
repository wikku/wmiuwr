module EchoLower where

import Data.Char

echoLower :: IO ()
echoLower = putStr . map toLower =<< getContents

main = echoLower
