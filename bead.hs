{-# LANGUAGE OverloadedStrings #-}
import Control.Concurrent
import Control.Parallel.Strategies
import Data.Char (isSpace)
import Data.Foldable (toList)
import Data.Sequence (Seq)
import Data.String (fromString)
import Data.Text (Text)
import Network
import System.IO
import Test.QuickCheck
--import Data.Text.IO (hGetLine)

import qualified Data.Sequence as Seq
import qualified Data.Text     as Text
import qualified Data.Text.IO  as Text

-----------------------------------------------------
hGetLines :: Handle -> IO [Text]
hGetLines hndl = do 
  line <- Text.hGetLine hndl
  if (Text.length line > 0)
    then do 
      rest <- hGetLines hndl 
      return ([line] ++ rest)
    else return ([])

-----------------------------------------------------
acceptFork :: Socket -> (Handle -> IO ()) -> IO ()
acceptFork socket action = do
  (hndl, b, c) <- accept socket
  hSetBuffering hndl NoBuffering
  hSetNewlineMode hndl $ NewlineMode CRLF CRLF 
  forkIO (action hndl)
  return ()
  --TODO call recursively acceptFork 

-----------------------------------------------------
response :: Text -> Text 
response text = Text.append 
    (Text.append 
    "HTTP/1.0 200 OK\nConnection: close\nContent-Type: text/plain\n\n"
    text )"\n" 
  
test_response =
  [ response "" ==
    "HTTP/1.0 200 OK\nConnection: close\nContent-Type: text/plain\n\n\n"
  , response "alma" ==
    "HTTP/1.0 200 OK\nConnection: close\nContent-Type: text/plain\n\nalma\n"
  , response "al\nma" ==
    "HTTP/1.0 200 OK\nConnection: close\nContent-Type: text/plain\n\nal\nma\n"
  ]


-----------------------------------------------------
handleClient :: Handle -> IO ()
handleClient hndl = do
  s <- hGetLines hndl
  Text.hPutStr hndl $ response "szia"
  hFlush hndl
  hClose hndl
  return ()

-----------------------------------------------------
main :: IO () 
main = do
  socket <- listenOn (PortNumber 8000)
  tid <- forkIO ( acceptFork socket handleClient)
  line <- getLine
  killThread tid
  sClose socket
  return ()

-----------------------------------------------------
{-
requestedResource (x:xs) = do
  let splitted = Text.words x
  if (length splitted > 2 && Text.toUpper(splitted!!0) == "GET")
  then
    return (splitted!!1)
  else
    return Nothing 
-}
   
requestedResource :: [Text] -> Maybe Text
requestedResource (x:xs) = reqRes $ Text.words x

reqRes (x:y:z:zx) | Text.toUpper(x) == "GET" = Just y
reqRes _ = Nothing



test_requestedResource =
  [ requestedResource [""]                      == Nothing
  , requestedResource ["ALMA"]                  == Nothing
  , requestedResource ["GET"]                   == Nothing
  , requestedResource ["GET /index.html"]       == Nothing
  , requestedResource ["ALMA /index.html alma"] == Nothing
  , requestedResource ["GET /index.html alma"]  == Just "/index.html"
  , requestedResource ["get /index.html alma"]  == Just "/index.html"

  , requestedResource ["GET /alma.html HTTP/1.1", "Korte: very"]
    == Just "/alma.html"
  ]

-----------------------------------------------------
parseResource :: Text -> Maybe (Text, Text)
parseResource input = do
  let maybeRest = Text.stripPrefix "/" input 
  case maybeRest of
    Just rest -> do
      let (cmd, next) = Text.break (== '/') rest
      if (Text.null next)
        then return (cmd, "")
        else return (cmd, Text.tail next)
    Nothing -> Nothing
  

test_parseResource =
  [ parseResource ""                   == Nothing
  , parseResource "alma"               == Nothing
  , parseResource "korte/alma"         == Nothing
  , parseResource "/alma"              == Just ("alma", "")
  , parseResource "/barack/"           == Just ("barack", "")
  , parseResource "/alma/korte"        == Just ("alma", "korte")
  , parseResource "/korte/barack/alma" == Just ("korte", "barack/alma")
  ]

-----------------------------------------------------
cmdSay :: Text -> Seq Text -> Seq Text
cmdSay e seq = if (Text.null e) 
                 then seq 
                 else seq Seq.|> 
                   Text.concat [
                   Text.pack $ show $ 1 + Seq.length seq, 
                   Text.pack "> ",
                   e ]
   

 
test_cmdSay =
  [ cmdSay "" Seq.empty
    == Seq.empty
  , cmdSay "" (Seq.fromList ["alma", "korte"])
    == Seq.fromList ["alma", "korte"]
  , cmdSay "alma" Seq.empty
    == Seq.singleton "1> alma"
  , cmdSay "alma" (Seq.fromList ["korte", "barack"])
    == Seq.fromList ["korte", "barack", "3> alma"]
  ]

-----------------------------------------------------

cmdReset :: Text -> Seq Text -> Seq Text
cmdReset _ _ = Seq.empty

test_cmdReset =
  [ cmdReset "" Seq.empty
    == Seq.empty
  , cmdReset "" (Seq.fromList ["alma", "korte"])
    == Seq.empty
  , cmdReset "korte" (Seq.fromList ["alma", "barack"])
    == Seq.empty
  ]

-----------------------------------------------------
{-
cmdAppend :: Text -> Seq Text -> Seq Text
-- Mivel a sorozat minden tagján ugyanazt a műveletet hajtjuk végre, ezt tehetjük párhuzamosan 
-- tegyünk is így, a Control.Parallel.Strategies csomag alkamazásával! 
-- Ezt a párhuzamosítást ugyanúgy végezhetjük el, mintha egy listán dolgoznánk, 
-- csak a Seq használata miatt a map helyett az fmap függvényt,
--  a parList helyett a parTraversable kiértékelési stratégiát kell használnunk.

test_cmdAppend =
  [ cmdAppend "" Seq.empty
    == Seq.empty
  , cmdAppend "" (Seq.fromList ["alma", "korte"])
    == Seq.fromList ["alma", "korte"]
  , cmdAppend "alma" Seq.empty
    == Seq.empty
  , cmdAppend "alma" (Seq.fromList ["korte", "barack"])
    == Seq.fromList ["korte | alma", "barack | alma"]
  , cmdAppend "4" ("0" Seq.<| cmdAppend "1" (Seq.fromList ["2", "3"]))
    == Seq.fromList ["0 | 4", "2 | 1 | 4", "3 | 1 | 4"]
  ]

-}
