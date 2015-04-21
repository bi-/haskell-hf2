# haskell-hf2

HTTP szerver
A feladatok egymásra épülnek, ezért a megadásuk sorrendjében kell ezeket megoldani! A függvények definíciójában lehet, sőt javasolt is alkalmazni a korábban definiált függvényeket.

Tekintve, hogy a tesztesetek, bár odafigyelés mellett íródnak, nem fedik le minden esetben a függvény teljes működését, határozottan javasolt még külön próbálgatni a megoldásokat beadás előtt, vagy megkérdezni az oktatókat!

A feladatban egy egyszerű HTTP szervert fogunk megvalósítani, amelyen szöveges üzenetek hagyhatók. A megvalósításhoz szükség lesz a OverloadedStrings kiterjesztésre:

{-# LANGUAGE OverloadedStrings #-}
Az alábbi modulokat, függvényeket érdemes importálni:

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

import qualified Data.Sequence as Seq
import qualified Data.Text     as Text
import qualified Data.Text.IO  as Text
Ha a fejlesztést Windows alatt végezzük, érdemes a GHCi helyett a WinGhci eszközt használni.

Sorok olvasása
Definiáljuk a hGetLines függvényt, amely az első üres sorig olvas sorokat egy IO csatornáról! A függvény a sorokat Text értékek listájaként adja vissza. Az IO csatornát a Handle típussal azonosítjuk, egy Text típusú sort olvasni a Data.Text.IO.hGetLine függvénnyel tudunk.

Tipp: Olvassunk be egy sort. Ha ez nem üres, fűzzük a rekurzívan előálló többi sor elejére.

A függvény típusa:

hGetLines :: Handle -> IO [Text]
A függvényt teszteljük interaktívan, a standard bemenet segítségével! (hGetLines stdin)

Új kapcsolat fogadása
Definiáljuk az acceptFork függvényt, amely egy bejövő TCP kapcsolatot fogad, majd egy új szálon dolgozik ezzel a kapcsolattal! Az első paraméter a szerver socket, a második paraméter az akció, amelyet új szálon akarunk végrehajtani a kapcsolat létrejötte után.

A függvény típusa:

acceptFork :: Socket -> (Handle -> IO ()) -> IO ()
Először az accept függvénnyel fogadjuk a kapcsolatot! Ez egy hármassal tér vissza, melynek csak az első komponensére lesz szükségünk, ez a kapcsolatot reprezentáló Handle érték.
A kommunikáció pufferelését kapcsoljuk ki a hSetBuffering függvény segítségével! A pufferelés módja NoBuffering.
A HTTP protokoll az új sorokat a \r\n karakterekkel, míg a Haskell csak a \n karakterrel ábrázolja. A hSetNewlineMode függvénnyel állítsuk be az újsor karakterek automatikus átírását! Ezt a (NewlineMode CRLF CRLF) paraméterrel tehetjük.
Indítsunk egy új szálat, amely a kapott akciót futtatja az accept függvénytől kapott Handle értéken!
Végül rekurzívan fogadjuk a következő bejövő kapcsolatot!
HTTP válasz előállítása
Definiáljuk a response függvényt, amely egy szöveget kiegészít a szükséges HTTP fejléccel!

A függvény típusa:

response :: Text -> Text
A válasz a következő alakú legyen:

HTTP/1.0 200 OK
Connection: close
Content-Type: text/plain
<üres sor>
<paraméterként kapott szöveg>
Tesztesetek:

test_response =
  [ response "" ==
    "HTTP/1.0 200 OK\nConnection: close\nContent-Type: text/plain\n\n\n"
  , response "alma" ==
    "HTTP/1.0 200 OK\nConnection: close\nContent-Type: text/plain\n\nalma\n"
  , response "al\nma" ==
    "HTTP/1.0 200 OK\nConnection: close\nContent-Type: text/plain\n\nal\nma\n"
  ]
Kliens kezelése
Definiáljuk a handleClient függvényt, amely a klienssel való kapcsolatot kezeli!

A függvény típusa:

handleClient :: Handle -> IO ()
A függvény egyelőre olvassa be a klienstől érkező sorokat -- üres sor jelzi a HTTP kérés végét. Majd a hPutStr függvénnyel küldjük vissza a "szia" (vagy más konstans) szövegből készített HTTP választ! Végül a hFlush és hClose függvényekkel zárjuk le a kapcsolatot!

Szerver indítása
Definiáljuk a main függvényt, amely elindítja a szerverünket! Mivel a kapcsolatok fogadása egy végtelen rekurzív hívás, ezt egy új szálon fogadjuk elindítani. A fő szálon beolvasunk egy sort a standard bemenetről, ezután megszakítjuk a kapcsolatokat fogadó szálat. Ezzel effektíve egy enterrel le tudjuk állítani a szerverünket GHCi-ből is.

A függvény típusa:

main :: IO ()
Hozzuk létre a szerver socketet a listenOn (PortNumber 8000) hívással! Így tehát a szerver a 8000 porton fog futni.
Indítsuk el egy új szálon a bejövő kapcsolatok fogadását! Ehhez a korábban megírt acceptFork és handleClient függvényeket kell használni. Mivel ezt a szálat később meg akarjuk szakítani, kössük egy változóval a forkIO visszatérési értékét!
Olvassunk be egy sort a standard bemenetről! (getLine)
Miután megjött a bemenetről ez a sor, szakítsuk meg az előbb elindított szálat a killThread függvénnyel!
Zárjuk le a szerver socketet a sClose függvénnyel!
A helyes működést a szerver elindításával tesztelhetjük, nyissuk meg egy böngészőből a http://localhost:8000 címet! Windows alatt az accept függvényt sajnos nem szakítja meg a killThread hívás, emiatt körülményesebb lenne a tesztelés. (Mivel a fogadó szál nem terminál, minden tesztelés után újra kellene indítani a GHCi-t.) Ezt azzal kerülhetjük el, hogy kikommentezzük a main függvényben a killThread hívást, a szerver socket bezárása megszakítja az accept futtását.

HTTP kérés feldolgozása
Definiáljuk a requestedResource függvényt, amely egy HTTP kérés sorai alapján visszaadja, hogy milyen címet kér le a kliens! (Ha érvényes a kérés.)

A függvény típusa:

requestedResource :: [Text] -> Maybe Text
A kérés első sorát bontsuk szavakra a Data.Text.words függvénnyel! Ha az első sor legalább három szóból áll és az első szó a GET tetszőlegesen kis- vagy nagybetűkkel, adjuk vissza a második szót!

Tesztesetek:

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
Lekért oldal feldolgozása
A HTTP szerverünkön szöveges üzenetek rögzítését fogjuk megvalósítani. Ha az URL /say/<string>, akkor üzenetet hagyunk. Ha /reset, akkor töröljük az üzeneteket, ha /append/<string>, akkor minden üzenet végére hozzáfűzünk egy szöveget. Tehát az elérési út első tagja egy parancsnak felel meg, ezt követheti egy paraméter.

Definiáljuk a parseResource függvényt, amely a lekért oldal alapján visszaadja ezt a parancsot és paramétert, ha megfelelő formájú az URL!

A függvény típusa:

parseResource :: Text -> Maybe (Text, Text)
Szedjük le a szövegből a kezdő / karaktert, ezt megtehetjük a Data.Text.stripPrefix függvénnyel! Majd vegyük a következő / karakterig tartó részszöveget (Data.Text.break), ez lesz a parancs. A paramétert pedig úgy kapjuk, hogy a maradék szövegből elvesszük a kezdő / karaktert, kivéve ha üres a maradék szöveg (Data.Text.tail). Ha az egész szöveg nem / karakterrel kezdődik, Nothing értéket adjunk vissza!

Tesztesetek:

test_parseResource =
  [ parseResource ""                   == Nothing
  , parseResource "alma"               == Nothing
  , parseResource "korte/alma"         == Nothing
  , parseResource "/alma"              == Just ("alma", "")
  , parseResource "/barack/"           == Just ("barack", "")
  , parseResource "/alma/korte"        == Just ("alma", "korte")
  , parseResource "/korte/barack/alma" == Just ("korte", "barack/alma")
  ]
Üzenet hagyása
A szerveren hagyott üzeneteket a Seq adattípus segítségével fogjuk tárolni. A parancsokat megvalósító három függvény a parancs paramétere és az üzenetek sorozata alapján állítja elő az új sorozatot.

Definiáljuk a cmdSay függvényt, amely a nem üres szöveget a sorozat végéhez fűzi! A szöveg elejére először adjuk hozzá a "n> " részszöveget, ha n a most beszúrt szöveg sorszáma a sorozatban!

A függvény típusa:

cmdSay :: Text -> Seq Text -> Seq Text
A sorozat végére a (Data.Sequence.|>) operátorral szúrhatunk elemet, szövegeket összefűzni pedig a Data.Text.concat függvénnyel tudunk.

Tesztesetek:

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
Üzenetek törlése
Definiáljuk a cmdReset függvényt, amely bármely paraméter és üzenetlista esetén egy üres sorozatot ad vissza!

Típusa:

cmdReset :: Text -> Seq Text -> Seq Text
Tesztesetek:

test_cmdReset =
  [ cmdReset "" Seq.empty
    == Seq.empty
  , cmdReset "" (Seq.fromList ["alma", "korte"])
    == Seq.empty
  , cmdReset "korte" (Seq.fromList ["alma", "barack"])
    == Seq.empty
  ]
Üzenetek végére fűzés
Definiáljuk a cmdAppend függvényt, amely a paramétert hozzáfűzi minden üzenet végéhez, ha a paraméter nem üres! A régi üzenet és a paraméter közé fűzzük még be a " | " szöveget!

Típusa:

cmdAppend :: Text -> Seq Text -> Seq Text
Mivel a sorozat minden tagján ugyanazt a műveletet hajtjuk végre, ezt tehetjük párhuzamosan -- tegyünk is így, a Control.Parallel.Strategies csomag alkamazásával! Ezt a párhuzamosítást ugyanúgy végezhetjük el, mintha egy listán dolgoznánk, csak a Seq használata miatt a map helyett az fmap függvényt, a parList helyett a parTraversable kiértékelési stratégiát kell használnunk.

Tesztesetek:

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
Parancs végrehajtása
Definiáljuk a handleRequest függvényt, amely az eddig megírt függvények segítségével feldolgozza a HTTP kérés sorait és ez alapján transzformálja az üzenetek sorozatát! Ha a kérés vagy a parancs nem megfelelő, azaz a requestedResource vagy a parseResource függvény Nothing értéket ad vissza, ne változtassunk semmit a sorozaton!

A függvény típusa:

handleRequest :: [Text] -> Seq Text -> Seq Text
Teszttulajdonság QuickCheckhez:

instance Arbitrary Text where
  arbitrary = Text.pack `fmap` arbitrary

instance Arbitrary a => Arbitrary (Seq a) where
  arbitrary = Seq.fromList `fmap` arbitrary

prop_handleRequest :: Text -> [Text] -> Seq Text -> Property
prop_handleRequest resourceSuffix xs ys =
  Text.all (not . isSpace) resourceSuffix ==>
    flip all prefs $ \(pref, f) ->
      handleRequest (request pref) ys == f resourceSuffix ys
  where
    requestHead pref = Text.concat ["GET ", pref, resourceSuffix, " HTTP/1.1"]
    request pref = requestHead pref : xs
    prefs =
      [ ("/say/", cmdSay), ("/reset/", cmdReset)
      , ("/append/", cmdAppend), ("/alma/", const id)
      ]
Üzenetek a szerveren
A szerver aktuális állapotát, azaz az üzenetek listáját az MVar típus segítségével fogjuk tárolni és biztonságosan megosztani az egyes szálak között.

Adjunk egy új paramétert a handleClient függvényhez:

handleClient :: MVar (Seq Text) -> Handle -> IO ()
Írjuk át a függvényt:

A HTTP kérés olvasása után vegyük ki az üzeneteket az MVar-ból!
Számítsuk ki a kérés és az üzenetek sorozata alapján az új sorozatot! Ezt tegyük be az MVar-ba!
A kliensnek küldjük el (a kérés tartalmától függetlenül), hogy hány üzenet van és az üzenetek listáját!
Ezt kövesse a kapcsolat lezárása úgy, mint eddig!
Írjuk át a main függvényt is:

A szerver socket létrehozása után hozzunk létre egy új MVar-t az üres sorozattal!
A kapcsolatokat kezelő akciónak adjuk paraméterül ezt az MVar értéket!
A tesztelést ismét egy böngészővel végezhetjük. Próbáljuk ki mindhárom műveletet: a rögzítést, a törlést és a hozzáfűzést!

Egy példa válasza a szervernek (HTTP fejlécek nélkül):

3 messages
1> alma | hozzafuztem
2> korte | hozzafuztem
3> barack
Ez az állapot az alábbi oldallekérések után áll elő:

http://localhost:8000/say/alma
http://localhost:8000/say/korte
http://localhost:8000/append/hozzafuztem
http://localhost:8000/say/barack
