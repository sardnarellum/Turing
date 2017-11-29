# Turing-gépek
A feladatok egymásra épülnek, ezért a megadásuk sorrendjében kell ezeket megoldani! A függvények definíciójában lehet, sőt javasolt is alkalmazni a korábban definiált függvényeket.

Az egyes feladatokhoz a tesztesetek logikai értékekből álló listaként adottak. Tekintve, hogy a tesztesetek, bár odafigyelés mellett íródnak, nem fedik le minden esetben a függvény teljes működését, határozottan javasolt még külön próbálgatni a megoldásokat beadás előtt, vagy megkérdezni az oktatókat!

A feladatban Turing-gépek szimulációját, és ehhez a zipper adatszerkezetet fogjuk megvalósítani.

Az alábbi importált modulokkal érdemes dolgozni: `StdEnv`, `StdLib`, `StdGeneric`, `GenEq`.

## Zipper adattípus
A Turing-gép szalagjának szimulációjához olyan lista jellegű adatszerkezetre van szükségünk, amelyben a balra és jobbra léptetés egyszerűen megvalósítható. A zipper alkalmazása esetén három részre bontjuk a listát: egy kijelölt, fókusz alatt álló elemre, és az ettől balra és jobbra lévő elemek listájára.

Ezt két listával fogjuk leírni. Az első a fókusz előtti elemek listája, fordított sorrendben. A fordított sorrend miatt ha balra akarunk lépni, csak az első lista fejelmét kell áttennünk a második listába. A második lista a kijelölt elem és az utána lévő elemek listája. Így jobbra lépésnél pedig a második lista fejelemét helyezzük át az első lista elejére.

Például az [1, 2, 3, 4, 5, 6] listát a [3, 2, 1] és [4, 5, 6] listával fogjuk leírni, ha a 4 a kijelölt elem.

Mivel a két lista végtelen is lehet, a Turing-gépek két irányban végtelen szalagját nem kell külön megvalósítanunk.

Definiáljuk a `Zipper` adattípust, amely egy a típusváltozóval paraméterezett! Az adatkonstruktora legyen `Z`, tároljunk vele két `[a]` típusú listát! Vezessük le a generikus egyenlőségvizsgálatot (`gEq` generikus függvény) a `Zipper` típusra!

**FONTOS: _az adatkonstruktort tényleg `Z`-nek nevezzük el, különben nem működik az automatikus tesztelés!_**

## Zipper létrehozása listából
Definiáljuk a `fromList` függvényt, amely egy listából létrehoz egy olyan zippert, amely a lista első elemére fókuszál!

A függvény típusa:

```
fromList :: [a] -> Zipper a
```

Tesztesetek:

```
test_fromList =
  [ fromList empty   === Z [] []
  , fromList [1]     === Z [] [1]
  , fromList [1..10] === Z [] [1..10]
  ]
  where
    empty :: [Int]
    empty = []
```

## Kijelölt elem lekérdezése
Definiáljuk a `read` függvényt, amely a zipper kijelölt elemét adja vissza! Üres zipper esetén nem kell működnie a függvénynek.

A függvény típusa:

```
read :: (Zipper a) -> a
```

Tesztesetek:

```
test_read =
  [ read (Z [] [1])      == 1
  , read (Z [] [2..])    == 2
  , read (Z [1..] [3..]) == 3
  ]
```

## Kijelölt elem módosítása
Definiáljuk a `write` függvényt, amellyel a zipper kijelölt elemét lehet felülírni! Üres zipper esetén nem kell működnie a függvénynek.

A függvény típusa:

```
write :: a (Zipper a) -> Zipper a
```

Tesztesetek:

```
test_write =
  [ write 9 (Z [] [1])        === Z [] [9]
  , write 9 (Z [] [1..3])     === Z [] [9,2,3]
  , write 9 (Z [4..6] [1..3]) === Z [4..6] [9,2,3]
  ]
```

## Zipper léptetése
Definiáljuk a Movement adattípust három adatkonstruktorral: Forward, Backward, Stay!

**FONTOS: _a konstruktorok nevei pontosan ugyanezek legyenek, és ebben a sorrendben legyenek megadva._**

Definiáljuk a `move` függvényt, amely a kapott Movement érték alapján a zippert jobbra vagy balra lépteti, illetve változatlanul hagyja! Ha olyan irányba léptetnénk, ahol már nincs elem, a függvénynek nem kell működnie.

A függvény típusa:

```
move :: Movement (Zipper a) -> Zipper a
```

Tesztesetek:

```
test_move =
  [ move Stay (Z empty [])            === Z [] []
  , move Stay (Z [1,2,3] [4,5,6])     === Z [1,2,3] [4,5,6]
  , move Forward (Z [1,2,3] [4,5,6])  === Z [4,1,2,3] [5,6]
  , move Backward (Z [1,2,3] [4,5,6]) === Z [2,3] [1,4,5,6]
  ]
  where
    empty :: [Int]
    empty = []
```

## Kijelölt elem környéke
Definiáljuk az `around` függvényt, amellyel egy zipper kijelölt elemét, valamint az előtte és utána lévő r számú elemet kaphatjuk meg egy listában tetszőleges r paraméterre!

A függvény típusa:

```
around :: Int (Zipper a) -> [a]
```

Tesztesetek:

```
test_around =
  [ around 0 (Z [] [1])      == [1]
  , around 3 (Z [1..] [0..]) == [3,2,1,0,1,2,3]
  ]
```

## Végtelen zipper létrehozása listából
Definiáljuk a `fromListInf` függvényt, amely egy listából létrehoz egy olyan zippert, amely a lista első elemére fókuszál, a lista két vége előtt és után pedig egy paraméterben kapott elem ismétlődik végtelenül!

A függvény típusa:

```
fromListInf :: a [a] -> Zipper a
```
Tipp: *egy elemből képzett végtelen listát a repeat függvénnyel kaphatunk.*

Tesztesetek:

```
test_fromListInf =
  [ let (Z xs ys) = fromListInf 0 [1..5]
    in  take 100 xs == repeatn 100 0
        && take 100 ys == [1..5] ++ repeatn 95 0
  ]
```

## Turing-gép típusosztály
A Turing-gépen definiált műveleteket kiemeljük egy típusosztályba, hogy ezeket egy interfészen keresztül használhassuk. Definiáljuk a Machine típusosztályt, amelynek a következő műveletei vannak a t típusparaméter és Machine t korlátozás mellett:

`done :: (t a) -> Bool`
Annak lekérdezése, hogy az a típusú szimbólumokkal dolgozó Turing-gép futása véget ért-e.

`tape :: (t a) -> Zipper a`
A Turing-gép szalagjának lekérdezése.

`step :: (t a) -> t a`
A Turing gép egy lépésének elvégzése.

## Turing-gép adattípus
Definiáljuk a State adattípust, amellyel a Turing-gép aktuális állapotát fogjuk ábrázolni! Adatkonstruktorai: InState Int, ha futás közben egy bizonyos állapotban vagyunk; Accepted, ha sikeresen lefutott a program, Rejected, ha sikertelenül futott le.

**FONTOS: _a konstruktorok nevei pontosan ugyanezek legyenek, és ebben a sorrendben legyenek megadva._**

Definiáljuk a TuringMachine adattípust, amelynek van egy a típusparamétere! Adatkonstruktora TM, amely a következő adatokat tárolja:

- `State` -- az aktuális állapot.
- `Zipper a` -- a szalag.
- `Int a -> (State, a, Movement)` -- az állapotátmeneteket leíró függvény. Az aktuális állapot sorszámából és a szalagon kijelölt szimbólumból képez egy új állapotba, egy szalagra írandó szimbólumba és egy irányba.

**FONTOS: _a konstruktor neve `TM` legyen (mindkettő nagybetű), és a paraméterei a fent megadott sorrendben legyenek megadva._**

## A Turing-gép működése
Definiáljuk a TuringMachine típus Machine-példányát!

A `done` és `tape` függvények a típusban tárolt adatok alapján adják vissza az eredményt.

A `step` függvény működése a következő:

A típusban tárolt függvényt meghívja az aktuális állapottal és a szalagon kijelölt szimbólummal. Ez visszaadja az új állapotot, a kiírandó szimbólumot és az irányt. Az új szalagot a következőképpen kapjuk: kiírjuk a szimbólumot a régi szalagra, majd ezután léptetjük az irány alapján. A függvényt változatlanul hagyja a típusban.

Tesztesetek:
```
test_done =
  [ not (done (TM (InState 0) undef undef))
  , done (TM Accepted undef undef)
  , done (TM Rejected undef undef)
  ]

test_tape =
  [ tape (TM Accepted (fromList [1..5]) undef) === fromList [1..5]
  ]

test_step =
  [ let m = step (TM (InState 0) (fromList ['a','b']) f)
    in  not (done m)
        && tape m === Z ['b'] ['b']
  , let m = step (TM (InState 0) (fromList ['b','b']) f)
    in  not (done m)
        && tape m === Z ['a'] ['b']
  , let m = step (TM (InState 1) (fromList ['a','b']) f)
    in  done m
        && tape m === fromList ['x','b']
  ]
  where
    f 0 'a' = (InState 0, 'b', Forward)
    f 0 'b' = (InState 0, 'a', Forward)
    f 1 _   = (Accepted,  'x', Stay)
```

## A Turing-gép futtatása
Definiáljuk a `run` függvényt, amely lefuttat egy Turing-gépet és visszaadja az összes állapotát a futás során! Ha a gép már lefutott, adjuk vissza ezt egy egyelemű listában! Ha nem, végezzünk el egy lépést, futtassuk le rekurzívan az adódó gépet, majd fűzzük a lista elejére a gépet!

A függvény típusa:

`run :: (t a) -> [t a] | Machine t`
Tesztesetek:
```
test_run =
  [ let m = last (run (tm ['a','b','x','x']))
    in done m
       && tape m === Z ['x','a','b'] ['x']
  , let m = last (run (tm ['b','a','x','x']))
    in done m
       && tape m === Z ['x','b','a'] ['x']
  , let m = last (run (tm ['a','b','x','a']))
    in done m
       && tape m === Z ['x','a','b'] ['!']
  ]
  where
    tm xs = TM (InState 0) (fromList xs) f
    f 0 'a' = (InState 0, 'b', Forward)
    f 0 'b' = (InState 0, 'a', Forward)
    f 0 'x' = (InState 1, 'x', Forward)
    f 1 'x' = (Accepted,  'x', Stay)
    f _ ch  = (Rejected,  '!', Stay)
```

## Turing-gép állapotainak megjelenítése
Tekinstünk azokat a Turing-gépeket, amelyek szimbólumai karakterek! Definiáljuk a `showStates` függvényt, amely lefuttatja a gépet, majd minden állapot szalagján veszi a fókuszált elem 5 sugarú környezetét (`around`), ezt a listát pedig pedig szövegesen adja vissza!

A függvény típusa:

`showStates :: (t Char) -> [String] | Machine t`
Tipp: *karakterek listáját String értékké tudjuk alakítani a `toString` függvénnyel.*

Tesztesetek:
```
test_showStates =
  [ showStates (tm ['a','b','x','x'])
    == [ "     abxx  "
       , "    bbxx   "
       , "   baxx    "
       , "  baxx     "
       , "  baxx     "
       ]
  , showStates (tm ['a','b','x','a'])
    == [ "     abxa  "
       , "    bbxa   "
       , "   baxa    "
       , "  baxa     "
       , "  bax!     "
       ]
  ]
    where
      tm xs = TM (InState 0) (fromListInf ' ' xs) f
      f 0 'a' = (InState 0, 'b', Forward)
      f 0 'b' = (InState 0, 'a', Forward)
      f 0 'x' = (InState 1, 'x', Forward)
      f 1 'x' = (Accepted,  'x', Stay)
      f _ ch  = (Rejected,  '!', Stay)
```
## Segítség a feltöltéshez
Az alábbi állományt érdemes módosítani, így, szövegesen kell feltölteni (az alábbi természetesen hibás működésű program),

**FONTOS: _csak olyan megoldást töltsünk fel, amely lefordul!_**
```
module Turing

import StdEnv, StdLib, StdGeneric, GenEq

:: Zipper a = Something

derive gEq Zipper

fromList :: [a] -> Zipper a
fromList a = abort "not defined"

read :: (Zipper a) -> a
read a = abort "not defined"

write :: a (Zipper a) -> Zipper a
write a b = abort "not defined"

:: Movement = Something1

move :: Movement (Zipper a) -> Zipper a
move a b = abort "not defined"

around :: Int (Zipper a) -> [a]
around a b = abort "not defined"

fromListInf :: a [a] -> Zipper a
fromListInf a b = abort "not defined"

class Machine t where
  done :: (t a) -> Bool
  tape :: (t a) -> Zipper a
  step :: (t a) -> t a

:: State = Something2

:: TuringMachine a = Something3

instance Machine TuringMachine where
  done a = abort "undefined"
  tape a = abort "undefined"
  step a = abort "undefined"

run :: (t a) -> [t a] | Machine t
run a = abort "not defined"

showStates :: (t Char) -> [String] | Machine t
showStates a = abort "not defined"
A tesztelést az alábbi függvényekkel lehet segíteni:

tests :: [[Bool]]
tests =
  [ test_fromList
  , test_read
  , test_write
  , test_move
  , test_around
  , test_fromListInf
  , test_done
  , test_tape
  , test_step
  , test_run
  , test_showStates
  ]

Start = (all and tests, zip2 [1..] (map and tests))
```
