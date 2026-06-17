-- | The Verdict standard library, written in Verdict itself: thin, typed
-- | wrappers over the FinVM builtins registry (logic / bigint / db / cache).
-- | It is auto-injected before every user module and tree-shaken, so a program
-- | only carries the wrappers it actually reaches (which also keeps `capabilities`
-- | precise). `Json` is the dynamic payload type for FFI/DB values.
module Verdict.Std.Prelude (preludeSource) where

preludeSource :: String
preludeSource =
  """
module Prelude exposing (..)

type Option a = Some a | None

type Result e a = Err e | Ok a

type Decoder a = Decoder Json

type Encoder a = Encoder Json

type ActorRef m = MkActorRef Pid

-- Logic ----------------------------------------------------------------------

and : Bool -> Bool -> Bool
and a b = builtin("logic.and@1", a, b)

or : Bool -> Bool -> Bool
or a b = builtin("logic.or@1", a, b)

not : Bool -> Bool
not b = builtin("logic.not@1", b)

-- Program inputs (prefer `input name : Ty` declarations; dynamic access below)

inputGet : String -> Json
inputGet name = builtin("input.get@1", name)

inputInt : String -> Int
inputInt name = builtin("input.get@1", name)

inputBool : String -> Bool
inputBool name = builtin("input.get@1", name)

inputString : String -> String
inputString name = builtin("input.get@1", name)

-- BigInt math ----------------------------------------------------------------

modPow : Int -> Int -> Int -> Int
modPow base exp mod = builtin("bigint.modPow@1", base, exp, mod)

modInv : Int -> Int -> Int
modInv a mod = builtin("bigint.modInv@1", a, mod)

-- Lists ----------------------------------------------------------------------

mapGo : (a -> b) -> List a -> Int -> List b -> List b
mapGo f xs i acc =
  if i == length(xs) then acc
  else mapGo(f, xs, i + 1, append(acc, f(get(xs, i))))

map : (a -> b) -> List a -> List b
map f xs = mapGo(f, xs, 0, [])

filterGo : (a -> Bool) -> List a -> Int -> List a -> List a
filterGo f xs i acc =
  if i == length(xs) then acc
  else if f(get(xs, i)) then filterGo(f, xs, i + 1, append(acc, get(xs, i)))
  else filterGo(f, xs, i + 1, acc)

filter : (a -> Bool) -> List a -> List a
filter f xs = filterGo(f, xs, 0, [])

foldlGo : (b -> a -> b) -> b -> List a -> Int -> b
foldlGo f acc xs i =
  if i == length(xs) then acc
  else foldlGo(f, f(acc, get(xs, i)), xs, i + 1)

foldl : (b -> a -> b) -> b -> List a -> b
foldl f acc xs = foldlGo(f, acc, xs, 0)

isEmpty : List a -> Bool
isEmpty xs = length(xs) == 0

rangeGo : Int -> Int -> List Int -> List Int
rangeGo i n acc =
  if i == n then acc
  else rangeGo(i + 1, n, append(acc, i))

range : Int -> List Int
range n = rangeGo(0, n, [])

reverseGo : List a -> Int -> List a -> List a
reverseGo xs i acc =
  if i < 0 then acc
  else reverseGo(xs, i - 1, append(acc, get(xs, i)))

reverse : List a -> List a
reverse xs = reverseGo(xs, length(xs) - 1, [])

concatGo : List a -> Int -> List a -> List a
concatGo ys i acc =
  if i == length(ys) then acc
  else concatGo(ys, i + 1, append(acc, get(ys, i)))

concat : List a -> List a -> List a
concat xs ys = concatGo(ys, 0, xs)

sumGo : List Int -> Int -> Int -> Int
sumGo xs i acc =
  if i == length(xs) then acc
  else sumGo(xs, i + 1, acc + get(xs, i))

sum : List Int -> Int
sum xs = sumGo(xs, 0, 0)

productGo : List Int -> Int -> Int -> Int
productGo xs i acc =
  if i == length(xs) then acc
  else productGo(xs, i + 1, acc * get(xs, i))

product : List Int -> Int
product xs = productGo(xs, 0, 1)

containsGo : a -> List a -> Int -> Bool
containsGo x xs i =
  if i == length(xs) then False
  else if get(xs, i) == x then True
  else containsGo(x, xs, i + 1)

contains : a -> List a -> Bool
contains x xs = containsGo(x, xs, 0)

takeGo : Int -> List a -> Int -> List a -> List a
takeGo n xs i acc =
  if i == n then acc
  else if i == length(xs) then acc
  else takeGo(n, xs, i + 1, append(acc, get(xs, i)))

take : Int -> List a -> List a
take n xs = takeGo(n, xs, 0, [])

dropGo : Int -> List a -> Int -> List a -> List a
dropGo n xs i acc =
  if i == length(xs) then acc
  else if i < n then dropGo(n, xs, i + 1, acc)
  else dropGo(n, xs, i + 1, append(acc, get(xs, i)))

drop : Int -> List a -> List a
drop n xs = dropGo(n, xs, 0, [])

-- Option helpers -------------------------------------------------------------

mapOption : (a -> b) -> Option a -> Option b
mapOption f o = match o { Some v -> Some(f(v)), None -> None }

isNone : Option a -> Bool
isNone o = match o { Some v -> False, None -> True }

-- Strings (deterministic str.* FFI builtins) ---------------------------------

strLength : String -> Int
strLength s = builtin("str.length@1", s)

strConcat : String -> String -> String
strConcat a b = builtin("str.concat@1", a, b)

strSlice : String -> Int -> Int -> String
strSlice s start len = builtin("str.slice@1", s, start, len)

indexOf : String -> String -> Int
indexOf s needle = builtin("str.indexOf@1", s, needle)

strContains : String -> String -> Bool
strContains s needle = indexOf(s, needle) > -1

split : String -> String -> List String
split s sep = builtin("str.split@1", s, sep)

toUpper : String -> String
toUpper s = builtin("str.toUpper@1", s)

toLower : String -> String
toLower s = builtin("str.toLower@1", s)

trim : String -> String
trim s = builtin("str.trim@1", s)

fromInt : Int -> String
fromInt n = builtin("str.fromInt@1", n)

replace : String -> String -> String -> String
replace s from to = builtin("str.replace@1", s, from, to)

parseInt : String -> Option Int
parseInt s =
  let r = builtin("str.toInt@1", s) in
  if r == unit then None else Some(r)

-- Regex strings (host-backed regex.* FFI builtins) ---------------------------

regexTest : String -> String -> Bool
regexTest pattern input = builtin("regex.test@1", pattern, input)

regexFindAll : String -> String -> List String
regexFindAll pattern input = builtin("regex.findAll@1", pattern, input)

regexReplace : String -> String -> String -> String
regexReplace pattern replacement input = builtin("regex.replace@1", pattern, replacement, input)

regexSplit : String -> String -> List String
regexSplit pattern input = builtin("regex.split@1", pattern, input)

-- JSON decoders / encoders ---------------------------------------------------

jsonValueDecoder : Decoder Json
jsonValueDecoder = Decoder({ kind = "value" })

jsonIntDecoder : Decoder Int
jsonIntDecoder = Decoder({ kind = "int" })

jsonStringDecoder : Decoder String
jsonStringDecoder = Decoder({ kind = "string" })

jsonBoolDecoder : Decoder Bool
jsonBoolDecoder = Decoder({ kind = "bool" })

jsonField : String -> Decoder a -> Decoder a
jsonField name decoder =
  match decoder { Decoder recipe -> Decoder({ kind = "field", name = name, decoder = recipe }) }

jsonListDecoder : Decoder a -> Decoder (List a)
jsonListDecoder decoder =
  match decoder { Decoder recipe -> Decoder({ kind = "list", decoder = recipe }) }

jsonNullable : Decoder a -> Decoder (Option a)
jsonNullable decoder =
  match decoder { Decoder recipe -> Decoder({ kind = "nullable", decoder = recipe }) }

jsonDecodeValue : Decoder a -> Json -> Result String a
jsonDecodeValue decoder value =
  match decoder { Decoder recipe -> builtin("json.decodeValue@1", recipe, value) }

jsonDecodeString : Decoder a -> String -> Result String a
jsonDecodeString decoder source =
  match decoder { Decoder recipe -> builtin("json.decodeString@1", recipe, source) }

jsonValueEncoder : Encoder Json
jsonValueEncoder = Encoder({ kind = "value" })

jsonIntEncoder : Encoder Int
jsonIntEncoder = Encoder({ kind = "int" })

jsonStringEncoder : Encoder String
jsonStringEncoder = Encoder({ kind = "string" })

jsonBoolEncoder : Encoder Bool
jsonBoolEncoder = Encoder({ kind = "bool" })

jsonListEncoder : Encoder a -> Encoder (List a)
jsonListEncoder encoder =
  match encoder { Encoder recipe -> Encoder({ kind = "list", encoder = recipe }) }

jsonNullableEncoder : Encoder a -> Encoder (Option a)
jsonNullableEncoder encoder =
  match encoder { Encoder recipe -> Encoder({ kind = "nullable", encoder = recipe }) }

jsonEncodeValue : Encoder a -> a -> Json
jsonEncodeValue encoder value =
  match encoder { Encoder recipe -> builtin("json.encodeValue@1", recipe, value) }

jsonEncodeString : Encoder a -> a -> String
jsonEncodeString encoder value =
  match encoder { Encoder recipe -> builtin("json.encodeString@1", recipe, value) }

jsonNull : Json
jsonNull = builtin("json.null@1")

jsonPair : String -> Json -> { key : String, value : Json }
jsonPair key value = { key = key, value = value }

jsonObject : List { key : String, value : Json } -> Json
jsonObject fields = builtin("json.object@1", fields)

-- Math -----------------------------------------------------------------------

max : Int -> Int -> Int
max a b = if a > b then a else b

min : Int -> Int -> Int
min a b = if a < b then a else b

abs : Int -> Int
abs n = if n < 0 then 0 - n else n

clamp : Int -> Int -> Int -> Int
clamp lo hi n = max(lo, min(hi, n))

gcd : Int -> Int -> Int
gcd a b = builtin("math.gcd@1", a, b)

lcm : Int -> Int -> Int
lcm a b = builtin("math.lcm@1", a, b)

pow : Int -> Int -> Int
pow base exp = builtin("math.pow@1", base, exp)

sqrtFloor : Int -> Int
sqrtFloor n = builtin("math.sqrtFloor@1", n)

-- HTTP -----------------------------------------------------------------------

httpGet : String -> { status : Int, ok : Bool, body : String }
httpGet url = builtin("http.get@1", url)

httpPost : String -> String -> { status : Int, ok : Bool, body : String }
httpPost url body = builtin("http.post@1", url, body)

-- System I/O -----------------------------------------------------------------

sysLog : String -> Unit
sysLog msg = builtin("sys.log@1", msg)

sysCwd : String
sysCwd = builtin("sys.cwd@1")

sysReadText : String -> Option String
sysReadText path =
  let r = builtin("sys.readText@1", path) in
  if r == unit then None else Some(r)

sysWriteText : String -> String -> Bool
sysWriteText path contents = builtin("sys.writeText@1", path, contents)

sysEnv : String -> Option String
sysEnv name =
  let r = builtin("sys.env@1", name) in
  if r == unit then None else Some(r)

-- Data processing (host-backed fast paths) ----------------------------------

sortInts : List Int -> List Int
sortInts xs = builtin("data.sortInts@1", xs)

distinctInts : List Int -> List Int
distinctInts xs = builtin("data.distinctInts@1", xs)

sumIntsFast : List Int -> Int
sumIntsFast xs = builtin("data.sumInts@1", xs)

averageFloor : List Int -> Int
averageFloor xs = builtin("data.averageFloor@1", xs)

statsMin : List Int -> Int
statsMin xs = builtin("stats.min@1", xs)

statsMax : List Int -> Int
statsMax xs = builtin("stats.max@1", xs)

meanFloor : List Int -> Int
meanFloor xs = builtin("stats.meanFloor@1", xs)

median : List Int -> Int
median xs = builtin("stats.median@1", xs)

percentileNearest : Int -> List Int -> Int
percentileNearest pct xs = builtin("stats.percentileNearest@1", pct, xs)

varianceFloor : List Int -> Int
varianceFloor xs = builtin("stats.varianceFloor@1", xs)

stddevFloor : List Int -> Int
stddevFloor xs = builtin("stats.stddevFloor@1", xs)

describeInts : List Int -> { count : Int, sum : Int, min : Int, max : Int, mean : Int, median : Int, variance : Int, stddev : Int }
describeInts xs = builtin("stats.describeInts@1", xs)

valueCountsInts : List Int -> List { value : Int, count : Int }
valueCountsInts xs = builtin("stats.valueCountsInts@1", xs)

rollingSumInts : Int -> List Int -> List Int
rollingSumInts window xs = builtin("stats.rollingSumInts@1", window, xs)

-- Technical-analysis / time-series indicators -------------------------------
-- Integer series in, integer series out. Feed scaled prices if you need fixed
-- decimal precision.

sma : List Int -> Int -> List Int
sma src period = builtin("series.sma@1", src, period)

ema : List Int -> Int -> List Int
ema src period = builtin("series.ema@1", src, period)

wma : List Int -> Int -> List Int
wma src period = builtin("series.wma@1", src, period)

rollingMedian : List Int -> Int -> List Int
rollingMedian src period = builtin("series.rollingMedian@1", src, period)

momentum : List Int -> Int -> List Int
momentum src period = builtin("series.momentum@1", src, period)

roc : List Int -> Int -> List Int
roc src period = builtin("series.roc@1", src, period)

rsi : List Int -> Int -> List Int
rsi src period = builtin("series.rsi@1", src, period)

macd : List Int -> Int -> Int -> List Int
macd src fast slow = builtin("series.macd@1", src, fast, slow)

macdSignal : List Int -> Int -> Int -> Int -> List Int
macdSignal src fast slow sig = builtin("series.macdSignal@1", src, fast, slow, sig)

macdHistogram : List Int -> Int -> Int -> Int -> List Int
macdHistogram src fast slow sig = builtin("series.macdHistogram@1", src, fast, slow, sig)

slope : List Int -> Int -> List Int
slope src period = builtin("series.slope@1", src, period)

rollingStd : List Int -> Int -> List Int
rollingStd src period = builtin("series.rollingStd@1", src, period)

realizedVol : List Int -> Int -> List Int
realizedVol src period = builtin("series.realizedVol@1", src, period)

ewmStd : List Int -> Int -> List Int
ewmStd src period = builtin("series.ewmStd@1", src, period)

stdevRatio : List Int -> Int -> Int -> List Int
stdevRatio src short long = builtin("series.stdevRatio@1", src, short, long)

atrApprox : List Int -> Int -> List Int
atrApprox src period = builtin("series.atrApprox@1", src, period)

bollingerUpper : List Int -> Int -> Int -> List Int
bollingerUpper src period nstd = builtin("series.bollingerUpper@1", src, period, nstd)

bollingerLower : List Int -> Int -> Int -> List Int
bollingerLower src period nstd = builtin("series.bollingerLower@1", src, period, nstd)

zscore : List Int -> Int -> List Int
zscore src period = builtin("series.zscore@1", src, period)

percentileRank : List Int -> Int -> List Int
percentileRank src period = builtin("series.percentileRank@1", src, period)

drawdown : List Int -> List Int
drawdown src = builtin("series.drawdown@1", src)

pctChange : List Int -> Int -> List Int
pctChange src period = builtin("series.pctChange@1", src, period)

ratio : List Int -> List Int -> List Int
ratio a b = builtin("series.ratio@1", a, b)

spread : List Int -> List Int -> List Int
spread a b = builtin("series.spread@1", a, b)

rollingCorr : List Int -> List Int -> Int -> List Int
rollingCorr a b period = builtin("series.rollingCorr@1", a, b, period)

rollingBeta : List Int -> List Int -> Int -> List Int
rollingBeta a b period = builtin("series.rollingBeta@1", a, b, period)

relativeMomentum : List Int -> List Int -> Int -> List Int
relativeMomentum a b period = builtin("series.relativeMomentum@1", a, b, period)

hedgeRatio : List Int -> List Int -> Int -> List Int
hedgeRatio a b period = builtin("series.hedgeRatio@1", a, b, period)

add : List Int -> List Int -> List Int
add a b = builtin("series.add@1", a, b)

sub : List Int -> List Int -> List Int
sub a b = builtin("series.sub@1", a, b)

mul : List Int -> List Int -> List Int
mul a b = builtin("series.mul@1", a, b)

div : List Int -> List Int -> List Int
div a b = builtin("series.div@1", a, b)

seriesAbs : List Int -> List Int
seriesAbs src = builtin("series.abs@1", src)

clip : List Int -> Int -> Int -> List Int
clip src lo hi = builtin("series.clip@1", src, lo, hi)

shift : List Int -> Int -> List Int
shift src period = builtin("series.shift@1", src, period)

diff : List Int -> List Int
diff src = builtin("series.diff@1", src)

log : List Int -> List Int
log src = builtin("series.log@1", src)

rollingMax : List Int -> Int -> List Int
rollingMax src period = builtin("series.rollingMax@1", src, period)

rollingMin : List Int -> Int -> List Int
rollingMin src period = builtin("series.rollingMin@1", src, period)

cummax : List Int -> List Int
cummax src = builtin("series.cummax@1", src)

cummin : List Int -> List Int
cummin src = builtin("series.cummin@1", src)

crossover : List Int -> List Int -> List Int
crossover a b = builtin("series.crossover@1", a, b)

crossunder : List Int -> List Int -> List Int
crossunder a b = builtin("series.crossunder@1", a, b)

atrOhlc : List Int -> List Int -> List Int -> Int -> List Int
atrOhlc high low close period = builtin("series.atrOhlc@1", high, low, close, period)

trueRange : List Int -> List Int -> List Int -> List Int
trueRange high low close = builtin("series.trueRange@1", high, low, close)

vwap : List Int -> List Int -> Int -> List Int
vwap close volume period = builtin("series.vwap@1", close, volume, period)

obv : List Int -> List Int -> List Int
obv close volume = builtin("series.obv@1", close, volume)

volumeSma : List Int -> Int -> List Int
volumeSma volume period = builtin("series.volumeSma@1", volume, period)

volumeRatio : List Int -> Int -> List Int
volumeRatio volume period = builtin("series.volumeRatio@1", volume, period)

bodySize : List Int -> List Int -> List Int
bodySize open close = builtin("series.bodySize@1", open, close)

upperWick : List Int -> List Int -> List Int -> List Int
upperWick high open close = builtin("series.upperWick@1", high, open, close)

lowerWick : List Int -> List Int -> List Int -> List Int
lowerWick low open close = builtin("series.lowerWick@1", low, open, close)

rangePct : List Int -> List Int -> List Int
rangePct high low = builtin("series.rangePct@1", high, low)

-- List predicates & search --------------------------------------------------

allGo : (a -> Bool) -> List a -> Int -> Bool
allGo f xs i =
  if i == length(xs) then True
  else if f(get(xs, i)) then allGo(f, xs, i + 1)
  else False

all : (a -> Bool) -> List a -> Bool
all f xs = allGo(f, xs, 0)

anyGo : (a -> Bool) -> List a -> Int -> Bool
anyGo f xs i =
  if i == length(xs) then False
  else if f(get(xs, i)) then True
  else anyGo(f, xs, i + 1)

any : (a -> Bool) -> List a -> Bool
any f xs = anyGo(f, xs, 0)

countGo : (a -> Bool) -> List a -> Int -> Int -> Int
countGo f xs i acc =
  if i == length(xs) then acc
  else if f(get(xs, i)) then countGo(f, xs, i + 1, acc + 1)
  else countGo(f, xs, i + 1, acc)

count : (a -> Bool) -> List a -> Int
count f xs = countGo(f, xs, 0, 0)

findGo : (a -> Bool) -> List a -> Int -> Option a
findGo f xs i =
  if i == length(xs) then None
  else if f(get(xs, i)) then Some(get(xs, i))
  else findGo(f, xs, i + 1)

find : (a -> Bool) -> List a -> Option a
find f xs = findGo(f, xs, 0)

flatMapGo : (a -> List b) -> List a -> Int -> List b -> List b
flatMapGo f xs i acc =
  if i == length(xs) then acc
  else flatMapGo(f, xs, i + 1, concat(acc, f(get(xs, i))))

flatMap : (a -> List b) -> List a -> List b
flatMap f xs = flatMapGo(f, xs, 0, [])

replicateGo : Int -> a -> Int -> List a -> List a
replicateGo n x i acc =
  if i == n then acc
  else replicateGo(n, x, i + 1, append(acc, x))

replicate : Int -> a -> List a
replicate n x = replicateGo(n, x, 0, [])

head : List a -> Option a
head xs = if isEmpty(xs) then None else Some(xs[0])

last : List a -> Option a
last xs = if isEmpty(xs) then None else Some(xs[length(xs) - 1])

-- String formatting ----------------------------------------------------------

joinGo : String -> List String -> Int -> String -> String
joinGo sep parts i acc =
  if i == length(parts) then acc
  else joinGo(sep, parts, i + 1, strConcat(acc, strConcat(sep, parts[i])))

join : String -> List String -> String
join sep parts =
  if isEmpty(parts) then ""
  else joinGo(sep, parts, 1, parts[0])

startsWith : String -> String -> Bool
startsWith s prefix = strSlice(s, 0, strLength(prefix)) == prefix

endsWith : String -> String -> Bool
endsWith s suffix =
  strSlice(s, strLength(s) - strLength(suffix), strLength(suffix)) == suffix

repeatGo : Int -> String -> Int -> String -> String
repeatGo n s i acc =
  if i == n then acc
  else repeatGo(n, s, i + 1, strConcat(acc, s))

repeat : Int -> String -> String
repeat n s = repeatGo(n, s, 0, "")

-- Option combinators ---------------------------------------------------------

andThen : (a -> Option b) -> Option a -> Option b
andThen f o = match o { Some v -> f(v), None -> None }

orElse : Option a -> Option a -> Option a
orElse fallback o = match o { Some v -> Some(v), None -> fallback }

-- Result helpers -------------------------------------------------------------

isOk : Result e a -> Bool
isOk r = match r { Ok v -> True, Err e -> False }

okOr : a -> Result e a -> a
okOr d r = match r { Ok v -> v, Err e -> d }

mapResult : (a -> b) -> Result e a -> Result e b
mapResult f r = match r { Ok v -> Ok(f(v)), Err e -> Err(e) }

-- Actors ---------------------------------------------------------------------

actorPid : ActorRef m -> Pid
actorPid ref = match ref { MkActorRef pid -> pid }

actorSelf : Unit -> ActorRef m
actorSelf _ = MkActorRef(self())

actorReceive : Unit -> m
actorReceive _ = recv()

actorSend : ActorRef m -> m -> Unit
actorSend ref msg = send(actorPid(ref), msg)

actorReply : ActorRef m -> m -> Unit
actorReply ref msg = actorSend(ref, msg)

actorCall : ActorRef m -> (Pid -> m) -> r
actorCall ref make =
  let me = self() in
  let _ = actorSend(ref, make(me)) in
  recv()

actorContinue : s -> { stop : Bool, state : s }
actorContinue state = { stop = False, state = state }

actorStop : s -> { stop : Bool, state : s }
actorStop state = { stop = True, state = state }

actorLoop : (m -> s -> { stop : Bool, state : s }) -> s -> Unit
actorLoop handle state =
  let msg = actorReceive(unit) in
  let next = handle(msg, state) in
  if next.stop then unit else actorLoop(handle, next.state)

-- Database (encrypted, indexed, persistent) ----------------------------------

dbInsert : String -> Json -> String
dbInsert table record = builtin("db.insert@1", table, record)

dbGet : String -> String -> Json
dbGet table id = builtin("db.get@1", table, id)

withDefault : a -> Option a -> a
withDefault d o = match o { Some v -> v, None -> d }

isSome : Option a -> Bool
isSome o = match o { Some v -> True, None -> False }

-- Assumes db.get@1 yields unit when a row is missing; the reference VM uses
-- that sentinel, so this wrapper turns it into a typed Option.
dbGetOpt : String -> String -> Option Json
dbGetOpt table id =
  let r = builtin("db.get@1", table, id) in
  if r == unit then None else Some(r)

dbUpdate : String -> String -> Json -> Bool
dbUpdate table id record = builtin("db.update@1", table, id, record)

dbDelete : String -> String -> Bool
dbDelete table id = builtin("db.delete@1", table, id)

dbQuery : String -> Json -> List Json
dbQuery table filter = builtin("db.query@1", table, filter, {})

dbCreateIndex : String -> String -> Unit
dbCreateIndex table field = builtin("db.createIndex@1", table, field)

dbHash : String -> String
dbHash table = builtin("db.hash@1", table)

-- Cache (fast RAM) -----------------------------------------------------------

cacheSet : String -> String -> Json -> Bool
cacheSet ns key value = builtin("cache.set@1", ns, key, value)

cacheGet : String -> String -> Json
cacheGet ns key = builtin("cache.get@1", ns, key)

cacheDelete : String -> String -> Bool
cacheDelete ns key = builtin("cache.delete@1", ns, key)
"""
