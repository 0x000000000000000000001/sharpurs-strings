module Data_String_CodeUnits_FFI

open System

let fromCharArray = fun (a: obj) ->
    let arr = unbox<obj[]> a
    let chars = Array.zeroCreate arr.Length
    for i = 0 to arr.Length - 1 do
        chars.[i] <- unbox<char> arr.[i]
    box (new String(chars))

let toCharArray = fun (str: obj) ->
    let s = unbox<string> str
    let arr = Array.zeroCreate s.Length
    for i = 0 to s.Length - 1 do
        arr.[i] <- box s.[i]
    box arr

let singleton = fun (c: obj) ->
    let ch = unbox<char> c
    box (ch.ToString())

let _charAt = fun (just: obj) -> fun (nothing: obj) -> fun (idx: obj) -> fun (str: obj) ->
    let i = unbox<int> idx
    let s = unbox<string> str
    if i >= 0 && i < s.Length then
        sharpurs_apply just (box s.[i])
    else
        nothing

let _toChar = fun (just: obj) -> fun (nothing: obj) -> fun (str: obj) ->
    let s = unbox<string> str
    if s.Length = 1 then
        sharpurs_apply just (box s.[0])
    else
        nothing

let length = fun (str: obj) ->
    let s = unbox<string> str
    box s.Length

let countPrefix = fun (p: obj) -> fun (str: obj) ->
    let s = unbox<string> str
    let mutable i = 0
    while i < s.Length && unbox<bool> (sharpurs_apply p (box s.[i])) do
        i <- i + 1
    box i

let _indexOf = fun (just: obj) -> fun (nothing: obj) -> fun (x: obj) -> fun (s: obj) ->
    let str = unbox<string> s
    let sub = unbox<string> x
    let idx = str.IndexOf(sub)
    if idx = -1 then nothing else sharpurs_apply just (box idx)

let _indexOfStartingAt = fun (just: obj) -> fun (nothing: obj) -> fun (x: obj) -> fun (startIdx: obj) -> fun (str: obj) ->
    let s = unbox<string> str
    let sub = unbox<string> x
    let start = unbox<int> startIdx
    if start < 0 || start > s.Length then nothing
    else
        let idx = s.IndexOf(sub, start)
        if idx = -1 then nothing else sharpurs_apply just (box idx)

let _lastIndexOf = fun (just: obj) -> fun (nothing: obj) -> fun (x: obj) -> fun (s: obj) ->
    let str = unbox<string> s
    let sub = unbox<string> x
    let idx = str.LastIndexOf(sub)
    if idx = -1 then nothing else sharpurs_apply just (box idx)

let _lastIndexOfStartingAt = fun (just: obj) -> fun (nothing: obj) -> fun (x: obj) -> fun (startIdx: obj) -> fun (str: obj) ->
    let s = unbox<string> str
    let sub = unbox<string> x
    let start = unbox<int> startIdx
    let safeStart = max 0 (min s.Length start)
    let endIdx = min s.Length (safeStart + sub.Length)
    let idx = s.Substring(0, endIdx).LastIndexOf(sub)
    if idx = -1 then nothing else sharpurs_apply just (box idx)

let take = fun (idx: obj) -> fun (str: obj) ->
    let i = unbox<int> idx
    let s = unbox<string> str
    let safeI = max 0 (min s.Length i)
    box (s.Substring(0, safeI))

let drop = fun (idx: obj) -> fun (str: obj) ->
    let i = unbox<int> idx
    let s = unbox<string> str
    let safeI = max 0 (min s.Length i)
    box (s.Substring(safeI))

let slice = fun (start: obj) -> fun (end_: obj) -> fun (str: obj) ->
    let s = unbox<string> str
    let mutable st = unbox<int> start
    let mutable en = unbox<int> end_
    if st < 0 then st <- s.Length + st
    if en < 0 then en <- s.Length + en
    st <- max 0 (min s.Length st)
    en <- max 0 (min s.Length en)
    if st > en then box ""
    else box (s.Substring(st, en - st))

let splitAt = fun (idx: obj) -> fun (str: obj) ->
    let s = unbox<string> str
    let i = unbox<int> idx
    let safeI = max 0 (min s.Length i)
    box (Map.ofList [ "before", box (s.Substring(0, safeI)); "after", box (s.Substring(safeI)) ])
