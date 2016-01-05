module NimMap

type NimMap

val make        : int -> NimMap
val update      : string -> int -> NimMap -> NimMap
val find        : string -> NimMap -> int
val getM        : NimMap -> int
val findKey     : int -> NimMap -> string
val findMax     : NimMap -> string * int
val getMap      : NimMap -> Map<string,int>
val win         : NimMap -> bool
val calcOptimal : NimMap -> string * int
