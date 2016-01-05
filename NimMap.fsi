module NimMap

type NimMap

val restartMap : 'a -> Map<string,'a>
val update : 'a -> 'b -> Map<'a,'b> -> Map<'a,'b>
