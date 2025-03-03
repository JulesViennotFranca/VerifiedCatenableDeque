(* Defining some hues. *)
type somegreen  = SOME_GREEN
type nogreen    = NO_GREEN

type someyellow = SOME_YELLOW
type noyellow   = NO_YELLOW

type somered    = SOME_RED
type nored      = NO_RED

(* Defining colors. *)
type green  = somegreen * noyellow   * nored
type yellow = nogreen   * someyellow * nored
type red    = nogreen   * noyellow   * somered
type uncolored = nogreen * noyellow * nored
