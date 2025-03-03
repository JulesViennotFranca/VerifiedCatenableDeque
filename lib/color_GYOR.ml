(* A group of distinct types encode the presence or absence of a hue. *)
type somegreen  = SOME_GREEN
type nogreen    = NO_GREEN
type someyellow = SOME_YELLOW
type noyellow   = NO_YELLOW
type someorange = SOME_ORANGE
type noorange   = NO_ORANGE
type somered    = SOME_RED
type nored      = NO_RED
(* A color is a quadruple of four hue bits. *)
type green      =  somegreen *   noyellow *   noorange *   nored
type yellow     =    nogreen * someyellow *   noorange *   nored
type orange     =    nogreen *   noyellow * someorange *   nored
type red        =    nogreen *   noyellow *   noorange * somered
