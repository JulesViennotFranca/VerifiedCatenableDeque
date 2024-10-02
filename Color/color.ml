module GYR = struct
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
end

module GYOR = struct
  (* Defining some hues. *)
  type somegreen  = SOME_GREEN
  type nogreen    = NO_GREEN

  type someyellow = SOME_YELLOW
  type noyellow   = NO_YELLOW

  type someorange = SOME_ORANGE
  type noorange   = NO_ORANGE

  type somered    = SOME_RED
  type nored      = NO_RED

  (* Defining colors. *)
  type green  = somegreen * noyellow   * noorange   * nored
  type yellow = nogreen   * someyellow * noorange   * nored
  type orange = nogreen   * noyellow   * someorange * nored
  type red    = nogreen   * noyellow   * noorange   * somered
  type uncolored = nogreen * noyellow * noorange * nored
end