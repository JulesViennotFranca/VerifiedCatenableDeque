open Datatypes
open GYOR
open Buffer

type nkind =
| Coq_only
| Coq_left
| Coq_right

type 'a prefix' = 'a t

type 'a suffix' = 'a t

type coloring =
| Gc of nat * nat * nat
| Yc of nat * nat * nat
| Oc of nat * nat * nat
| Rc of nat * nat * nat
| Ec of nat * nat

type 'a node' =
| Only_end of nat * 'a prefix'
| Only of nat * nat * nat * color * coloring * 'a prefix' * 'a suffix'
| Left of nat * nat * nat * color * coloring * 'a prefix' * 'a suffix'
| Right of nat * nat * nat * color * coloring * 'a prefix' * 'a suffix'

type regularity =
| G of nat * color * color
| Y of nat * color * color
| OS of color
| OP of color
| R of nat

type 'a stored_triple =
| Ground of 'a
| Small of nat * nat * 'a stored_triple suffix'
| Big of nat * nat * nat * nat * color * color * 'a stored_triple prefix'
   * 'a chain * 'a stored_triple suffix'
and 'a body =
| Hole of nat * nkind
| Single_child of nat * nat * nkind * nkind * yellow_hue * orange_hue
   * 'a stored_triple node' * 'a body
| Pair_yellow of nat * nat * nkind * nkind * color * 'a stored_triple node'
   * 'a body * 'a chain
| Pair_orange of nat * nat * nkind * nkind * 'a stored_triple node'
   * 'a chain * 'a body
and 'a packet =
| Packet of nat * nat * nat * nkind * nkind * green_hue * red_hue * 'a body
   * 'a stored_triple node'
and 'a chain =
| Empty of nat
| Single of nat * nat * nat * nkind * color * color * color * regularity
   * 'a packet * 'a chain
| Pair of nat * color * color * 'a chain * 'a chain

type 'a prefix = 'a stored_triple prefix'

type 'a suffix = 'a stored_triple suffix'

type 'a node = 'a stored_triple node'

type 'a green_buffer =
| Gbuf of nat * 'a stored_triple t

type 'a stored_buffer =
| Sbuf of nat * 'a stored_triple t

type 'x triple =
| Triple of nat * nat * nkind * color * color * color * color * regularity
   * 'x node * 'x chain

type 'x left_right_triple =
| Not_enough of nat * nkind * 'x stored_triple vector
| Ok_lrt of nat * nkind * color * 'x triple

type 'a six_stored_triple =
  ((((('a stored_triple, 'a stored_triple) prod, 'a stored_triple) prod, 'a
  stored_triple) prod, 'a stored_triple) prod, 'a stored_triple) prod

type 'x partial_triple =
| Zero_element of nat * nkind
| Six_elements of nat * nkind * 'x six_stored_triple
| Ok_pt of nat * nat * nkind * color * 'x triple

type ('x0, 'x) sandwich =
| Alone of 'x0
| Sandwich of 'x0 * 'x * 'x0

type 'x semi_cadeque =
| Semi of nat * nat * color * color * 'x chain

type 'x cadeque =
| T of nat * 'x chain
