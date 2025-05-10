(* -------------------------------------------------------------------------- *)

(* There are four colors. *)

Inductive color := Red | Orange | Yellow | Green.

(* -------------------------------------------------------------------------- *)

(* A tree consists of a root node, which carries a color, and a child
   forest. A forest consists of zero, one, or two trees. *)

Inductive tree :=
  Root (c : color) (f : forest)

with forest :=
| Empty
| Single (t : tree)
| Pair (l : tree) (r : tree).

(* -------------------------------------------------------------------------- *)

(* In a well-formed tree / forest, only a green node can have zero children. *)

Inductive wf_tree : tree -> Prop :=
| WfTree :
    forall c f,
    (f = Empty -> c = Green) ->
    wf_forest f ->
    wf_tree (Root c f)

with wf_forest : forest -> Prop :=
| WfEmpty :
    wf_forest Empty
| WfSingle :
    forall t,
    wf_tree t ->
    wf_forest (Single t)
| WfPair :
    forall l r,
    wf_tree l ->
    wf_tree r ->
    wf_forest (Pair l r)
.

Hint Constructors wf_tree wf_forest : wf.

Local Ltac destruct_wf_tree :=
  match goal with h: wf_tree _ |- _ => inversion h; subst end.

Local Ltac destruct_wf_forest :=
  match goal with h: wf_forest _ |- _ => inversion h; subst end.

(* -------------------------------------------------------------------------- *)

(* A tree / forest is decomposed into a set of packets as follows.
   The shape of each packet is dictated by the colors of the nodes. *)

(* TODO possibly rename isPacket and isPartitioned *)
(* TODO can we prove that the decomposition is unique (determined
   by the colors of the nodes?) *)

Inductive isPacket : tree -> Prop :=

(* A red or green node ends the current packet. Its child forest is
   decomposed into packets. (Thus, the root nodes of the child forest
   form the beginning of new packets.) *)

| RedOrGreenRoot:
    forall c f,
    c = Red \/ c = Green ->
    isPartitioned f ->
    isPacket (Root c f)

(* If an orange or yellow node has a single child,
   then the current packet extends to this child. *)

| OrangeOrYellowUnaryRoot:
    forall c t,
    c = Orange \/ c = Yellow ->
    isPacket t ->           (* continuation of this packet *)
    isPacket (Root c (Single t))

(* If an orange binary node has two children, then its right child is the
   preferred child. If a yellow binary node has two children, then its left
   child is the preferred child.

   In both cases, the current packet extends to the preferred child. The
   non-preferred child is decomposed into packets: in other words, its root
   node forms the beginning of a new packet. *)

| OrangeBinaryRoot:
    forall l r,
    isPartitioned (Single l) -> (* start of hanging packet *)
    isPacket r ->           (* continuation of this packet *)
    isPacket (Root Orange (Pair l r))

| YellowBinaryRoot:
    forall l r,
    isPacket l ->           (* continuation of this packet *)
    isPartitioned (Single r) -> (* start of hanging packet *)
    isPacket (Root Yellow (Pair l r))


with isPartitioned : forest -> Prop :=

| PartitionedEmpty:
    isPartitioned Empty

| PartitionedSingle:
    forall t,
    isPacket t ->                  (* start of root packet *)
    isPartitioned (Single t)

| PartitionedPair:
    forall l r,
    isPacket l ->             (* start of left root packet *)
    isPacket r ->            (* start of right root packet *)
    isPartitioned (Pair l r)

.

Hint Constructors isPacket isPartitioned : decompose.

(* -------------------------------------------------------------------------- *)

(* A well-formed tree / forest can always be decomposed into packets
   in the manner described above. *)

Lemma isPacket_universal :
  forall t,
  wf_tree t ->
  isPacket t

with isPartitioned_universal :
  forall f,
  wf_forest f ->
  isPartitioned f.

Local Ltac fabada :=
  match goal with h: ?x = ?x -> _ |- _ => specialize (h eq_refl) end.
Proof.
  (* Trees. *)
  induction 1; intros.
  destruct c.
  (* Red node. *)
  { eauto with decompose. }
  (* Orange node. *)
  { destruct_wf_forest.
    + exfalso. fabada. congruence.
    + eauto with decompose.
    + eauto with decompose. }
  (* Yellow node. *)
  { destruct_wf_forest.
    + exfalso. fabada. congruence.
    + eauto with decompose.
    + eauto with decompose. }
  (* Green node. *)
  { eauto with decompose. }

  (* Forests. *)
  induction 1; eauto with decompose.
Qed.

(* -------------------------------------------------------------------------- *)

(* We now assign colors to packets,
   and we impose certain regularity constraints. *)

(* The color of a packet is the color of the red or green node that is found
   at the bottom of this packet. (Thus, the color of a packet must be red or
   green.) *)

(* [isValidPacket] carries a color, which reflects the color of this packet.
   [isValidPacketC] carries a color constraint, which applies to this packet.
   [isValidPartitioned] also carries a color constraint, which applies to the
   (zero, one, or two) trees in this forest. *)

Inductive color_constraint := MustBeGreen | AnyColor.

Definition satisfies (c : color) (cc : color_constraint) :=
  match cc with
  | MustBeGreen => c = Green
  | AnyColor    => True
  end.

Inductive isValidPacket : tree -> color -> Prop :=

(* A green node ends the current packet,
   and makes the current packet green.
   No color constraint bears on the following packets. *)

(* A red node ends the current packet,
   and makes the current packet red.
   The following packets must be green. *)

| VRedOrGreenRoot:
    forall c f cc,
    c = Red \/ c = Green ->
    (c = Red -> cc = MustBeGreen) ->
    isValidPartitioned f cc ->
    isValidPacket (Root c f) c

(* If an orange or yellow node has a single child,
   then the current packet extends to this child.

   The color of the current packet is the color of the sub-packet;
   in other words, it is the color of the (green or red) node that
   is found at the bottom of this packet. *)

| VOrangeOrYellowUnaryRoot:
    forall c c' t,
    c = Orange \/ c = Yellow ->
    isValidPacket t c' ->
    isValidPacket (Root c (Single t)) c'

(* If an orange or yellow node has two children,
   then the current packet extends to the preferred child.

   The color of the current packet is the color of the sub-packet;
   in other words, it is the color of the (green or red) node that
   is found at the bottom of this packet.

   A yellow node imposes no color constraint on the non-preferred child.
   An orange node requires the non-preferred child to be green. *)

| VOrangeBinaryRoot:
    forall c l r,
    isValidPartitioned (Single l) MustBeGreen ->
    isValidPacket r c ->
    isValidPacket (Root Orange (Pair l r)) c

| VYellowBinaryRoot:
    forall c l r,
    isValidPacket l c ->
    isValidPartitioned (Single r) AnyColor ->
    isValidPacket (Root Yellow (Pair l r)) c

with isValidPacketC : tree -> color_constraint -> Prop :=

| ValidPacketC :
    forall t c cc,
    isValidPacket t c ->
    satisfies c cc ->
    isValidPacketC t cc

with isValidPartitioned : forest -> color_constraint -> Prop :=

| ValidEmpty:
    forall cc,
    isValidPartitioned Empty cc

| ValidSingle:
    forall t cc,
    isValidPacketC t cc ->
    isValidPartitioned (Single t) cc

| ValidPair:
    forall l r cc,
    isValidPacketC l cc ->
    isValidPacketC r cc ->
    isValidPartitioned (Pair l r) cc

.

Hint Constructors isValidPacket isValidPacketC isValidPartitioned : valid.

(* -------------------------------------------------------------------------- *)

(* During to the coloring constraints that it imposes, [isValidPacket] is
   stronger than [isPacket]. Similarly, [isValidPartitioned] is stronger
   than [isPartitioned]. *)

Lemma isValidPacket_isPacket :
  forall t c,
  isValidPacket t c ->
  isPacket t

with isValidPacketC_isPacket :
  forall t cc,
  isValidPacketC t cc ->
  isPacket t

with isValidPartitioned_isPartitioned :
  forall f cc,
  isValidPartitioned f cc ->
  isPartitioned f.

Proof.
  { induction 1; eauto with decompose. }
  { induction 1; eauto. }
  { induction 1; eauto with decompose. }
Qed.

(* -------------------------------------------------------------------------- *)

(* TODO comment *)

Definition arity := nat.

(* A node carries a color and an arity. *)

(* Its arity must be 0, 1, or 2. *)

(* Only a green node can have arity 0. *)

Inductive node : color -> arity -> Type :=
| Node :
    forall (c : color) (a : arity),
    a <= 2 ->
    (a = 0 -> c = Green) ->
    node c a.

Inductive body : Type :=
| Hole :
    body
| Single_child :
    forall {c},
    c = Orange \/ c = Yellow ->
    node c 1 ->
    body ->     (* continuation of the body *)
    body
| Pair_orange :
    node Orange 2 ->
    chain 1 MustBeGreen -> (* left: a hanging chain (must be green) *)
    body ->     (* right: the continuation of the body *)
    body
| Pair_yellow :
    node Yellow 2 ->
    body ->     (* left: the continuation of the body *)
    chain 1 AnyColor ->       (* right: a hanging chain *)
    body

with packet : color -> arity -> Type :=
  | Packet :
      forall {c a},
      body ->             (* body of this packet *)
      c = Red \/ c = Green ->
      node c a -> (* tail node of this packet (must be green or red) *)
      packet c a

with chain : nat -> color_constraint -> Type :=

  | EmptyChain :
      forall {cc},
      chain 0 cc

  | SingleChain :
      forall {c a cc cc'},
      (* color of chain is color of top packet; must satisfy [cc] *)
      satisfies c cc ->
      packet c a ->
      (* red packet must have green children *)
      (* chain arity must match number of descendants of tail node *)
      (c = Red -> cc' = MustBeGreen) ->
      chain a cc' ->
      chain 1 cc

  | PairChain :
      forall {cc},
      chain 1 cc ->
      chain 1 cc ->
      chain 2 cc

.

(* -------------------------------------------------------------------------- *)

(* TODO comment *)

Definition get_color {c a} (n : node c a) : color :=
  c.

From Coq Require Import Program.
From Equations Require Import Equations.

Equations decode_body {c a} (b : body) : node c a -> forest -> tree
by struct b
:=
decode_body Hole tail f :=
  Root (get_color tail) f;
decode_body (Single_child _ head b) tail f :=
  Root (get_color head) (Single (decode_body b tail f));
decode_body (Pair_yellow head b rch) tail f :=
  Root (get_color head) (Pair (decode_body b tail f) (decode_chain1 rch));
decode_body (Pair_orange head lch b) tail f :=
  Root (get_color head) (Pair (decode_chain1 lch) (decode_body b tail f))

with decode_packet {c a} (p : packet c a) : forest -> tree
by struct p
:=
decode_packet (Packet body _ tail) f :=
  decode_body body tail f

with decode_chain1 {cc} (ch : chain 1 cc) : tree
by struct ch
:=
decode_chain1 (SingleChain _ p _ ch) :=
  decode_packet p (decode_chain ch)

with decode_chain {a cc} (ch : chain a cc) : forest
by struct ch
:=
decode_chain EmptyChain :=
  Empty;
decode_chain (SingleChain _ p _ ch) :=
  Single (decode_packet p (decode_chain ch));
decode_chain (PairChain lch rch) :=
  Pair (decode_chain1 lch) (decode_chain1 rch).

(* TODO *)

Lemma decode_chain_decode_chain1 cc (ch : chain 1 cc) :
  decode_chain ch = Single (decode_chain1 ch).
Proof.
  dependent elimination ch. reflexivity.
Qed.

Lemma invert_is_ValidPartitioned_chain1 cc (ch : chain 1 cc) :
  isValidPartitioned (decode_chain ch) cc ->
  isValidPacketC (decode_chain1 ch) cc.
Proof.
  rewrite decode_chain_decode_chain1. inversion 1; subst. tauto.
Qed.

Lemma ok_decode_chain :
  forall a cc (ch : chain a cc),
  isValidPartitioned (decode_chain ch) cc

with ok_decode_packet :
  forall a c cc (p : packet c a) f,
  isValidPartitioned f cc ->
  (c = Red -> cc = MustBeGreen) ->
  isValidPacket (decode_packet p f) c

with ok_decode_body c a b (n : node c a) f cc :
  c = Red \/ c = Green ->
  isValidPartitioned f cc ->
  (c = Red -> cc = MustBeGreen) ->
  isValidPacket (decode_body b n f) c

with ok_decode_chain1 cc (ch : chain 1 cc) :
  isValidPacketC (decode_chain1 ch) cc.

Proof.
  (* Chains. *)
  { induction ch; simpl;
    eauto using invert_is_ValidPartitioned_chain1 with valid. }

  (* Packets. *)
  { intros. destruct p. simpl. eauto. }

  (* Bodies. *)
  { intros. destruct b;
    (* These manual rewrites are ugly, but never mind. *)
    [ rewrite decode_body_equation_1
    | rewrite decode_body_equation_2
    | rewrite decode_body_equation_3
    | rewrite decode_body_equation_4 ];
    eauto with valid. }

  (* Single chains. *)
  { dependent elimination ch.
    rewrite decode_chain1_equation_1.
    eauto with valid. }

Qed.

(* TODO ideally, we should also prove that every well-formed forest
   that satisfies isValidPartitioned can be encoded as a chain
   (that is, there exists a chain whose decoded form is the original forest). *)

(* -------------------------------------------------------------------------- *)

Module A. (* TODO avoid name clash *)

(* On traduit nos définitions des prédicats packet et partitioned avec les
   contraintes de régularité en types.

   Les packets contiennent un body, une suite de node jaunes et oranges, et une
   tail, un node vert ou rouge.

   Les partitioneds sont codés par des chains.

   Je présente ici une version simplifiée du code du git. Quand je parle du code complet, je fais référence au code présent ici : https://github.com/JulesViennotFranca/VerifiedCatenableDeque/blob/main/theory/cadeque/types.v *)

(* La coloration permet ici de lier la couleur d'un node avec son nombre
   d'enfants dans un arbre : comme les feuilles sont vertes, un node jaune,
   orange ou rouge ne peut pas avoir 0 enfant.

   Dans le code complet, la coloration permettra aussi d'indiquer la taille des
   buffers correspondant à chaque couleur. *)

Inductive coloration : nat -> color -> Type :=
  | Gc {n : nat} : coloration n Green
  | Yc {n : nat} : coloration (S n) Yellow
  | Oc {n : nat} : coloration (S n) Orange
  | Rc {n : nat} : coloration (S n) Red.

(* Un node contient ici seulement une coloration.

   Dans le code complet, un node contient aussi des buffers avec des
   contraintes sur leur taille, d'un côté par la coloration, de l'autre par le
   genre du node en question (only, left ou right). *)

Inductive node : nat -> color -> Type :=
  | Simple_node {nbr_child : nat} {c : color} :
    coloration nbr_child c -> node nbr_child c.

(* Le type regularity représente les contraintes 1 et 3 des règles de
   régularité de explication.md.

   La contrainte 2 va être directement codée dans le type body.

   La contrainte 4 va être directement codée dans le type cadeque.

   Dans le code complet, regularity est un [Type] plutôt qu'un [Prop] car on a
   parfois besoin de pattern matcher sur un élément de regularity dans nos
   fonctions. De plus, on a plus de constructeurs car on veut aussi exprimer
   des contraintes de régularité sur un autre type, le type [triple]. Pour
   rendre plus compréhensible ces deux utilisations, on pourrait séparer le
   type [regularity] du code complet en un type [regularity] similaire à celui
   présenté ici, et un type [triple_regularity] pour les contraintes de
   régularité de [triple]. *)

Inductive regularity : color -> color -> color -> Prop :=
  | G {cl cr : color} : regularity Green cl cr
  | R : regularity Red Green Green.

(* Le body représente la suite de nodes jaunes et oranges d'un packet. Un body
   se termine par un hole, qui représente la tail du packet.

   Dans le code complet, il est important de tenir compte du type des éléments
   contenus dans un node, ainsi que du genre des nodes. Les bodies sont donc
   paramétré par le type d'éléments et le genre de leur premier node. Comme la
   tail d'un packet est représentée par le hole d'un body, il faut aussi
   paramétrer les bodies avec le type d'éléments et le genre de leur hole. *)

Inductive body : Type :=
  | Hole : body
  | Single_child {node_color : color} :
    node_color = Yellow \/ node_color = Orange ->
    node 1 node_color ->    (* current node *)
    body ->     (* continuation of the body *)
    body
  | Pair_yellow {cl cr : color} :
    node 2 Yellow ->        (* current node *)
    body ->     (* continuation of the body *)
    chain 1 cl cr -> (* right hanging chain *)
    body
  | Pair_orange :
    node 2 Orange ->        (* current node *)
    chain 1 Green Green -> (* left hanging chain /!\ green /!\ *)
    body ->     (* continuation of the body *)
    body

(* Un packet contient un body et une tail. Le packet hérite du nombre d'enfants
   et de la couleur de sa tail.

   Dans le code complet, un packet est aussi paramétré par le genre de son premier node. Les packets sont de plus paramétré par les types d'éléments contenus dans leur premier node et dans les nodes fils de leur tail. *)

with packet : nat -> color -> Type :=
  | Packet {nbr_child : nat} {c : color} :
    body ->             (* body of the packet *)
    c = Green \/ c = Red ->
    node nbr_child c -> (* tail of the packet *)
    packet nbr_child c

(* Une chain représente une forêt partitionnée en packets. Les contraintes 1 et
   3 de régularité sont exprimées dans le constructeur [SingleChain].

   Dans le code complet, le paramètre de type [arity] représentant le genre de
   la chain est exactement le même que notre paramètre entier ici.

   Dans le code complet, un paramètre de genre de node est ajouté. Pour une
   single chain, il fait sens, c'est le genre du node racine de la single
   chain. Pour la empty chain et les pair chains, il n'y a pas de tel node dont
   on voudrait savoir le genre. On attribut alors à l'empty chain et aux pair
   chains la valeur [only] pour ce paramètre.

   Cette valeur [only] peut sembler arbitraire mais il y a une raison. Dans les
   single chains, les seules qui peuvent représenter à elle seule une cadeque
   ou être une following chain sont celles dont la racine est un only node. Ces
   single chains sont labélisées par [only] pour le paramètre en question.

   Comme l'empty chain et les pair chains peuvent être à elles seules une
   cadeque ou être une following chain, elles sont aussi labélisées par [only].

   Les chains labélisées par [left] ou [right] sont forcément des singles chains, et ne peuvent être que des hanging chains ou des singles chains formant une pair chain. *)

with chain : nat -> color -> color -> Type :=

  | EmptyChain : chain 0 Green Green

  | SingleChain {nbr_tree : nat} {c cl cr : color} :
    regularity c cl cr ->
    packet nbr_tree c ->
    chain nbr_tree cl cr ->
    chain 1 c c

  | PairChain {cl cr : color} :
    chain 1 cl cl ->
    chain 1 cr cr ->
    chain 2 cl cr.

(* Les cadeques sont des forêts de 0, 1 ou 2 arbres. On les code donc avec une
   chain quelconque. La contrainte 4 de régularité impose que cette chain ait
   ses deux couleurs vertes.

   Dans le code complet, comme évoqué plus haut, on précise en plus que cette chain doit être [only] pour pouvoir représenter une cadeque. *)

Inductive cadeque : Type :=
  | Cadeque {n : nat} : chain n Green Green -> cadeque.

End A.
