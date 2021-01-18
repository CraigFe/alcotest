open Ppxlib

let return x = Some x
let ( >>= ) x f = match x with None -> None | Some x -> f x

(** Representation of types with corresponding combinators in Alcotest. *)
type typ =
  | Any
  | Custom of string
  | Unit
  | Bool
  | Int
  | Int32
  | Int64
  | Float
  | Char
  | String
  | Bytes
  | List of typ
  | Array of typ
  | Option of typ
  | Result of typ * typ
  | Pair of typ * typ
  | Triple of typ * typ * typ

let combinator_name_of_type_name = function
  | "t" -> "testable"
  | s -> s ^ "_testable"

let comb ~loc : typ -> expression =
  let rec aux = function
    | Custom n -> Ast_builder.Default.evar ~loc (combinator_name_of_type_name n)
    | Any -> [%expr Alcotest.fail]
    | Unit -> [%expr Alcotest.unit]
    | Bool -> [%expr Alcotest.bool]
    | Int -> [%expr Alcotest.int]
    | Int32 -> [%expr Alcotest.int32]
    | Int64 -> [%expr Alcotest.int64]
    | Float -> [%expr Alcotest.float 1. (* TODO *)]
    | Char -> [%expr Alcotest.char]
    | String -> [%expr Alcotest.string]
    | Bytes -> [%expr Alcotest.bytes]
    | List a -> [%expr Alcotest.list [%e aux a]]
    | Array a -> [%expr Alcotest.array [%e aux a]]
    | Option a -> [%expr Alcotest.option [%e aux a]]
    | Result (a, e) -> [%expr Alcotest.result [%e aux a] [%e aux e]]
    | Pair (a, b) -> [%expr Alcotest.pair [%e aux a] [%e aux b]]
    | Triple (a, b, c) ->
        [%expr Alcotest.triple [%e aux a] [%e aux b] [%e aux c]]
  in
  aux

let rec unify a b =
  match (a, b) with
  | Any, x | x, Any -> x
  | (Custom a as t), Custom b -> if String.equal a b then t else assert false
  | Unit, Unit -> Unit
  | Bool, Bool -> Bool
  | Int, Int -> Int
  | Int32, Int32 -> Int32
  | Int64, Int64 -> Int64
  | Float, Float -> Float
  | Char, Char -> Char
  | String, String -> String
  | Bytes, Bytes -> Bytes
  | List a, List b -> List (unify a b)
  | Array a, Array b -> Array (unify a b)
  | Option a, Option b -> Option (unify a b)
  | Result (a1, e1), Result (a2, e2) -> Result (unify a1 a2, unify e1 e2)
  | Pair (a1, b1), Pair (a2, b2) -> Pair (unify a1 a2, unify b1 b2)
  | Triple (a1, b1, c1), Triple (a2, b2, c2) ->
      Triple (unify a1 a2, unify b1 b2, unify c1 c2)
  | ( ( Custom _ | Unit | Bool | Int | Int32 | Int64 | Float | Char | String
      | Bytes | List _ | Array _ | Option _ | Result _ | Pair _ | Triple _ ),
      _ ) ->
      failwith "Unable to unify mismatched types"

(** This module provides a very basic inference of structural types of
    expressions.*)

let lift_constructor c ts =
  match (c, ts) with
  | "list", [ a ] -> List a
  | "result", [ a; e ] -> Result (a, e)
  | "option", [ a ] -> Option a
  | s, _ -> Format.kasprintf failwith "unknown constructor: %s" s

let rec core_type : core_type -> typ option =
 fun t ->
  let x = t.ptyp_desc in
  match x with
  | Ptyp_any | Ptyp_var _ -> return Any
  | Ptyp_tuple [ t1; t2 ] ->
      core_type t1 >>= fun t1 ->
      core_type t2 >>= fun t2 -> return @@ Pair (t1, t2)
  | Ptyp_tuple [ t1; t2; t3 ] ->
      core_type t1 >>= fun t1 ->
      core_type t2 >>= fun t2 ->
      core_type t3 >>= fun t3 -> return @@ Triple (t1, t2, t3)
  | Ptyp_tuple _ -> failwith "tuple"
  | Ptyp_constr (c, []) -> (
      match c.txt with
      | Lident "int" -> return Int
      | Lident "" | _ -> failwith "todo")
  | Ptyp_constr (c, ts) -> (
      let ts =
        ListLabels.filter_map ts ~f:core_type
        (* TODO: ensure no drop *)
      in
      match c.txt with
      | Lident c -> return (lift_constructor c ts)
      | _ -> failwith "lident todo")
  | Ptyp_alias (_, _)
  | Ptyp_variant (_, _, _)
  | Ptyp_poly (_, _)
  | Ptyp_arrow _ | Ptyp_object _ | Ptyp_class _ | Ptyp_package _
  | Ptyp_extension _ ->
      None

let rec expression : expression -> typ option =
 fun e ->
  match e with
  | {
   pexp_desc =
     Pexp_ifthenelse (_, _, None) | Pexp_setfield _ | Pexp_while _ | Pexp_for _;
   _;
  } ->
      return Unit
  | { pexp_desc = Pexp_constant c; _ } -> (
      match c with
      | Pconst_char _ -> return Char
      | Pconst_integer (_, None) -> return Int
      | Pconst_integer (_, Some 'l') -> return Int32
      | Pconst_integer (_, Some 'L') -> return Int64
      | Pconst_integer (_, Some _) -> None
      | Pconst_string _ -> return String
      | Pconst_float (_, _) -> return Float)
  | { pexp_desc = Pexp_tuple t; _ } -> (
      match t with
      | [ e1; e2 ] ->
          expression e1 >>= fun t1 ->
          expression e2 >>= fun t2 -> return (Pair (t1, t2))
      | [ e1; e2; e3 ] ->
          expression e1 >>= fun t1 ->
          expression e2 >>= fun t2 ->
          expression e3 >>= fun t3 -> return (Triple (t1, t2, t3))
      | _ -> failwith "tuple")
  | [%expr None] -> return (Option Any)
  | [%expr Some [%e? x]] -> expression x >>= fun t -> return @@ Option t
  | [%expr Ok [%e? x]] -> expression x >>= fun t -> return @@ Result (t, Any)
  | [%expr Error [%e? x]] -> expression x >>= fun t -> return @@ Result (Any, t)
  | { pexp_desc = Pexp_construct (_, _); _ } -> assert false
  | { pexp_desc = Pexp_array es; _ } ->
      ListLabels.fold_left es ~init:None ~f:(fun acc x ->
          match acc with Some _ as t -> t | None -> expression x)
  | { pexp_desc = Pexp_ifthenelse (_, e1, Some e2); _ } -> (
      match (expression e1, expression e2) with
      | None, None -> None
      | (Some _ as t), None | None, (Some _ as t) -> t
      | Some x, Some y -> return (unify x y))
  | { pexp_desc = Pexp_constraint (_, t) | Pexp_coerce (_, _, t); _ } ->
      core_type t
  | {
   pexp_desc =
     ( Pexp_let (_, _, e)
     | Pexp_letmodule (_, _, e)
     | Pexp_letexception (_, e)
     | Pexp_open (_, e)
     | Pexp_sequence (_, e) );
   _;
  } ->
      expression e
  (* Expressions for which we make no attempt at inference *)
  | {
   pexp_desc =
     ( Pexp_apply _ | Pexp_assert _ | Pexp_extension _
     | Pexp_field (_, _)
     | Pexp_fun _ | Pexp_function _ | Pexp_ident _ | Pexp_lazy _ | Pexp_letop _
     | Pexp_match _ | Pexp_new _ | Pexp_newtype _ | Pexp_object _
     | Pexp_override _ | Pexp_pack _ | Pexp_poly _ | Pexp_record _ | Pexp_send _
     | Pexp_setinstvar _ | Pexp_try _ | Pexp_unreachable | Pexp_variant _ );
   _;
  } ->
      None
