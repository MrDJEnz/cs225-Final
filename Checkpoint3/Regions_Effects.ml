(* Names: Andy, Devin, Duncan, Jacob *)
(* CS225 Final Project *)
(* The following file is test cases to run our program... *)
(* TODO all the draft test cases for now... *)

open Util
open StringSetMap



exception NOT_FOUND

(* ℓ ∈ loc ≈ ℕ
*)
type loc = int
[@@deriving show {with_path = false}]

(* Label Variables
 * p ∈ P ⩴ |label
*)

type p = string
[@@deriving show {with_path = false}]

(* Expressions.
 *
 * e ∈ exp ⩴ true | false | if(e){e}{e}
 * | ⟨e,e⟩ | projl(e) | projr(e)
 * | ref(e) | !e | e ≔ e | e ; e
 * | loc(ℓ) | tag(t,p) | untag(t,p)
*)

type exp =
  | True
  | False
  | If of exp * exp * exp
  | Pair of exp * exp
  | Projl of exp
  | Projr of exp
  | Ref of exp
  | Deref of exp
  | Assign of exp * exp
  | Sequence of exp * exp
  | Loc of loc
  | Tag of exp * p
  | Untag of exp * p
[@@deriving show {with_path = false}]

(* Values.
 * v ∈ value ⩴ true | false
 * | ⟨v,v⟩
 * | loc(ℓ)
 * | Tag(v,p)
*)
type value =
  | VTrue
  | VFalse
  | VPair of value * value
  | VLoc of loc
  | VTag of value * p
[@@deriving show {with_path = false}]

let rec exp_of_val (v : value) : exp = match v with
  | VTrue -> True
  | VFalse -> False
  | VPair(v1,v2) -> Pair(exp_of_val v1,exp_of_val v2)
  | VLoc(l) -> Loc(l)
  | VTag(v,p) -> Tag(exp_of_val v, p)

type store = (loc * value) list
[@@deriving show {with_path = false}]

type stringty = (string * value) list
[@@deriving show {with_path = false}]

(* store_lookup l s = s(l) *)
let rec store_lookup (l : loc) (s : store) : value = match s with
  | [] -> raise NOT_FOUND
  | (l',v) :: s' -> if l = l' then v else store_lookup l s'

  let rec string_lookupty (t : string) (st : stringty) : value = match st with
    | [] -> raise NOT_FOUND
    | (t',v) :: st' -> if t = t' then v else string_lookupty t st'

(* store_update l v s = s[l↦v] *)
let rec store_update (l : loc) (v : value) (s : store) : store = match s with
  | [] -> (l,v) :: []
  | (l',v') :: s' -> if l = l' then (l,v) :: s' else (l',v') :: store_update l v s'

(* store_max_loc s = ⨆{ℓ | ℓ ∈ dom(s)} *)
let rec store_max_loc (s : store) : int = match s with
  | [] -> 0
  | (l,_) :: s' -> max l (store_max_loc s')

(* store_fresh s = ℓ where ℓ ∉ dom(s) *)
let rec store_fresh (s : store) : int =
  let l = store_max_loc s in
  l + 1

(* r ∈ result ⩴ v | ⟨e,s⟩ | stuck
*)
type result =
  | Val of value
  | Step of exp * store *stringty
  | Stuck
[@@deriving show {with_path = false}]

let rec step (e0 : exp) (s : store) (st: stringty) : result = match e0 with
  | True -> Val(VTrue)
  | False -> Val(VFalse)
  | If(e1,e2,e3) -> begin match step e1 s st with
      | Val(VTrue) -> Step(e2,s, st)
      | Val(VFalse) -> Step(e3,s, st)
      | Val(_) -> Stuck
      | Step(e1',s',st') -> Step(If(e1',e2,e3),s',st')
      | Stuck -> Stuck
    end
  | Pair(e1,e2) -> begin match step e1 s st with
      | Val(v1) -> begin match step e2 s st with
          | Val(v2) -> Val(VPair(v1,v2))
          | Step(e2',s',st') -> Step(Pair(e1,e2'),s',st')
          | Stuck -> Stuck
        end
      | Step(e1',s',st') -> Step(Pair(e1',e2),s',st')
      | Stuck -> Stuck
    end
  | Projl(e1) -> begin match step e1 s st with
      | Val(VPair(v1,v2)) -> Step(exp_of_val(v1),s,st)
      | Val(_) -> Stuck
      | Step(e1',s',st') -> Step(Projl(e1'),s',st')
      | Stuck -> Stuck
    end
  | Projr(e1) -> begin match step e1 s st with
      | Val(VPair(v1,v2)) -> Step(exp_of_val(v2),s,st)
      | Val(_) -> Stuck
      | Step(e1',s',st') -> Step(Projr(e1'),s',st')
      | Stuck -> Stuck
    end
  | Ref(e1) -> begin match step e1 s st with
      | Val(v1) -> Step(Loc(store_fresh s), store_update(store_fresh s) v1 s, st)
      | Step(e1',s',st') -> Step(Ref(e1'),s',st')
      | Stuck -> Stuck
    end
  | Deref(e1) -> begin match step e1 s st with
      | Val(VLoc(l)) -> Step(exp_of_val(store_lookup l s), s,st)
      | Val(_) -> Stuck
      | Step(e1',s',st') -> Step(Deref(e1'),s',st')
      | Stuck -> Stuck
    end
  | Assign(e1,e2) -> begin match step e1 s st with
      | Val(VLoc(l1)) -> begin match step e2 s st with
          | Val(v2) -> Step(True, store_update l1 v2 s, st)
          | Step(e2',s', st') -> Step(Assign(Loc(l1), e2'), s', st')
          | Stuck -> Stuck
        end
      | Val(_) -> Stuck
      | Step(e1',s', st') -> Step(Assign(e1', e2),s', st')
      | Stuck -> Stuck
    end
  | Sequence(e1,e2) -> begin match step e1 s st with
      | Val(_) -> Step(e2, s, st)
      | Step(e1',s', st') -> Step(Sequence(e1' ,e2), s', st')
      | Stuck -> Stuck
    end
  | Loc(l) -> Val(VLoc(l))

  | Tag(e,p) -> begin match step e s st with
      |Val(VTag(v1,p)) -> Step(Tag(e,p),s, st)
      |Val(v1) -> Step(Tag(e,p), s, st)
      |Step(e',s',st') -> Step(Tag(e',p),s', st')
      |Stuck -> Stuck
    end

  | Untag(e,p) -> begin match step e s st with
      | Val(VTag(v1,p)) -> Step(exp_of_val(string_lookupty p st), s, st)
      | Val(_) -> Stuck
      | Step(e1',s', st') -> Step(Untag(e1',p),s',st')
      | Stuck -> Stuck
end

(* The reflexive transitive closure of the small-step semantics relation *)
let rec step_star (e : exp) (s : store) (st: stringty): exp * store * stringty  = match step e s st with
  | Val(v) -> (exp_of_val v,s,st)
  | Step(e',s', st') -> step_star e' s' st'
  | Stuck -> (e,s,st)

(* Types.
 *
 * τ ∈ ty ⩴ bool
 * | τ × τ
 * | ref(τ)
*)
type ty =
  | Bool
  | Prod of ty * ty
  | Ref of ty
  | Tagty of p
  | TagtyErr
[@@deriving show {with_path = false}]

type store_ty = (loc * ty) list
[@@deriving show {with_path = false}]

type string_ty = (string * ty) list
[@@deriving show {with_path = false}]

let rec store_ty_lookup (l : loc) (st : store_ty) : ty = match st with
  | [] -> raise NOT_FOUND
  | (l',t) :: st' -> if l = l' then t else store_ty_lookup l st'

let rec string_ty_lookup (p : string) (str : string_ty) : ty = match str with
  | [] -> raise NOT_FOUND
  | (p', t) :: str' -> if p = p' then t else string_ty_lookup p str'

exception TYPE_ERROR

let rec infer (e : exp) (st : store_ty)(str : string_ty): ty = match e with
  | True -> Bool
  | False -> Bool
  | If(e1,e2,e3) ->
    let t1 = infer e1 st str in
    let t2 = infer e2 st str in
    let t3 = infer e3 st str in
    if not (t1 = Bool) then raise TYPE_ERROR else
    if not (t2 = t3) then raise TYPE_ERROR else
      t2
  | Pair(e1,e2) ->
    let t1 = infer e1 st str in
    let t2 = infer e2 st str in
    Prod(t1,t2)
  | Projl(e1) ->
    let t1 = infer e1 st str in
    begin match t1 with
      | Prod(t11,_) -> t11
      | _ -> raise TYPE_ERROR
    end
  | Projr(e1) ->
    let t1 = infer e1 st str in
    begin match t1 with
      | Prod(_,t12) -> t12
      | _ -> raise TYPE_ERROR
    end
  | Ref(e1) ->
    let t1 = infer e1 st str in
    Ref(t1)
  | Deref(e1) ->
    let t1 = infer e1 st str in
    begin match t1 with
      |Ref(t11) -> t11
      |_ -> raise TYPE_ERROR
    end

  | Assign(e1,e2) ->
    let t1 = Deref(e1) in
    let t11 = infer t1 st str  in
    let t2 = infer e2 st str in
    if not (t11 = t2) then raise TYPE_ERROR else
      Bool

  | Sequence(e1,e2) ->
    let t2 = infer e2 st str in
    t2
  | Loc(l) ->
    let t1 = store_ty_lookup l st  in
    Ref(t1)

  | Tag(e,p) ->
    Tagty(p)

  | Untag(e,p) ->
    let t1 = infer e st str in
    if not (t1 = Tagty(p)) then TagtyErr else
      Tagty(p)



let step_tests : test_block =
  let s1 : store = [(1,VTrue);(2,VFalse)] in
  let s2 : store = [(1,VTrue);(2,VTrue)] in
  let st : stringty = [("Test",VTag(VTrue,"test"))] in
  TestBlock
    ( "STEP"
    , [ (True,s1) , Val(VTrue)
      ; (False,s1) , Val(VFalse)
      ; (If(True,False,True),s1) , Step(False,s1,st)
      ; (If(False,False,True),s1) , Step(True,s1,st)
      ; (If(Pair(True,False),True,False),s1) , Stuck
      ; (If(Assign(Loc(2),True),False,True),s1) , Step(If(True,False,True),s2,st)
      ; (If(Deref(True),True,False),s1) , Stuck
      ; (Pair(True,False),s1) , Val(VPair(VTrue,VFalse))
      ; (Pair(Assign(Loc(2),True),True),s1) , Step(Pair(True,True),s2,st)
      ; (Pair(True,Assign(Loc(2),True)),s1) , Step(Pair(True,True),s2, st)
      ; (Pair(Deref(True),True),s1) , Stuck
      ; (Pair(True,Deref(True)),s1) , Stuck
      ; (Projl(Pair(True,False)),s1) , Step(True,s1, st)
      ; (Projl(True),s1) , Stuck
      ; (Projl(If(True,Pair(True,False),Pair(False,True))),s1) , Step(Projl(Pair(True,False)),s1, st)
      ; (Projl(Deref(True)),s1) , Stuck
      ; (Projr(Pair(True,False)),s1) , Step(False,s1, st)
      ; (Projr(True),s1) , Stuck
      ; (Projr(If(True,Pair(True,False),Pair(False,True))),s1) , Step(Projr(Pair(True,False)),s1, st)
      ; (Projr(Deref(True)),s1) , Stuck
      ; (Ref(True),s1) , Step(Loc(3),s1 @ [(3,VTrue)], st)
      ; (Ref(Assign(Loc(2),True)),s1) , Step(Ref(True),s2, st)
      ; (Ref(Deref(True)),s1) , Stuck
      ; (Deref(Loc(2)),s1) , Step(False,s1, st)
      ; (Deref(True),s1) , Stuck
      ; (Deref(If(True,Loc(1),Loc(2))),s1) , Step(Deref(Loc(1)),s1, st)
      ; (Deref(Deref(True)),s1) , Stuck
      ; (Assign(Loc(2),True),s1) , Step(True,s2, st)
      ; (Assign(True,False),s1) , Stuck
      ; (Assign(If(False,Loc(1),Loc(2)),True),s1) , Step(Assign(Loc(2),True),s1, st)
      ; (Assign(Loc(1),Assign(Loc(2),True)),s1) , Step(Assign(Loc(1),True),s2, st)
      ; (Assign(Deref(False),True),s1) , Stuck
      ; (Assign(Loc(1),Deref(False)),s1) , Stuck
      ; (Sequence(True,False),s1) , Step(False,s1, st)
      ; (Sequence(Assign(Loc(2),True),Deref(Loc(2))),s1) , Step(Sequence(True,Deref(Loc(2))),s2, st)
      ; (Sequence(Deref(True),False),s1) , Stuck
      (*WRITING THE NEW TEST CASES*)
      ; (Tag(Sequence(True,False),"Tagged"),s1) , Step(Tag(False,"Tagged"),s1, st)
      ; (Tag(True,"true"),s1), Step(Tag(True, "true"), s1,st)
(*THIS TEST BELOW SHOULD FAIL FOR DEBUG*)
      (*; (Tag(True,"true"),s1), Step(Tag(True, "false"), s1)*)
      ; (Untag(Tag(True,"Tagged"),"Tagged"),s1) , Step(True, s1,st)
      ; (Untag(Untag(True,"true"),"true"),s1), Stuck
      ]
    , (fun (e,s) -> step e s st)
    , [%show : exp * store]
    , [%show : result]
    )

let infer_tests =
  let st : store_ty = [(1,Bool);(2,Prod(Bool,Bool));(3,Ref(Bool));(4,Tagty("true"))] in
  let str : string_ty = [("true",Tagty("true")); ("false", Tagty("false"))] in
  TestBlock
    ( "INFER"
    , [ True , Bool
      ; False , Bool
      ; If(True,False,True) , Bool
      ; If(True,Pair(True,Ref(False)),Pair(False,Ref(True))) , Prod(Bool,Ref(Bool))
      ; Projl(Pair(True,Ref(False))) , Bool
      ; Projr(Pair(True,Ref(False))) , Ref(Bool)
      ; Ref(True) , Ref(Bool)
      ; Ref(Loc(1)) , Ref(Ref(Bool))
      ; Deref(Loc(1)) , Bool
      ; Deref(Loc(2)) , Prod(Bool,Bool)
      ; Deref(Deref(Loc(3))) , Bool
      ; Assign(Loc(1),False) , Bool
      ; Assign(Loc(2),Pair(True,False)) , Bool
      ; Sequence(Assign(Loc(1),False),Ref(True)) , Ref(Bool)
(*WRITING THE NEW TEST CASES*)
      ; Tag(True, "true"),                Tagty("true")
      ; Tag(False, "false"),               Tagty("false")
      ; Untag(Tag(True, "true"), "true"),    Tagty("true")
      ; Untag(Tag(True, "true"), "false"),   TagtyErr
      ]
    , (fun e -> infer e st str)
    , (fun e -> [%show : exp * store_ty] (e,st))
    , [%show : ty]
    )

let _ =
  _SHOW_PASSED_TESTS := false ;
  run_tests [step_tests;infer_tests]



(* Names: Andy, Devin, Duncan, Jacob*)
