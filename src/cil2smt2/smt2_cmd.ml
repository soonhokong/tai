(*
Author: Soonho Kong   <soonhok@cs.cmu.edu>
        Wei Chen      <weichen1@andrew.cmu.edu>
        Sicun Gao     <sicung@cs.cmu.edu>
        Edmund Clarke <emc@cs.cmu.edu>

dReal -- Copyright (C) 2013 - 2014, Soonho Kong, Wei Chen, Sicun Gao, and Edmund Clarke

dReal is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

dReal is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with dReal. If not, see <http://www.gnu.org/licenses/>.
*)

open Batteries
open Basic

type logic = | QF_NRA
             | QF_NRA_ODE

type exp = Basic.exp
type formula = Basic.formula

type t = | SetLogic of logic
         | SetInfo of string * string
         | DeclareFun of string
         | DeclareConst of string
         (** ode group X LHS X RHS **)
         (** [x1_k_t ... xn_k_t] = (integral 0.0 time_k [x1_k_0 ... xn_k_0] flow_i) *)
         | DefineODE of string * (string * exp) list
         | Assert of formula
         | CheckSAT
         | Exit

let make_lb (name : string) (v : float)
    = Assert (Basic.Le (Basic.Num v,  Basic.Var name))

let make_ub (name : string) (v : float)
    = Assert (Basic.Le (Basic.Var name, Basic.Num v ))

let make_lbp (name : string) (v : float) (precision : float)
    = Assert (Basic.Lep (Basic.Num v,  Basic.Var name, precision))

let make_ubp (name : string) (v : float) (precision : float)
    = Assert (Basic.Lep (Basic.Var name, Basic.Num v, precision ))

let set_precision (p : float) : t =
  SetInfo (":precision", string_of_float p)

let print_logic out =
  function
  | QF_NRA -> String.print out "QF_NRA"
  | QF_NRA_ODE -> String.print out "QF_NRA_ODE"

let print out =
  function
  | SetLogic l ->
    Printf.fprintf out "(set-logic %s)" (IO.to_string print_logic l)
  | SetInfo (key, value) ->
    Printf.fprintf out "(set-info %s %s)" key value
  | DeclareFun v ->
    Printf.fprintf out "(declare-fun %s () Real)" v
  | DeclareConst v ->
    Printf.fprintf out "(declare-const %s Real)" v
  | DefineODE (g, eqs) ->
     let print_eq out (x, e) = Printf.fprintf out "(= d/dt[%s] %s)" x (IO.to_string Basic.print_exp e) in
     let str_eqs = IO.to_string (List.print ~first:"(" ~last:")" ~sep:" " print_eq) eqs in
     List.print ~first:"(define-ode " ~last:")" ~sep:" " String.print out [g; str_eqs]
  | Assert f ->
    Printf.fprintf out "(assert %s)" (IO.to_string Basic.print_formula f)
  | CheckSAT ->
    String.print out "(check-sat)"
  | Exit ->
    String.print out "(exit)"
