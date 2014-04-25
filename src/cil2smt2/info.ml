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

type t = {lineno:    int;
            func_name: string;
            args:      string list;
            info:      string list}

let print out {lineno = lineno;
               func_name = func_name;
               args = args;
               info = info}
  = begin
    String.print out "{";
    Int.print out lineno;
    String.print out ":";
    String.print out func_name;
    String.print out "(";
    String.print out (String.join "," args);
    String.print out ") = ";
    String.print out (String.join "," info);
    String.print out "}";
  end

let split_and_trim ?by:(sep=" ") (s : string) : string list =
  let elems = String.nsplit s ~by:sep in
  let trimmed_elems = List.map String.trim elems in
  trimmed_elems

let of_string (s : string) : t =
  let elems = String.nsplit s ~by:"|" in
  let lookup = List.at elems in
  let lineno = Int.of_string (lookup 0) in
  let fn     = lookup 1 in
  let args   = split_and_trim (lookup 2) in
  let info   = split_and_trim (lookup 3) in
  {lineno = lineno;
   func_name = fn;
   args = args;
   info = info;}
