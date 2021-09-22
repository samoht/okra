(*
 * Copyright (c) 2021 Magnus Skjegstad <magnus@skjegstad.com>
 * Copyright (c) 2021 Thomas Gazagnaire <thomas@gazagnaire.org>
 * Copyright (c) 2021 Patrick Ferris <pf341@patricoferris.com>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

type entry = {
  counter : int;
  project : string;
  objective : string;
  kr_title : string;
  kr_id : string;
  time_entries : string list;
  time_per_engineer : (string, float) Hashtbl.t;
  work : Item.t list list;
}

type t = entry list

let compare a b =
  if String.compare a.project b.project = 0 then
    (* compare on project first *)
    if String.compare a.objective b.objective = 0 then
      (* then obj if proj equal *)
      (* Check if KR IDs are the same --if one of the KR IDs are
         blank, compare on title instead *)
      let compare_kr_id =
        if
          a.kr_id = ""
          || b.kr_id = ""
          || a.kr_id = "NEW KR"
          || b.kr_id = "NEW KR"
          || a.kr_id = "NEW OKR"
          || b.kr_id = "NEW OKR"
        then String.compare a.kr_title b.kr_title
        else String.compare a.kr_id b.kr_id
      in
      (* If KRs match, check counter *)
      if compare_kr_id = 0 then compare a.counter b.counter else compare_kr_id
    else String.compare a.objective b.objective
  else String.compare a.project b.project

let make_days d =
  let d = floor (d *. 2.0) /. 2. in
  if d = 1. then
    "1 day"
  else if classify_float (fst (modf d)) = FP_zero then
    Printf.sprintf "%.0f days" d
  else
    Printf.sprintf "%.1f days" d

let make_engineer ~time (e, d) =
  if time then
    Printf.sprintf "@%s (%s)" e (make_days d)
  else
    Printf.sprintf "@%s" e

let make_engineers ~time entries =
  let entries = List.of_seq (Hashtbl.to_seq entries) in
  let entries = List.sort (fun (x, _) (y, _) -> String.compare x y) entries in
  let engineers = List.rev_map (make_engineer ~time) entries in
  match engineers with
  | [] -> []
  | e :: es ->
    let open Item in
    let lst =
      List.fold_left (fun acc engineer -> Text engineer :: Text ", " :: acc)
        [Text e] es
    in
    [ Paragraph (Concat lst) ]

let pp ?(include_krs = []) ?(show_time = true) ?(show_time_calc = true)
    ?(show_engineers = true) okrs =
  let uppercase_include_krs = List.map String.uppercase_ascii include_krs in
  let c_project = ref "" in
  let c_objective = ref "" in
  let c_kr_id = ref "" in
  let c_kr_title = ref "" in
  List.concat_map (fun e ->
    (* only proceed if include_krs is empty or has a match *)
      if List.length include_krs <> 0 && 
        not (List.mem e.kr_id uppercase_include_krs)
      then
        []
       else (
         let open Item in
         let add_project acc =
           if e.project <> !c_project then (
             c_project := e.project;
             Title (1, e.project) :: acc
           ) else
             acc
         in
         let add_objective acc =
           if e.objective <> !c_objective then (
             c_objective := e.objective;
             Title (2, e.objective) :: acc
           ) else 
             acc
         in
         let kr_item =
           if e.kr_id <> !c_kr_id || e.kr_title <> !c_kr_title then (
             c_kr_title := e.kr_title;
             c_kr_id := e.kr_id;
             [Paragraph (Text (Printf.sprintf "%s (%s)" e.kr_title e.kr_id))]
           ) else 
             []
         in
         let engineers_items =
           if not show_engineers then
             []
           else (
             if show_time then
               if show_time_calc then
                 (* show time calc + engineers *)
                 [
                   List (
                     Bullet '+',
                     List.map (fun x -> [Paragraph (Text x)]) e.time_entries
                   );
                   List (
                     Bullet '=', 
                     [make_engineers ~time:true e.time_per_engineer]
                   )
                 ]
               else
                 make_engineers ~time:true e.time_per_engineer
             else
               make_engineers ~time:false e.time_per_engineer
           )
         in
         Fmt.epr "XXXX %a\n%!" Fmt.Dump.(list (list Item.dump)) e.work;
         add_project @@
         add_objective @@
         [
           List (Bullet '-',
                 [kr_item @ [List (Bullet '-', engineers_items :: e.work)]])
         ]
       )
  ) (List.sort compare okrs)
  |> PPrint.(separate_map hardline) Item.pp
