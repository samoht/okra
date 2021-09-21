(*
 * Copyright (c) 2021 Magnus Skjegstad <magnus@skjegstad.com>
 * Copyright (c) 2021 Thomas Gazagnaire <thomas@gazagnaire.org>
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

open Omd

let hashtbl_keys ht =
  List.sort_uniq String.compare (List.of_seq (Hashtbl.to_seq_keys ht))

exception No_time_found of string (* Record found without a time record *)

exception Invalid_time of string (* Time record found, but has errors *)

exception No_work_found of string (* No work items found under KR *)

exception Multiple_time_entries of string (* More than one time entry found *)

exception No_KR_ID_found of string (* Empty or no KR ID *)

exception No_title_found of string (* No title found *)

exception Invalid_markdown_in_work_items of string
(* Subset of markdown not supported in work items *)

(* Types for parsing the AST *)
type elt =
  | O of string (* Objective name, without lead *)
  | Proj of string (* Project name / pillar *)
  | KR of string (* Full name of KR, with ID *)
  | KR_id of string (* ID of KR *)
  | KR_title of string (* Title without ID, tech lead *)
  | Work of Item.t list list (*  Work items *)
  | Time of string (* Time entry *)
  | Counter of int
(* Increasing counter to be able to sort multiple entries by time *)

type t = (string, elt list list) Hashtbl.t
type markdown = (string * string) list Omd.block list

let okr_re = Str.regexp "\\(.+\\) (\\([a-zA-Z]+[0-9]+\\))$"
(* Header: This is a KR (KR12) *)

let obj_re = Str.regexp "\\(.+\\) (\\([a-zA-Z ]+\\))$"
(* Header: This is an objective (Tech lead name) *)

let new_okr_re = obj_re

let is_time_block = function
  | [ Paragraph (_, Text (_, s)) ] -> String.get (String.trim s) 0 = '@'
  | _ -> false

let time_block_is_sane s =
  let regexp = Str.regexp "^@[a-zA-Z0-9-]+[ ]+([0-9.]+ day[s]?)$" in
  let pieces = String.split_on_char ',' (String.trim s) in
  List.for_all
    (fun s ->
      let s = String.trim s in
      Str.string_match regexp s 0)
    pieces

let is_suffix suffix s =
  String.length s >= String.length suffix
  &&
  let suffix = String.uppercase_ascii suffix in
  let s = String.uppercase_ascii s in
  String.equal suffix
    (String.sub s
       (String.length s - String.length suffix)
       (String.length suffix))

let parse_okr_title s =
  (* todo: could match on ??) too? *)
  if is_suffix "(new kr)" s || is_suffix "(new okr)" s then
    match Str.string_match new_okr_re s 0 with
    | false -> None
    | true -> Some (String.trim (Str.matched_group 1 s), "new KR")
  else
    match Str.string_match okr_re s 0 with
    | false -> None
    | true ->
        Some
          ( String.trim (Str.matched_group 1 s),
            String.trim (Str.matched_group 2 s) )

let pp ppf okr =
  let pp ppf = function
    | Proj s -> Fmt.pf ppf "P: %s" s
    | O s -> Fmt.pf ppf "O: %s" s
    | KR s -> Fmt.pf ppf "KR: %s" s
    | KR_id s -> Fmt.pf ppf "KR id: %s" s
    | KR_title s -> Fmt.pf ppf "KR title: %s" s
    | Work w -> Fmt.pf ppf "W: %a" Fmt.Dump.(list (list Item.dump)) w
    | Time _ -> Fmt.pf ppf "Time: <not shown>"
    | Counter c -> Fmt.pf ppf "Cnt: %d" c
  in
  Fmt.Dump.list pp ppf okr

let store_result store okr_list =
  let key1 =
    let f =
      List.find_opt
        (fun xs -> match xs with KR_title _ -> true | _ -> false)
        okr_list
    in
    match f with Some (KR_title t) -> t | _ -> "Unknown"
  in
  let key = Printf.sprintf "%s" (String.uppercase_ascii key1) in
  let has_time =
    match
      List.find_opt
        (fun xs -> match xs with Time _ -> true | _ -> false)
        okr_list
    with
    | None -> false
    | Some _ -> true
  in
  match has_time with
  | false ->
      raise
        (No_time_found (Fmt.str "@[<hov 2>Time not found.@ %a@]\n" pp okr_list))
  | true -> (
      match Hashtbl.find_opt store key with
      | None -> Hashtbl.add store key [ okr_list ]
      | Some x -> Hashtbl.replace store key (x @ [ okr_list ]))

let rec inline = function
  | Concat (_, xs) -> Item.Concat (List.map inline xs)
  | Text (_, s) -> Item.Text s
  | Emph (_, s) -> Item.Emph (inline s)
  | Strong (_, s) -> Item.Strong (inline s)
  | Code (_, s) -> Item.Code s
  | Hard_break _ -> Item.Hard_break
  | Soft_break _ -> Item.Soft_break
  | Link (_, { label; destination; title }) ->
      Item.Link { label = inline label; destination; title }
  | Html (_, s) -> Item.Html s
  | Image (_, { label; destination; title }) ->
      Item.Image { label = inline label; destination; title }

let list_type = function
  | Ordered (i, c) -> Item.Ordered (i, c)
  | Bullet c -> Item.Bullet c

let rec block = function
  | Paragraph (_, x) -> Item.Paragraph (inline x)
  | List (_, x, _, bls) -> Item.List (list_type x, List.map (List.map block) bls)
  | Blockquote (_, x) -> Item.Blockquote (List.map block x)
  | Code_block (_, x, y) -> Item.Code_block (x, y)
  | Html_block _ -> raise (Invalid_markdown_in_work_items "Html_bloc")
  | Definition_list _ ->
      raise (Invalid_markdown_in_work_items "Definition_list")
  | Thematic_break _ -> raise (Invalid_markdown_in_work_items "Thematic_break")
  | Heading _ -> raise (Invalid_markdown_in_work_items "Heading")

let inline_to_string i =
  let buf = Buffer.create 10 in
  PPrint.ToBuffer.pretty 10. 80 buf (Item.pp_inline i);
  Buffer.contents buf

let item_to_string i =
  let buf = Buffer.create 10 in
  PPrint.ToBuffer.pretty 10. 80 buf (Item.pp i);
  Buffer.contents buf

let block_okr = function
  | Paragraph (_, x) -> (
      let okr_title = String.trim (inline_to_string (inline x)) in
      match parse_okr_title okr_title with
      | None -> [ KR okr_title; KR_title okr_title ]
      | Some (title, id) -> [ KR okr_title; KR_title title; KR_id id ])
  | List (_, _, _, bls) -> (
      if List.length (List.filter is_time_block bls) > 1 then
        (* This is fatal, as we can miss tracked time if this occurs
           -- time should always be summarised in first entry *)
        raise (Multiple_time_entries "Multiple time entries found")
      else ();
      match bls with
      | [] -> []
      | tb :: tl ->
          if
            (* Assume first block is time if present, and ... *)
            is_time_block tb
          then
            (* todo verify that this is true *)
            let time_s =
              String.concat "" (List.map (fun b -> item_to_string (block b)) tb)
            in
            let work_items = List.map (List.map block) tl in
            [ Time time_s; Work work_items ]
          else [])
  | _ -> []

let strip_obj_lead s =
  match Str.string_match obj_re (String.trim s) 0 with
  | false -> s
  | true -> Str.matched_group 1 s

type state = {
  mutable current_o : string;
  mutable current_proj : string;
  mutable current_counter : int;
  ignore_sections : string list;
  include_sections : string list;
}

let init ?(ignore_sections = []) ?(include_sections = []) () =
  {
    current_o = "";
    current_proj = "";
    current_counter = 0;
    ignore_sections;
    include_sections;
  }

let process_okr_block t ht hd tl =
  (* peek at each block in list, consume if match - otherwise return
     full list for regular processing*)
  match hd with
  | Heading (_, _, il) ->
      let title =
        match il with
        (* Display header with level, strip lead from objectives if present *)
        | Text (_, s) -> strip_obj_lead s
        | _ -> "None"
      in
      (* remember last object title seen - works if Os have not been
         renamed in a set of files *)
      t.current_o <- title;
      tl
  | List (_, _, _, bls) ->
      let _ =
        List.map
          (fun xs ->
            let okr_list = List.concat (List.map block_okr xs) in
            if
              List.length t.ignore_sections = 0
              || (* ignore if proj or obj is in ignore_sections *)
              (not
                 (List.mem
                    (String.uppercase_ascii t.current_proj)
                    t.ignore_sections))
              && not
                   (List.mem
                      (String.uppercase_ascii t.current_o)
                      t.ignore_sections)
            then
              if
                List.length t.include_sections = 0
                (* only include if proj or obj is in include_sections *)
                || List.mem
                     (String.uppercase_ascii t.current_proj)
                     t.include_sections
                || List.mem
                     (String.uppercase_ascii t.current_o)
                     t.include_sections
              then
                store_result ht
                  ([
                     Proj t.current_proj;
                     O t.current_o;
                     Counter t.current_counter;
                   ]
                  @ okr_list)
              else ()
            else ())
          bls
      in
      t.current_counter <- t.current_counter + 1;
      tl
  | _ -> tl

let process_entry t ht hd tl =
  (* Find project level headers *)
  (match hd with
  | Heading (_, level, il) -> (
      match (level, il) with
      (* Display header with level, strip lead from objectives if present *)
      | 1, Text (_, s) -> t.current_proj <- s
      | _, _ -> ())
  | _ -> ());
  process_okr_block t ht hd tl

let rec process t ht ast =
  match ast with
  | [ hd ] -> process t ht (process_entry t ht hd [])
  | hd :: tl -> process t ht (process_entry t ht hd tl)
  | [] -> ()

let of_makdown ?(ignore_sections = [ "OKR Updates" ]) ?(include_sections = [])
    ast =
  let u_ignore = List.map String.uppercase_ascii ignore_sections in
  let u_include = List.map String.uppercase_ascii include_sections in
  let state = init ~ignore_sections:u_ignore ~include_sections:u_include () in
  let store = Hashtbl.create 100 in
  process state store ast;
  store

let entry okr_list =
  (* This function expects a list of entries for the same KR, typically
     corresponding to a set of weekly reports. Each list item will consist of
     a list of okr_t items, which provides time, work items etc for this entry.

     This function will aggregate all entries for the same KR in an
     okr_entry record for easier processing later.
  *)
  let okr_proj = ref "" in
  let okr_obj = ref "" in
  let okr_kr_title = ref "" in
  let okr_kr_id = ref "" in
  let okr_counter = ref 0 in

  (* Assume each item in list has the same O/KR/Proj, so just parse
     the first one *)
  (* todo we could sanity check here by verifying that every entry has
     the same KR/O *)
  List.iter
    (fun el ->
      match el with
      | Proj s -> okr_proj := s
      | O s -> okr_obj := s
      | KR_title s -> okr_kr_title := s
      | KR_id s -> okr_kr_id := String.uppercase_ascii s
      | Counter x -> okr_counter := x
      | _ -> ())
    (List.hd okr_list);

  (* Find all the time records and store in hashtbl keyed by engineer
     + original *)
  let okr_time_entries = ref [] in
  let ht_t = Hashtbl.create 7 in
  List.iter
    (fun elements ->
      List.iter
        (fun el ->
          match el with
          | Time t_ ->
              (* Store the string entry to be able to check correctness later *)
              okr_time_entries := !okr_time_entries @ [ t_ ];
              (* check that time block makes sense *)
              if not (time_block_is_sane t_) then
                raise (Invalid_time (Fmt.str "Time record is invalid: %s" t_))
              else ();
              (* split on @, then extract first word and any float after *)
              let t_split = Str.split (Str.regexp "@+") t_ in
              List.iter
                (fun s ->
                  match
                    Str.string_match
                      (Str.regexp
                         "^\\([a-zA-Z0-9-]+\\)[ ]+(\\([0-9.]+\\) day[s]?)")
                      s 0
                  with
                  | false -> ()
                  | true ->
                      let user = Str.matched_group 1 s in
                      (* todo: let this conversion raise an exception,
                         would be nice to exit more cleanly, but it
                         should be fatal *)
                      let days = Float.of_string (Str.matched_group 2 s) in
                      Hashtbl.add ht_t user days)
                t_split
          | _ -> ())
        elements)
    okr_list;

  (* Sum time per engineer *)
  let time_per_engineer = Hashtbl.create 7 in
  List.iter
    (fun key ->
      let sum =
        List.fold_left
          (fun a b -> Float.add a b)
          0.0
          (Hashtbl.find_all ht_t key)
      in
      Hashtbl.replace time_per_engineer key sum)
    (hashtbl_keys ht_t);

  (* Add work items in order, concat all the lists *)
  let work =
    List.concat_map
      (List.concat_map (function Work e -> e | _ -> []))
      okr_list
  in

  (* Some basic sanity checking *)
  if List.length work = 0 then
    raise
      (No_work_found (Fmt.str "KR with ID %s is without work items" !okr_kr_id))
  else ();

  if String.length (String.trim !okr_kr_id) = 0 then
    raise
      (No_KR_ID_found
         (Fmt.str "No KR ID found for \"%s\" (under objective %s)" !okr_kr_title
            !okr_obj))
  else ();

  if
    String.length (String.trim !okr_proj) = 0
    || String.length (String.trim !okr_obj) = 0
  then
    raise
      (No_title_found
         (Fmt.str "No title for project or objective found for \"%s\""
            !okr_kr_title))
  else ();

  (* Construct final entry *)
  {
    Reports.counter = !okr_counter;
    project = !okr_proj;
    objective = !okr_obj;
    kr_title = !okr_kr_title;
    kr_id = !okr_kr_id;
    time_entries = !okr_time_entries;
    time_per_engineer;
    work;
  }

let ht_add_or_sum ht k v =
  match Hashtbl.find_opt ht k with
  | None -> Hashtbl.add ht k v
  | Some x -> Hashtbl.replace ht k (Float.add v x)

let reports okrs = List.map entry (List.of_seq (Hashtbl.to_seq_values okrs))

let by_engineer ?(include_krs = []) okrs =
  let v = reports okrs in
  let uppercase_include_krs = List.map String.uppercase_ascii include_krs in
  let result = Hashtbl.create 7 in
  List.iter
    (fun e ->
      (* only proceed if include_krs is empty or has a match *)
      if
        List.length include_krs = 0
        || List.mem e.Reports.kr_id uppercase_include_krs
      then
        Hashtbl.iter (fun k w -> ht_add_or_sum result k w) e.time_per_engineer
      else ()) (* skip this KR *)
    v;
  result
