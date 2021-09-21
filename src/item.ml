(*
 * Copyright (c) 2021 Magnus Skjegstad
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

type list_type = Ordered of int * char | Bullet of char

type inline =
  | Concat of inline list
  | Text of string
  | Emph of inline
  | Strong of inline
  | Code of string
  | Hard_break
  | Soft_break
  | Link of link
  | Image of link
  | Html of string

and link = { label : inline; destination : string; title : string option }

(* The subset of mardown supported for work items *)
type t =
  | Paragraph of inline
  | List of list_type * t list list
  | Blockquote of t list
  | Code_block of string * string

(* Dump contents *)

let dump_list_type ppf = function
  | Ordered (d, c) -> Fmt.pf ppf "Ordered (%d, %c)" d c
  | Bullet c -> Fmt.pf ppf "Bullet %c" c

let rec dump_inline ppf = function
  | Concat c -> Fmt.Dump.list dump_inline ppf c
  | Text s -> Fmt.pf ppf "Text %S" s
  | Emph e -> Fmt.pf ppf "Emph (%a)" dump_inline e
  | Strong e -> Fmt.pf ppf "Strong (%a)" dump_inline e
  | Code s -> Fmt.pf ppf "Code %S" s
  | Hard_break -> Fmt.pf ppf "Hard_break"
  | Soft_break -> Fmt.pf ppf "Soft_break"
  | Link l -> Fmt.pf ppf "Link %a" dump_link l
  | Image l -> Fmt.pf ppf "Image %a" dump_link l
  | Html s -> Fmt.pf ppf "Html %S" s

and dump_link ppf t =
  let open Fmt.Dump in
  record
    [
      field "label" (fun x -> x.label) dump_inline;
      field "destination" (fun x -> x.destination) string;
      field "title" (fun x -> x.title) (option string);
    ]
    ppf t

let rec dump ppf = function
  | Paragraph t -> Fmt.pf ppf "Paragraph (%a)" dump_inline t
  | List (x, y) ->
      Fmt.pf ppf "List (%a, %a)" dump_list_type x Fmt.Dump.(list (list dump)) y
  | Blockquote l -> Fmt.pf ppf "Blockquote %a" Fmt.(Dump.list dump) l
  | Code_block (x, y) -> Fmt.pf ppf "Code_block (%S, %S)" x y

(* Pretty-print contents *)

let rec pp_inline ppf = function
  | Concat c -> List.iter (pp_inline ppf) c
  | Text s -> Fmt.string ppf s
  | Emph e -> Fmt.pf ppf "*%a*" pp_inline e
  | Strong e -> Fmt.pf ppf "**%a**" pp_inline e
  | Code s -> Fmt.pf ppf "`%s`" s
  | Hard_break -> Fmt.pf ppf "@."
  | Soft_break -> Fmt.pf ppf "@.@."
  | Link l -> Fmt.pf ppf "[%a](%s)" pp_inline l.label l.destination
  | Image l -> Fmt.pf ppf "![%a](%s)" pp_inline l.label l.destination
  | Html s -> Fmt.string ppf s

let rec pp ppf = function
  | Paragraph t -> Fmt.hovbox pp_inline ppf t
  | List (Ordered (i, c), y) ->
      List.iter
        (fun e ->
          Fmt.pf ppf "%d%c " i c;
          List.iter (pp ppf) e;
          Fmt.pf ppf "@.")
        y
  | List (Bullet c, y) ->
      List.iter
        (fun e ->
          Fmt.pf ppf "%c " c;
          List.iter (pp ppf) e;
          Fmt.pf ppf "@.")
        y
  | Blockquote l ->
      Fmt.pf ppf "> ";
      List.iter (pp ppf) l
  | Code_block (x, y) -> Fmt.pf ppf "```%s@.%s@.```" x y
