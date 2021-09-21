(*
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

type t = {
  show_time : bool;
  show_time_calc : bool;
  show_engineers : bool;
  ignore_sections : string list;
  include_sections : string list;
  include_krs : string list;
}

let default_conf =
  {
    show_time = true;
    show_time_calc = false;
    show_engineers = true;
    ignore_sections = [];
    include_sections = [];
    include_krs = [];
  }

let run ?(conf = default_conf) file =
  let ic = open_in file in
  let str = really_input_string ic (in_channel_length ic) in
  let md = Omd.of_string str in
  let okrs =
    Okra.Aggregate.of_makdown ~ignore_sections:conf.ignore_sections
      ~include_sections:conf.include_sections md
  in
  let okrs = Okra.Aggregate.reports okrs in
  let doc =
    Okra.Reports.pp ~show_time:conf.show_time
      ~show_time_calc:conf.show_time_calc ~show_engineers:conf.show_engineers
      ~include_krs:conf.include_krs okrs
  in
  PPrint.ToChannel.pretty 10. 80 stdout doc

let () =
  if Array.length Sys.argv <> 2 then Fmt.epr "usage: ./test.exe file"
  else run Sys.argv.(1)
