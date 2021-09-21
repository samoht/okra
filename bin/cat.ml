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

type t = {
  show_time : bool;
  show_time_calc : bool;
  show_engineers : bool;
  ignore_sections : string list;
  include_sections : string list;
  include_krs : string list;
}

open Cmdliner

let show_time_term =
  let info =
    Arg.info [ "show-time" ] ~doc:"Include engineering time in output"
  in
  Arg.value (Arg.opt Arg.bool true info)

let show_time_calc_term =
  let info =
    Arg.info [ "show-time-calc" ]
      ~doc:
        "Include intermediate time calculations in output, showing each time \
         entry found with a sum at the end. This is useful for debugging when \
         aggregating reports for multiple weeks."
  in
  Arg.value (Arg.opt Arg.bool false info)

let show_engineers_term =
  let info =
    Arg.info [ "show-engineers" ] ~doc:"Include a list of engineers per KR"
  in
  Arg.value (Arg.opt Arg.bool true info)

let include_sections_term =
  let info =
    Arg.info [ "include-sections" ]
      ~doc:
        "If non-empty, only aggregate entries under these sections - \
         everything else is ignored."
  in
  Arg.value (Arg.opt (Arg.list Arg.string) [] info)

let ignore_sections_term =
  let info =
    Arg.info [ "ignore-sections" ]
      ~doc:
        "If non-empty, ignore everyhing under these sections (titles) from the \
         report"
  in
  Arg.value (Arg.opt (Arg.list Arg.string) [ "OKR updates" ] info)

let include_krs_term =
  let info =
    Arg.info [ "include-krs" ]
      ~doc:"If non-empty, only include this list of KR IDs in the output."
  in
  Arg.value (Arg.opt (Arg.list Arg.string) [] info)

let engineer_term =
  let info =
    Arg.info [ "engineer"; "e" ]
      ~doc:
        "Aggregate engineer reports. This is an alias for \
         --include-sections=\"last week\", --ignore-sections=\"\""
  in
  Arg.value (Arg.flag info)

let team_term =
  let info =
    Arg.info [ "team"; "t" ]
      ~doc:
        "Aggregate team reports. This is an alias for --include-sections=\"\", \
         --ignore-sections=\"OKR updates\""
  in
  Arg.value (Arg.flag info)

let run conf =
  let md = Omd.of_channel stdin in
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

let term =
  let cat show_time show_time_calc show_engineers include_krs ignore_sections
      include_sections team engineer =
    let conf =
      if engineer then
        {
          show_time;
          show_time_calc;
          show_engineers;
          include_krs;
          ignore_sections = [];
          include_sections = [ "Last week" ];
        }
      else if team then
        {
          show_time;
          show_time_calc;
          show_engineers;
          include_krs;
          ignore_sections = [ "OKR Updates" ];
          include_sections = [];
        }
      else
        {
          show_time;
          show_time_calc;
          show_engineers;
          include_krs;
          ignore_sections;
          include_sections;
        }
    in
    run conf
  in
  Term.(
    const cat
    $ show_time_term
    $ show_time_calc_term
    $ show_engineers_term
    $ include_krs_term
    $ ignore_sections_term
    $ include_sections_term
    $ team_term
    $ engineer_term)

let cmd =
  let info =
    Term.info "cat" ~doc:"parse and concatenate reports"
      ~man:
        [
          `S Manpage.s_description;
          `P
            "Parses one or more OKR reports and outputs a report aggregated \
             per KR. See below for options for modifying the output format.";
        ]
  in
  (term, info)
