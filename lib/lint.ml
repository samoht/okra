(*
 * Copyright (c) 2021 Magnus Skjegstad <magnus@skjegstad.com>
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

let ( let* ) = Result.bind

type lint_error =
  | Format_error of (int * string) list
  | No_time_found of int option * string
  | Invalid_time of { lnum : int option; title : string; entry : string }
  | Invalid_total_time of string * Time.t * Time.t
  | Multiple_time_entries of int option * string
  | No_work_found of int option * string
  | No_KR_ID_found of int option * string
  | No_project_found of int option * string
  | Not_all_includes of string list
  | Invalid_markdown_in_work_items of int option * string
  | Invalid_quarter of KR.Work.t

type lint_result = (unit, lint_error list) result

let fail_fmt_patterns =
  [
    (Str.regexp ".*\t", "Tab found. Use spaces for indentation (2 preferred).");
    ( Str.regexp ".*-  ",
      "Double space before text after bullet point ('-  text'), this can \
       confuse the parser. Use '- text'" );
    ( Str.regexp "^ -",
      "Single space used for indentation (' - text'). Remove or replace by 2 \
       or more spaces." );
    ( Str.regexp "^[ ]*\\*",
      "* used as bullet point, this can confuse the parser. Only use - as \
       bullet marker." );
    ( Str.regexp "^[ ]*\\+",
      "+ used as bullet point, this can confuse the parser. Only use - as \
       bullet marker." );
    ( Str.regexp "^[ ]+#",
      "Space found before title marker #. Start titles in first column." );
    ( Str.regexp "^[ ]+- Work Item 1",
      "Placeholder text detected. Replace with actual activity." );
  ]

let pp_error ppf = function
  | Format_error x ->
      let pp_msg ppf (pos, msg) = Fmt.pf ppf "Line %d: %s" pos msg in
      Fmt.pf ppf "@[<v 0>%a@,%d formatting errors found. Parsing aborted.@]"
        (Fmt.list ~sep:Fmt.sp pp_msg)
        x (List.length x)
  | No_time_found (_, s) ->
      Fmt.pf ppf
        "@[<hv 2>In KR %S:@ No time entry found. Each KR must be followed by \
         '- @@... (x days)'@]@,"
        s
  | Invalid_time { lnum = _; title; entry } ->
      Fmt.pf ppf
        "@[<hv 2>In KR %S:@ Invalid time entry %S found. Format is '- @@eng1 \
         (x days), @@eng2 (y days)'@ where x and y must be divisible by 0.5@]@,"
        title entry
  | Invalid_total_time (s, t, total) ->
      Fmt.pf ppf
        "@[<hv 2>Invalid total time found for %s (reported %a, expected %a).@]@,"
        s Time.pp t Time.pp total
  | Multiple_time_entries (_, s) ->
      Fmt.pf ppf
        "@[<hv 2>In KR %S:@ Multiple time entries found. Only one time entry \
         should follow immediately after the KR.@]@,"
        s
  | No_work_found (_, s) ->
      Fmt.pf ppf
        "@[<hv 2>In KR %S:@ No work items found. This may indicate an \
         unreported parsing error. Remove the KR if it is without work.@]@,"
        s
  | No_KR_ID_found (_, s) ->
      Fmt.pf ppf
        "@[<hv 2>In KR %S:@ No KR ID found. WIs should be in the format \"This \
         is a WI (#123)\", where 123 is the WI issue ID. Legacy KRs should be \
         in the format \"This is a KR (PLAT123)\", where PLAT123 is the KR ID. \
         For WIs that don't have an ID yet, use \"New WI\" and for work \
         without a WI use \"No WI\".@]@,"
        s
  | No_project_found (_, s) ->
      Fmt.pf ppf "@[<hv 2>In KR %S:@ No project found (starting with '#')@]@," s
  | Not_all_includes s ->
      Fmt.pf ppf "Missing includes section: %a\n" Fmt.(list ~sep:comma string) s
  | Invalid_markdown_in_work_items (_, s) ->
      Fmt.pf ppf "@[<hv 2>Invalid markdown in work items:@ %s@]@," s
  | Invalid_quarter kr ->
      Fmt.pf ppf "@[<hv 2>In WI %S:@ Work logged on WI scheduled for %a@]@,"
        kr.title (Fmt.option Quarter.pp) kr.quarter

let string_of_error = Fmt.to_to_string pp_error

(* Check a single line for formatting errors returning a list of error messages
   with the position *)
let check_line line pos =
  List.fold_left
    (fun acc (regexp, msg) ->
      if Str.string_match regexp line 0 then (pos, msg) :: acc else acc)
    [] fail_fmt_patterns

let grep_n s lines =
  let re = Str.regexp_case_fold (".*" ^ Str.quote s ^ ".*") in
  List.find_map
    (fun (i, line) -> if Str.string_match re line 0 then Some i else None)
    lines

let add_context lines = function
  | Parser.No_time_found s -> No_time_found (grep_n s lines, s)
  | Parser.Invalid_time { title; entry } ->
      Invalid_time { lnum = grep_n entry lines; title; entry }
  | Parser.Multiple_time_entries s -> Multiple_time_entries (grep_n s lines, s)
  | Parser.No_work_found s -> No_work_found (grep_n s lines, s)
  | Parser.No_KR_ID_found s -> No_KR_ID_found (grep_n s lines, s)
  | Parser.No_project_found s -> No_project_found (grep_n s lines, s)
  | Parser.Not_all_includes_accounted_for s -> Not_all_includes s
  | Parser.Invalid_markdown_in_work_items s ->
      Invalid_markdown_in_work_items (grep_n s lines, s)

let check_total_time ?check_time (krs : KR.t list) =
  match check_time with
  | None -> Ok ()
  | Some expected ->
      let tbl = Hashtbl.create 7 in
      List.iter
        (fun (kr : KR.t) ->
          Hashtbl.iter
            (fun name time ->
              let time =
                let open Time in
                match Hashtbl.find_opt tbl name with
                | Some x -> x +. time
                | None -> time
              in
              Hashtbl.replace tbl name time)
            kr.time_per_engineer)
        krs;
      Hashtbl.fold
        (fun name time acc ->
          let* () = acc in
          if Time.equal time expected then Ok ()
          else Error (Invalid_total_time (name, time, expected)))
        tbl (Ok ())

let check_quarters quarter krs warnings =
  List.fold_left
    (fun acc kr ->
      match kr.KR.kind with
      | Meta _ -> acc
      | Work w ->
          if Quarter.check quarter w.quarter then acc
          else Invalid_quarter w :: acc)
    warnings krs

let maybe_emit warnings =
  match warnings with [] -> Ok () | warnings -> Error warnings

let ( let* ) = Result.bind

(* Parse document as a string to check for aggregation errors (assumes no
   formatting errors) *)
let check_document ?okr_db ~include_sections ~ignore_sections ?check_time
    ~filename s =
  let quarter = Quarter.of_filename ~filename in
  let lines =
    String.split_on_char '\n' s |> List.mapi (fun i s -> (i + 1, s))
  in
  let md = Omd.of_string s in
  let okrs, warnings =
    Parser.of_markdown ~include_sections ~ignore_sections md
  in
  let warnings =
    let warnings = List.map (add_context lines) warnings in
    match check_total_time ?check_time okrs with
    | Ok () -> warnings
    | Error w -> w :: warnings
  in
  let* () = maybe_emit warnings in
  let report = Report.of_krs ?okr_db okrs in
  let krs = Report.all_krs report in
  let warnings = check_quarters quarter krs warnings in
  let* () = maybe_emit warnings in
  Ok ()

let document_ok ?okr_db ~include_sections ~ignore_sections ~format_errors
    ?check_time ~filename s =
  if !format_errors <> [] then
    Error
      [
        Format_error
          (List.sort (fun (x, _) (y, _) -> compare x y) !format_errors);
      ]
  else
    check_document ?okr_db ~include_sections ~ignore_sections ?check_time
      ~filename s

let lint_string_list ?okr_db ?(include_sections = []) ?(ignore_sections = [])
    ?check_time ~filename lines =
  let format_errors = ref [] in
  let rec check_and_read buf pos = function
    | [] -> Buffer.contents buf
    | line :: rest ->
        format_errors := check_line line pos @ !format_errors;
        Buffer.add_string buf line;
        Buffer.add_string buf "\n";
        check_and_read buf (pos + 1) rest
  in
  let s = check_and_read (Buffer.create 1024) 1 lines in
  document_ok ?okr_db ~include_sections ~ignore_sections ~format_errors
    ?check_time ~filename s

let lint ?okr_db ?(include_sections = []) ?(ignore_sections = []) ?check_time
    ~filename ic =
  let format_errors = ref [] in
  let rec check_and_read buf ic pos =
    try
      let line = input_line ic in
      format_errors := check_line line pos @ !format_errors;
      Buffer.add_string buf line;
      Buffer.add_string buf "\n";
      check_and_read buf ic (pos + 1)
    with
    | End_of_file -> Buffer.contents buf
    | e -> raise e
  in
  let s = check_and_read (Buffer.create 1024) ic 1 in
  document_ok ?okr_db ~include_sections ~ignore_sections ~format_errors
    ?check_time ~filename s

let short_messages_of_error file_name =
  let short_message line_number msg =
    [ Printf.sprintf "%s:%d:%s" file_name line_number msg ]
  in
  let short_messagef line_number_opt fmt =
    let line_number = Option.value ~default:1 line_number_opt in
    Format.kasprintf (short_message line_number) fmt
  in
  function
  | Format_error errs ->
      List.concat_map
        (fun (line_number, message) -> short_message line_number message)
        errs
  | No_time_found (line_number, kr) ->
      short_messagef line_number "No time found in %S" kr
  | Invalid_time { lnum; title; entry } ->
      short_messagef lnum "Invalid time entry %S in %S" entry title
  | Invalid_total_time (s, t, total) ->
      short_messagef None "Invalid total time for %S (%a/%a)" s Time.pp t
        Time.pp total
  | Multiple_time_entries (line_number, kr) ->
      short_messagef line_number "Multiple time entries for %S" kr
  | No_work_found (line_number, kr) ->
      short_messagef line_number "No work found for %S" kr
  | No_KR_ID_found (line_number, kr) ->
      short_messagef line_number "No KR ID found for %S" kr
  | No_project_found (line_number, kr) ->
      short_messagef line_number "No project found for %S" kr
  | Not_all_includes l ->
      short_messagef None "Missing includes section: %s" (String.concat ", " l)
  | Invalid_markdown_in_work_items (line_number, s) ->
      short_messagef line_number "Invalid markdown in work items: %s" s
  | Invalid_quarter kr ->
      short_messagef None "Using KR of invalid quarter: %S (%a)" kr.title
        (Fmt.option Quarter.pp) kr.quarter
