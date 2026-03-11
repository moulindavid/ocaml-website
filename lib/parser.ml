let read_file path =
  let ic = open_in path in
  let n = in_channel_length ic in
  let s = Bytes.create n in
  really_input ic s 0 n;
  close_in ic;
  Bytes.to_string s

let split_frontmatter raw =
  let lines = String.split_on_char '\n' raw in
  match lines with
  | "---" :: rest ->
    let rec aux acc = function
      | [] -> (String.concat "\n" (List.rev acc), "")
      | "---" :: body -> (String.concat "\n" (List.rev acc), String.concat "\n" body)
      | line :: rest  -> aux (line :: acc) rest
    in
    aux [] rest
  | _ -> ("", raw)

let string_of_yaml_value = function
  | `String s -> s
  | _ -> ""

let parse_frontmatter yaml_str slug =
  let title = ref slug in
  let date  = ref None in
  let tags  = ref [] in
  if yaml_str <> "" then begin
    match Yaml.of_string yaml_str with
    | Error (`Msg e) ->
      Printf.eprintf "Warning: YAML parse error in %s: %s\n" slug e
    | Ok value ->
      (match Yaml.Util.find_exn "title" value with
       | Some (`String s) -> title := s
       | _ -> ());
      (match Yaml.Util.find_exn "date" value with
       | Some (`String s) -> date := Some s
       | _ -> ());
      (match Yaml.Util.find_exn "tags" value with
       | Some (`A items) -> tags := List.filter_map (function `String s -> Some s | _ -> None) items
       | _ -> ())
  end;
  Site.{ title = !title; date = !date; tags = !tags; slug }

let parse_file path slug =
  let raw = read_file path in
  let (yaml_str, body_str) = split_frontmatter raw in
  let fm = parse_frontmatter yaml_str slug in
  let body_html =
    Cmarkit_html.of_doc ~safe:false (Cmarkit.Doc.of_string body_str)
  in
  (fm, body_html)
