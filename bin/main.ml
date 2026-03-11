let rec walk_dir dir =
  Sys.readdir dir
  |> Array.to_list
  |> List.concat_map (fun name ->
      let path = Filename.concat dir name in
      if Sys.is_directory path then walk_dir path
      else [path])

let slug_of_path path =
  Filename.remove_extension (Filename.basename path)

let kind_of_path path =
  let norm = String.map (fun c -> if c = '\\' then '/' else c) path in
  match String.split_on_char '/' norm |> List.rev with
  | _ :: "cv"        :: _ -> `Cv
  | _ :: "blog"      :: _ -> `Blog
  | _ :: "portfolio" :: _ -> `Portfolio
  | _ :: "art"       :: _ -> `Art
  | _                     -> `Unknown

let rec mkdir_p dir =
  if not (Sys.file_exists dir) then begin
    mkdir_p (Filename.dirname dir);
    Unix.mkdir dir 0o755
  end

let write_file path content =
  mkdir_p (Filename.dirname path);
  let oc = open_out path in
  output_string oc content;
  close_out oc

let output_path_for kind (fm : Site.frontmatter) =
  match kind with
  | `Cv        -> "output/cv/index.html"
  | `Blog      -> Printf.sprintf "output/blog/%s/index.html" fm.slug
  | `Portfolio -> Printf.sprintf "output/portfolio/%s/index.html" fm.slug
  | `Art       -> Printf.sprintf "output/art/%s/index.html" fm.slug
  | `Unknown   -> assert false

let url_for kind (fm : Site.frontmatter) =
  match kind with
  | `Cv        -> "/cv/"
  | `Blog      -> Printf.sprintf "/blog/%s/" fm.slug
  | `Portfolio -> Printf.sprintf "/portfolio/%s/" fm.slug
  | `Art       -> Printf.sprintf "/art/%s/" fm.slug
  | `Unknown   -> assert false


let copy_file src dst =
  let ic = open_in_bin src in
  let n = in_channel_length ic in
  let s = Bytes.create n in
  really_input ic s 0 n;
  close_in ic;
  let oc = open_out_bin dst in
  output_bytes oc s;
  close_out oc

let rec copy_tree src dst =
  mkdir_p dst;
  Sys.readdir src
  |> Array.to_list
  |> List.iter (fun name ->
        let src_path = Filename.concat src name in
        let dst_path = Filename.concat dst name in
        if Sys.is_directory src_path
        then copy_tree src_path dst_path
        else copy_file src_path dst_path)


let () =
  print_endline "OCaml SSG — starting build...";

  let content_dir = "content" in
  let files =
    if Sys.file_exists content_dir && Sys.is_directory content_dir
    then walk_dir content_dir
    else []
  in

  (* Only process .md files *)
  let md_files = List.filter (fun p ->
    Filename.check_suffix p ".md"
  ) files in

  let site = List.fold_left (fun (acc : Site.site) path ->
    let slug = slug_of_path path in
    let kind = kind_of_path path in
    if kind = `Unknown then acc
    else begin
      let (fm, body_html) = Parser.parse_file path slug in
      let page, out_path = match kind with
        | `Cv ->
          (Site.Cv { fm; body_html },
           output_path_for `Cv fm)
        | `Blog ->
          (Site.Post { fm; body_html },
           output_path_for `Blog fm)
        | `Portfolio ->
          (Site.Project { fm; body_html },
           output_path_for `Portfolio fm)
        | `Art ->
          (Site.ArtPiece { fm; body_html },
           output_path_for `Art fm)
        | `Unknown -> assert false
      in
      let html = Renderer.render_page page in
      write_file out_path html;
      Printf.printf "  wrote %s\n%!" out_path;
      match kind with
      | `Cv        -> { acc with cv = Some page }
      | `Blog      -> { acc with posts = acc.posts @ [page] }
      | `Portfolio -> { acc with projects = acc.projects @ [page] }
      | `Art       -> { acc with art = acc.art @ [page] }
      | `Unknown   -> assert false
    end
  ) Site.{ cv = None; posts = []; projects = []; art = [] } md_files in

  (* Sort posts by date descending *)
  let fm_of = Renderer.fm_of_page in
  let sorted_posts =
    List.sort (fun a b ->
      let da = Option.value ~default:"" (fm_of a).date in
      let db = Option.value ~default:"" (fm_of b).date in
      String.compare db da
    ) site.posts
  in
  let site = { site with posts = sorted_posts } in

  (* Index pages *)
  let blog_index =
    Renderer.render_index
      ~title:"Blog"
      site.posts
      (fun p -> url_for `Blog (fm_of p))
      (fun p -> (fm_of p).title)
      (fun p -> (fm_of p).date)
  in
  write_file "output/blog/index.html" blog_index;
  print_endline "  wrote output/blog/index.html";

  let portfolio_index =
    Renderer.render_index
      ~title:"Portfolio"
      site.projects
      (fun p -> url_for `Portfolio (fm_of p))
      (fun p -> (fm_of p).title)
      (fun p -> (fm_of p).date)
  in
  write_file "output/portfolio/index.html" portfolio_index;
  print_endline "  wrote output/portfolio/index.html";

  let art_index =
    Renderer.render_index
      ~title:"Art"
      site.art
      (fun p -> url_for `Art (fm_of p))
      (fun p -> (fm_of p).title)
      (fun p -> (fm_of p).date)
  in
  write_file "output/art/index.html" art_index;
  print_endline "  wrote output/art/index.html";

  (* Home page *)
  write_file "output/index.html" (Renderer.render_home site);
  print_endline "  wrote output/index.html";

  (* Copy static assets *)
  if Sys.file_exists "static" then
    copy_tree "static" "output";

  print_endline "Done."
