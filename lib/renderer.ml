open Tyxml.Html

let page_shell ?(with_math = false) ~title ~canonical content =
  let math_head =
    if not with_math then []
    else
      [ link ~rel:[`Stylesheet]
          ~href:"https://cdn.jsdelivr.net/npm/katex@0.16.22/dist/katex.min.css" ()
      ; script ~a:[ Unsafe.string_attrib "defer" ""
                  ; Unsafe.string_attrib "src"
                      "https://cdn.jsdelivr.net/npm/katex@0.16.22/dist/katex.min.js" ]
          (txt "")
      ; script ~a:[ Unsafe.string_attrib "defer" ""
                  ; Unsafe.string_attrib "src"
                      "https://cdn.jsdelivr.net/npm/katex@0.16.22/dist/contrib/auto-render.min.js"
                  ; Unsafe.string_attrib "onload"
                      "renderMathInElement(document.body,{delimiters:[{left:'$$',right:'$$',display:true},{left:'$',right:'$',display:false}]})" ]
          (txt "")
      ]
  in
  html ~a:[a_lang "en"]
    (head (Tyxml.Html.title (txt title))
       ([ meta ~a:[a_charset "utf-8"] ()
        ; meta ~a:[ Unsafe.string_attrib "name" "viewport"
                  ; a_content "width=device-width, initial-scale=1" ] ()
        ; link ~rel:[`Stylesheet] ~href:"/css/style.css" ()
        ; link ~rel:[`Icon] ~href:"/favicon.svg" ()
        ; link ~rel:[`Other "canonical"] ~href:canonical ()
        ] @ math_head))
    (body
       [ nav ~a:[a_class ["site-nav"]]
           [ a ~a:[a_href "/"] [txt "Home"]
           ; a ~a:[a_href "/blog/"] [txt "Blog"]
           ; a ~a:[a_href "/portfolio/"] [txt "Portfolio"]
           ; a ~a:[a_href "/art/"] [txt "Art"]
           ; a ~a:[a_href "/cv/"] [txt "CV"]
           ]
       ; main ~a:[a_class ["content"]] content
       ; footer ~a:[a_class ["site-footer"]] [txt "\xe2\x80\x94"]
       ])

let to_string doc =
  Format.asprintf "%a" (Tyxml.Html.pp ()) doc

let render_page ~base_url ~url (page : Site.page) =
  let canonical = base_url ^ url in
  match page with
  | Site.Cv { fm; body_html } ->
    let doc =
      page_shell ~title:fm.title ~canonical
        [ h1 [txt fm.title]
        ; p ~a:[a_class ["cv-download"]]
            [ a ~a:[a_href "/cv/cv.pdf"; Unsafe.string_attrib "download" ""] [txt "Download PDF"] ]
        ; Unsafe.data body_html
        ]
    in
    to_string doc
  | Site.Post { fm; body_html } ->
    let tag_spans = List.map (fun t ->
      span ~a:[a_class ["post-tag"]] [txt t]
    ) fm.tags in
    let meta_content = match fm.date, fm.tags with
      | None,   []   -> []
      | Some d, []   -> [p ~a:[a_class ["post-meta"]] [txt d]]
      | None,   _    -> [p ~a:[a_class ["post-meta"]] tag_spans]
      | Some d, _    -> [p ~a:[a_class ["post-meta"]] ([txt d; txt " "] @ tag_spans)]
    in
    let doc =
      page_shell ~with_math:true ~title:fm.title ~canonical
        ([ h1 [txt fm.title] ] @ meta_content @ [ Unsafe.data body_html ])
    in
    to_string doc
  | Site.Project { fm; body_html } ->
    let doc =
      page_shell ~title:fm.title ~canonical
        [ h1 [txt fm.title]
        ; Unsafe.data body_html
        ]
    in
    to_string doc
  | Site.ArtPiece { fm; body_html } ->
    let doc =
      page_shell ~title:fm.title ~canonical
        [ h1 [txt fm.title]
        ; Unsafe.data body_html
        ]
    in
    to_string doc

let render_index ~base_url ~canonical_path ~title items item_url item_title item_date =
  let li_of_item item =
    let url  = item_url item in
    let name = item_title item in
    match item_date item with
    | Some d ->
      li [ a ~a:[a_href url] [txt name]
         ; txt (" — " ^ d)
         ]
    | None ->
      li [ a ~a:[a_href url] [txt name] ]
  in
  let doc =
    page_shell ~title ~canonical:(base_url ^ canonical_path)
      [ h1 [txt title]
      ; ul (List.map li_of_item items)
      ]
  in
  to_string doc

let fm_of_page = function
  | Site.Cv { fm; _ } | Site.Post { fm; _ }
  | Site.Project { fm; _ } | Site.ArtPiece { fm; _ } -> fm

let render_home ~base_url (site : Site.site) =
  let recent_posts    = List.filteri (fun i _ -> i < 5) site.posts in
  let recent_projects = List.filteri (fun i _ -> i < 5) site.projects in
  let post_items =
    List.map (fun p ->
      let fm = fm_of_page p in
      let date_str = match fm.date with Some d -> " — " ^ d | None -> "" in
      li [ a ~a:[a_href ("/blog/" ^ fm.slug ^ "/")] [txt fm.title]
         ; txt date_str
         ]
    ) recent_posts
  in
  let project_items =
    List.map (fun p ->
      let fm = fm_of_page p in
      li [ a ~a:[a_href ("/portfolio/" ^ fm.slug ^ "/")] [txt fm.title] ]
    ) recent_projects
  in
  let doc =
    page_shell ~title:"David Moulin" ~canonical:(base_url ^ "/")
      [ h1 [txt "David Moulin"]
      ; p ~a:[a_class ["tagline"]] [txt "Sometimes, when I'm in the right mood, I try to make stuff."]
      ; hr ()
      ; h2 [txt "Writing"]
      ; (if post_items = [] then p [txt "Nothing yet."]
         else ul post_items)
      ; p [ a ~a:[a_href "/blog/"] [txt "All posts \xe2\x86\x92"] ]
      ; h2 [txt "Projects"]
      ; (if project_items = [] then p [txt "Nothing yet."]
         else ul project_items)
      ; p [ a ~a:[a_href "/portfolio/"] [txt "All projects \xe2\x86\x92"] ]
      ]
  in
  to_string doc

let rfc822_of_date date_str =
  try
    match String.split_on_char '-' date_str with
    | [y; m; d] ->
      let year  = int_of_string y in
      let month = int_of_string m in
      let day   = int_of_string d in
      let month_names = [| "Jan"; "Feb"; "Mar"; "Apr"; "May"; "Jun";
                           "Jul"; "Aug"; "Sep"; "Oct"; "Nov"; "Dec" |] in
      let day_names = [| "Sun"; "Mon"; "Tue"; "Wed"; "Thu"; "Fri"; "Sat" |] in
      let t = [| 0; 3; 2; 5; 0; 3; 5; 1; 4; 6; 2; 4 |] in
      let y = if month < 3 then year - 1 else year in
      let dow = (y + y/4 - y/100 + y/400 + t.(month-1) + day) mod 7 in
      Printf.sprintf "%s, %02d %s %d 00:00:00 +0000"
        day_names.(dow) day month_names.(month-1) year
    | _ -> date_str
  with _ -> date_str

let render_sitemap ~base_url (site : Site.site) =
  let url ?(lastmod="") loc =
    if lastmod = "" then
      Printf.sprintf "  <url><loc>%s%s</loc></url>" base_url loc
    else
      Printf.sprintf "  <url><loc>%s%s</loc><lastmod>%s</lastmod></url>" base_url loc lastmod
  in
  let static_urls = List.map url ["/"; "/blog/"; "/portfolio/"; "/art/"; "/cv/"] in
  let post_urls = List.map (fun p ->
    let fm = fm_of_page p in
    url ~lastmod:(Option.value ~default:"" fm.date) (Printf.sprintf "/blog/%s/" fm.slug)
  ) site.posts in
  let project_urls = List.map (fun p ->
    let fm = fm_of_page p in
    url (Printf.sprintf "/portfolio/%s/" fm.slug)
  ) site.projects in
  let art_urls = List.map (fun p ->
    let fm = fm_of_page p in
    url (Printf.sprintf "/art/%s/" fm.slug)
  ) site.art in
  let all = static_urls @ post_urls @ project_urls @ art_urls in
  Printf.sprintf {|<?xml version="1.0" encoding="UTF-8"?>
<urlset xmlns="http://www.sitemaps.org/schemas/sitemap/0.9">
%s
</urlset>|} (String.concat "\n" all)

let render_robots ~base_url =
  Printf.sprintf "User-agent: *\nAllow: /\n\nSitemap: %s/sitemap.xml\n" base_url

let render_rss ~base_url (posts : Site.page list) =
  let item_of_post post =
    let fm = fm_of_page post in
    let date = Option.value ~default:"" fm.date |> rfc822_of_date in
    Printf.sprintf {|
		<item>
			<title>%s</title>
			<link>%s/blog/%s/</link>
			<description></description>
			<pubDate>%s</pubDate>
		</item>|} fm.title base_url fm.slug date
  in
  let items = String.concat "\n" (List.map item_of_post posts) in
  Printf.sprintf {|<?xml version="1.0" encoding="UTF-8" ?>
<?xml-stylesheet type="text/xsl" href="/rss.xsl"?>
<rss version="2.0">
	<channel>
		<title>David Moulin</title>
		<link>%s</link>
		<language>en-us</language>
		<description>Backend engineer.</description>
		%s
	</channel>
</rss>|} base_url items
