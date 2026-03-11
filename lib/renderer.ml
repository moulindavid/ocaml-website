open Tyxml.Html

let page_shell ~title content =
  html ~a:[a_lang "en"]
    (head (Tyxml.Html.title (txt title))
       [ meta ~a:[a_charset "utf-8"] ()
       ; meta ~a:[ Unsafe.string_attrib "name" "viewport"
                 ; a_content "width=device-width, initial-scale=1" ] ()
       ; link ~rel:[`Stylesheet] ~href:"/css/style.css" ()
       ])
    (body
       [ nav ~a:[a_class ["site-nav"]]
           [ a ~a:[a_href "/"] [txt "Home"]
           ; txt " · "
           ; a ~a:[a_href "/blog/"] [txt "Blog"]
           ; txt " · "
           ; a ~a:[a_href "/portfolio/"] [txt "Portfolio"]
           ; txt " · "
           ; a ~a:[a_href "/art/"] [txt "Art"]
           ; txt " · "
           ; a ~a:[a_href "/cv/"] [txt "CV"]
           ]
       ; main ~a:[a_class ["content"]] content
       ; footer ~a:[a_class ["site-footer"]] [txt "\xe2\x80\x94"]
       ])

let to_string doc =
  Format.asprintf "%a" (Tyxml.Html.pp ()) doc

let render_page (page : Site.page) =
  match page with
  | Site.Cv { fm; body_html } ->
    let doc =
      page_shell ~title:fm.title
        [ h1 [txt fm.title]
        ; Unsafe.data body_html
        ]
    in
    to_string doc
  | Site.Post { fm; body_html } ->
    let date_el =
      match fm.date with
      | Some d -> [p ~a:[a_class ["post-date"]] [txt d]]
      | None   -> []
    in
    let doc =
      page_shell ~title:fm.title
        ([ h1 [txt fm.title] ] @ date_el @ [ Unsafe.data body_html ])
    in
    to_string doc
  | Site.Project { fm; body_html } ->
    let doc =
      page_shell ~title:fm.title
        [ h1 [txt fm.title]
        ; Unsafe.data body_html
        ]
    in
    to_string doc
  | Site.ArtPiece { fm; body_html } ->
    let doc =
      page_shell ~title:fm.title
        [ h1 [txt fm.title]
        ; Unsafe.data body_html
        ]
    in
    to_string doc

let render_index ~title items item_url item_title item_date =
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
    page_shell ~title
      [ h1 [txt title]
      ; ul (List.map li_of_item items)
      ]
  in
  to_string doc

let fm_of_page = function
  | Site.Cv { fm; _ } | Site.Post { fm; _ }
  | Site.Project { fm; _ } | Site.ArtPiece { fm; _ } -> fm

let render_home (site : Site.site) =
  let recent_posts = List.filteri (fun i _ -> i < 5) site.posts in
  let post_items =
    List.map (fun p ->
      let fm = fm_of_page p in
      let date_str = match fm.date with Some d -> " — " ^ d | None -> "" in
      li [ a ~a:[a_href ("/blog/" ^ fm.slug ^ "/")] [txt fm.title]
         ; txt date_str
         ]
    ) recent_posts
  in
  let doc =
    page_shell ~title:"Home"
      [ h1 [txt "Hello"]
      ; p  [txt "Welcome to my site."]
      ; h2 [txt "Recent posts"]
      ; (if post_items = [] then p [txt "No posts yet."]
         else ul post_items)
      ; p [ a ~a:[a_href "/blog/"] [txt "All posts \xe2\x86\x92"] ]
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
<rss version="2.0">
	<channel>
		<title>David Moulin</title>
		<link>%s</link>
		<language>en-us</language>
		<description>Backend engineer.</description>
		%s
	</channel>
</rss>|} base_url items
