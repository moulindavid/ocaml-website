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
