type frontmatter = {
  title : string;
  date  : string option;
  tags  : string list;
  slug  : string;
}

type page =
  | Cv       of { fm : frontmatter; body_html : string }
  | Post     of { fm : frontmatter; body_html : string }
  | Project  of { fm : frontmatter; body_html : string }
  | ArtPiece of { fm : frontmatter; body_html : string }

type site = {
  cv       : page option;
  posts    : page list;
  projects : page list;
  art      : page list;
}
