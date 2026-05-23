<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
  <xsl:output method="html" encoding="UTF-8" indent="yes"/>
  <xsl:template match="/">
    <html lang="en">
      <head>
        <meta charset="utf-8"/>
        <meta name="viewport" content="width=device-width, initial-scale=1"/>
        <title><xsl:value-of select="/rss/channel/title"/> — RSS Feed</title>
        <style>
          @import url('https://fonts.googleapis.com/css2?family=IBM+Plex+Mono:ital,wght@0,400;0,600;1,400&amp;display=swap');
          *, *::before, *::after { box-sizing: border-box; margin: 0; padding: 0; }
          :root {
            --bg: #faf9ff; --text: #1e1b2e; --muted: #7b6f9e;
            --accent: #6c47b8; --accent-dim: #e8e2f8; --border: #d8d0f0;
          }
          body { font-family: 'IBM Plex Mono', monospace; background: var(--bg); color: var(--text); line-height: 1.7; font-size: 15px; }
          .wrap { max-width: 680px; margin: 0 auto; padding: 3rem 1.5rem; }
          .badge { display: inline-block; background: var(--accent-dim); color: var(--accent); font-size: 0.75rem; padding: 0.15em 0.6em; border-radius: 3px; margin-bottom: 1.5rem; }
          h1 { font-size: 1.6rem; font-weight: 600; color: var(--accent); margin-bottom: 0.4rem; }
          .desc { color: var(--muted); font-size: 0.85rem; margin-bottom: 2.5rem; }
          ul { list-style: none; padding: 0; }
          li { padding: 0.75rem 0; border-bottom: 1px solid var(--border); }
          li:first-child { border-top: 1px solid var(--border); }
          a { color: var(--text); text-decoration: none; font-weight: 600; }
          a:hover { color: var(--accent); }
          .date { display: block; color: var(--muted); font-size: 0.8rem; margin-top: 0.2rem; }
        </style>
      </head>
      <body>
        <div class="wrap">
          <span class="badge">RSS Feed</span>
          <h1><xsl:value-of select="/rss/channel/title"/></h1>
          <p class="desc"><xsl:value-of select="/rss/channel/description"/></p>
          <ul>
            <xsl:for-each select="/rss/channel/item">
              <li>
                <a><xsl:attribute name="href"><xsl:value-of select="link"/></xsl:attribute>
                  <xsl:value-of select="title"/>
                </a>
                <span class="date"><xsl:value-of select="pubDate"/></span>
              </li>
            </xsl:for-each>
          </ul>
        </div>
      </body>
    </html>
  </xsl:template>
</xsl:stylesheet>
