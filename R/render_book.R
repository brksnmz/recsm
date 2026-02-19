#!/usr/bin/env Rscript

# Try to reuse RStudio's bundled Pandoc when running outside RStudio.
if (!nzchar(Sys.which("pandoc"))) {
  rstudio_pandoc <- "/Applications/RStudio.app/Contents/MacOS/pandoc"
  if (dir.exists(rstudio_pandoc)) {
    Sys.setenv(RSTUDIO_PANDOC = rstudio_pandoc)
  }
}

if (!rmarkdown::pandoc_available("2.0")) {
  stop(
    "Pandoc >= 2.0 is required to render this book (anchor sections enabled).\n",
    "Install/upgrade RStudio or set RSTUDIO_PANDOC to a Pandoc 2+ directory."
  )
}

bookdown::render_book(
  "index.Rmd",
  output_format = "bookdown::gitbook",
  output_dir = "docs"
)

html_files <- list.files("docs", pattern = "\\.html$", full.names = TRUE)
empty_html <- html_files[file.size(html_files) == 0]
if (length(empty_html) > 0) {
  stop(
    "Render completed with empty HTML files:\n",
    paste(empty_html, collapse = "\n")
  )
}
