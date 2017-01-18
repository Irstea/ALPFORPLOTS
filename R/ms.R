## This is a temporary helper until first class rendering is done:
render_html <- function(filename_md) {
  rmarkdown::render(filename_md, "html_document", quiet=TRUE)
}

## This is a temporary helper until first class rendering is done:
render_doc <- function(filename_md) {
  rmarkdown::render(filename_md, "word_document", quiet=TRUE)
}

## This is a temporary helper until first class rendering is done:
render_pdf <- function(filename_md) {
  rmarkdown::render(filename_md, "pdf_document", quiet=TRUE)
}
