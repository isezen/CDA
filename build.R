library(knitr)
library(rmarkdown)

render_rmd <- function(input = NULL, output_format = NULL,
                       output_dir = "_pdf_files/", ..., cache = FALSE) {
  knitr_cache <<- cache
  if (endsWith(output_dir, "/")) output_dir <- substr(output_dir, 1, nchar(output_dir) - 1)
  if (!cache) unlink("_cache", recursive = TRUE)
  dir.create(output_dir, showWarnings = FALSE)
  files <- if (is.null(input)) list.files(pattern = '\\.Rmd$') else input
  for (file in files) {
    r <- render(file, output_format, output_dir = output_dir, ...,
                quiet = TRUE)
    cat("Rendered:", r, "\n")
  }
  files_to_copy <- c("images", "_figures", "template", "_output.yml",
                     "_beamer.yml", "bibliography.bib")
  invisible(lapply(c("md", "tex"), function(p) {
    file <- list.files(output_dir, pattern = sprintf('\\.%s$', p),
                       full.names = TRUE)
    td <- sprintf(paste0(output_dir, "_%s_files/"), p)
    if (length(file) > 0) {
      dir.create(td, showWarnings = FALSE)
      file.copy(file, td)
      file.remove(file)
      file.copy(files_to_copy, td, recursive = TRUE)
    } else {
      unlink(td, recursive = TRUE)
      unlink(paste0(output_dir, "/*.", p))
    }
  }))
}

buildbeamer <- function(input = NULL, ...) {
  render_rmd(input, "beamer_presentation", "_presentation", ...)
}

buildpdf <- function(input = NULL, ...) {
  render_rmd(input, "pdf_document", "_pdf", ...)
}

