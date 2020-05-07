library(scales)
library(latex2exp)
library(ggplot2)
library(ggrepel)
library(xtable)
# https://bookdown.org/yihui/bookdown/custom-blocks.html

if (!exists("knitr_cache")) knitr_cache <- FALSE
# set ggplot theme
er <- element_rect(fill = "transparent", colour = "transparent")
global_theme <- ggthemes::theme_pander() +
  theme(rect = er, plot.background = er, panel.background = er,
        panel.border = er, panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
theme_set(global_theme)

prefix <- tools::file_path_sans_ext(knitr::current_input())
fig_path <- paste0("_figures/", prefix, "-")
cache_path <- paste0("_cache/", prefix, "-")

options(xtable.comment = FALSE)
options(width = 80)
knitr::opts_chunk$set(
  echo = TRUE,
  cache = knitr_cache,
  cache.path = cache_path,
  background = "#FEF8DF",
  fig.path = fig_path,
  fig.align = "center",
  out.width = "50%")

# a common hook for messages, warnings and errors
optlist <- c("caption", "numbers", "basicstyle", "firstnumber", "style")
hook <- function(x, options, ..., begin = "lstlisting") {
  if (length(x) > 1) x <- paste(x, collapse = "\n")
  opts <- options[optlist]
  opts <- append(list(...), opts)
  opts[sapply(opts, is.null)] <- NULL
  opts <- ifelse(length(opts) < 1, "",
                 sprintf("[%s]", paste0(names(opts), "=", opts,
                                        collapse = ",")))
  knitr::raw_latex(
    sprintf("\n\\begin{%s}%s\n%s\n\\end{%s}\n", begin, opts, x, begin))
}

hook_lst_bf <- function(x, options) {
  hook(x, options, basicstyle = "{\\bfseries}")
}

knitr::knit_engines$set(
  box = function(options) {
    type <- options$type
    options$type <- ifelse(length(type) == 0, "dbox", paste0(type, "box"))
    knitr::knit_engines$get("block")(options)
  },
  equation = function(options) {
    options$type <- "align*"
    knitr::knit_engines$get("block")(options)
  }
)

knitr::knit_hooks$set(source = function(x, options) {
  if (options$engine == "R") {
    if (length(x) > 1) x <- paste(x, collapse = "\n")
    return(hook(x, options, begin = "rcode"))
  }
  x
},
inline = function(x) {
  if (is.numeric(x)) x <- knitr:::round_digits(x)
  paste(as.character(x), collapse = ", ")
},
evaluate.inline = function(code, envir = knitr::knit_global()) {
  v <- withVisible(eval(xfun::parse_only(code), envir = envir))
  last_val <- v
  if (v$visible) {
    last_val <- knitr::knit_print(v$value, inline = TRUE,
                                  options = knitr::opts_chunk$get())
  }
  return(last_val)
},
chunk = function(x, options) {
  if (options$engine == "box") {
    return(knitr:::process_group.inline(knitr:::split_file(x)[[1]]))
  }
  x
},
output = function(x, options) {
  if (options$results == "asis") return(x)
  if ("caption" %in% names(options)) options$caption <- "\\mbox{}"
  options$firstnumber <- 1
  x <- substr(x, 1, nchar(x) - 1)
  if (substr(x, 1, 4) == paste(options$comment, "\n"))
    x <- substr(x, 5, nchar(x))
  hook(x, options, begin = "outp")
},
warning = hook_lst_bf,
message = hook_lst_bf,
error = hook_lst_bf)

## empty highlight header since it is not useful any more
knitr::set_header(highlight = "")

include_graphics <-
  function(path, auto_pdf = getOption("knitr.graphics.auto_pdf", FALSE),
           dpi = NULL, error = TRUE, latex_remove_dir = TRUE) {
  path <- knitr:::native_encode(path)
  if (auto_pdf && knitr::is_latex_output()) {
    path2 <- xfun::with_ext(path, "pdf")
    i <- file.exists(path2)
    if (latex_remove_dir) path2 <- basename(path2)
    path[i] <- path2[i]
  }
  if (error && length(p <- path[!knitr:::is_web_path(path) &
                                !file.exists(path)]))
    stop("Cannot find the file(s): ", paste0("\"", p, "\"",
                                             collapse = "; "))
  structure(path, class = c("knit_image_paths", "knit_asis"),
            dpi = dpi)
  }

print.df <- function(x, ...) {
  rws <- if (nrow(x) < 3) 1 else seq(1, (nrow(x) - 1), by = 2)
  if (nrow(x) < 2) rws <- 0
  col <- rep("\\rowcolor[gray]{0.95}", length(rws))
  print(xtable::xtable(x), booktabs = TRUE,
        add.to.row = list(pos = as.list(rws), command = col), ...)
}
