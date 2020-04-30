library(scales)
library(latex2exp)
library(ggplot2)
library(ggrepel)
library(kableExtra)


er <- element_rect(fill = "transparent", colour = "transparent")
global_theme <- ggthemes::theme_pander() +
  theme(rect = er, plot.background = er, panel.background = er,
        panel.border = er)
theme_set(global_theme)

options(knitr.table.format = "latex")
prefix <- tools::file_path_sans_ext(knitr::current_input())
fig_path <- paste0("_figures/", prefix, "-")
cache_path <- paste0("_cache/", prefix, "-")
knitr::opts_chunk$set(cache.path = cache_path, cache = TRUE, echo = TRUE,
                      background = '#FEF8DF', fig.path = fig_path,
                      fig.align = "center", fig.width = 4,
                      fig.height = 3, out.width = '50%')
# a common hook for messages, warnings and errors
optlist <- c("caption", "numbers", "basicstyle", "firstnumber", "style")
hook <- function(x, options, ..., begin = "lstlisting") {
  if (length(x) > 1) x <- paste(x, collapse = '\n')
  opts <- options[optlist]
  opts <- append(list(...), opts)
  opts[sapply(opts, is.null)] <- NULL
  opts <- ifelse(length(opts) < 1, "",
                 sprintf("[%s]",paste0(names(opts), "=", opts, collapse = ",")))
  knitr::raw_latex(
    sprintf("\n\\begin{%s}%s\n%s\n\\end{%s}\n",begin, opts, x, begin))
}
hook_lst_bf = function(x, options) {
  hook(x, options, basicstyle = "{\\bfseries}")
}
knitr::knit_hooks$set(source = function(x, options) {
  if (length(x) > 1) x <- paste(x, collapse = '\n')
  hook(x, options, begin = "rcode")
}, output = function(x, options) {
  if ("caption" %in% names(options)) options$caption = "\\mbox{}"
  options$firstnumber = 1
  x <- substr(x, 1, nchar(x) - 1)
  if (substr(x, 1, 4) == paste(options$comment,"\n")) x <- substr(x, 5, nchar(x))
  hook(x, options, begin = "outp")
}, warning = hook_lst_bf, message = hook_lst_bf, error = hook_lst_bf)

## empty highlight header since it is not useful any more
knitr::set_header(highlight = "")
