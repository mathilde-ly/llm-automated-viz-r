print.ggplot_context <- function(x, ...) {
  cat("=== Interprétation du graphique ===\n")
  cat(x$interpretation, "\n\n")
  print(x$plot)
  invisible(x)
}
