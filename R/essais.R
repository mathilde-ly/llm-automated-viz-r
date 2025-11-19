data(iris)
library(ggplot2)
library(jsonlite)


res <- ggplot_context(
  data = iris,
  mapping = ggplot2::aes(x = Species, y = Sepal.Length),
  context = "Étude des variations de longueur de sépale selon les espèces."
)

print(res)
