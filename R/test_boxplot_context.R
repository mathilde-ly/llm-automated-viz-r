# Test local des fonctions sans charger le package

# Sourcing direct des fonctions
source("R/boxplot_context.R")
source("R/ollama_api.R")
source("R/methods.R")

# Jeu de données iris
boxplot_context(iris, x_col = "Species", y_col = "Sepal.Length")
print(res_iris)
plot(res_iris)

# Jeu de données penguins (si disponible)
if (requireNamespace("palmerpenguins", quietly = TRUE)) {
  library(palmerpenguins)
  res_penguins <- boxplot_context(penguins, x_col = "species", y_col = "bill_length_mm")
  print(res_penguins$titre)
  print(res_penguins$interpretation)
} else {
  message("Le package palmerpenguins n'est pas installé.")
}
