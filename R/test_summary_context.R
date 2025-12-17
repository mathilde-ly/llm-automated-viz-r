# Sourcing direct des fonctions
source("R/summary_context.R")
source("R/ollama_api.R")
source("R/methods.R")


df <- data.frame(
  age = c(20L, 30L, 40L, NA),
  taille = c(1.65, 1.80, 1.75, 1.70),
  sexe = c("F", "H", "F", "H"),
  fumeur = c(TRUE, FALSE, FALSE, TRUE)
)

res <- summary_context(df)
print(res)
