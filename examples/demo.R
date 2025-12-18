# exemple d'utilisation du package
library(lamaplot.context)

# --- TESTEZ VOTRE CONNEXION A OLLAMA ---

# Notre package utilise le port et le modele suivant, ne le changez pas sauf si vous êtes un professionel
test_ollama_connection <- function(endpoint = "http://localhost:11434", model = "mistral") {
  url <- paste0(endpoint, "/api/tags")
  res <- tryCatch(
    httr::GET(url),
    error = function(e) return(NULL)
  )
  if (is.null(res) || httr::http_error(res)) {
    message(":c mpossible de contacter Ollama à l'adresse : ", endpoint)
    return(FALSE)
  }
  tags <- tryCatch(httr::content(res, as = "parsed"), error = function(e) NULL)
  model_names <- unlist(lapply(tags$models, function(m) m$name))
  if (!is.null(tags) && any(grepl(model, model_names, ignore.case = TRUE))) {
    message(":D Connexion à Ollama OK. Modèle '", model, "' disponible.")
    return(TRUE)
  } else {
    message("!! Connexion à Ollama OK, mais le modèle '", model, "' n'est pas disponible.")
    return(FALSE)
  }
}

test_ollama_connection()
# En cas de problème, réferrez vous à la documentation officielle Ollama : https://docs.ollama.com/

# --- TESTEZ LE BOXPLOT ---

res_iris <- boxplot_context(iris, x_col = "Species", y_col = "Sepal.Length")
print(res_iris)
plot(res_iris)

# --- TESTEZ LE SCATTERPLOT AVEC ET SANS INSTRUCTIONS SUPPLEMENTAIRES ---
nuage_cars <- scatterplot_context(mtcars, x_col = "wt", y_col = "mpg")
print(nuage_cars)
plot(nuage_cars)

nuage_cars_description <- scatterplot_context(mtcars, x_col = "wt", y_col = "mpg", instruction = "Analyse de la relation entre le poids et la consommation de carburant des voitures.")
print(nuage_cars_description)
plot(nuage_cars_description)

# --- TESTEZ LE SUMMARY ---

df <- data.frame(
  age = c(20L, 30L, 40L, NA),
  taille = c(1.65, 1.80, 1.75, 1.70),
  sexe = c("F", "H", "F", "H"),
  fumeur = c(TRUE, FALSE, FALSE, TRUE)
)

res_summary <- summary_context(df)
print(res_summary)
