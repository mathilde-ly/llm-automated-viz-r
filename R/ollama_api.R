#' Interroger l'API Ollama
#'
#' @param prompt Le prompt à envoyer au modèle
#' @param model Nom du modèle (par défaut "mistral")
#' @param endpoint URL de l'API Ollama (par défaut "http://localhost:11434")
#'
#' @return Une liste contenant titre et paragraphe
#' @keywords internal
interroger_ollama <- function(prompt,
                               model = "mistral",
                               endpoint = "http://localhost:11434") {

  # Construction de l'URL complète
  url <- paste0(endpoint, "/api/generate")

  # Préparation du corps de la requête
  body <- list(
    model = model,
    prompt = prompt,
    stream = FALSE,
    format = "json"
  )

  # Appel à l'API avec gestion d'erreur
  tryCatch({
    response <- httr::POST(
      url = url,
      body = jsonlite::toJSON(body, auto_unbox = TRUE),
      httr::content_type_json(),
      httr::timeout(120)
    )

    # Vérification du statut HTTP
    if (httr::http_error(response)) {
      stop(paste(
        "Erreur API Ollama:",
        httr::status_code(response),
        "-",
        httr::content(response, as = "text", encoding = "UTF-8")
      ))
    }

    # Extraction et parsing de la réponse
    contenu <- httr::content(response, as = "text", encoding = "UTF-8")
    reponse_json <- jsonlite::fromJSON(contenu)

    # Parsing de la réponse générée
    reponse_llm <- parser_reponse_llm(reponse_json$response)

    return(reponse_llm)

  }, error = function(e) {
    stop(paste(
      "Impossible de contacter Ollama :c",
      e$message,
      "\nVérifiez qu'Ollama est démarré et que le modèle mistral est disponible sur le port 11434.\nSi l'erreur persiste, redémarrez votre session R et croisez les doigts."
    ))
  })
}

#' Parser la réponse du LLM
#'
#' @param reponse_brute Chaîne de caractères JSON retournée par le LLM
#'
#' @return Une liste avec titre et paragraphe
#' @keywords internal
parser_reponse_llm <- function(reponse_brute) {
  tryCatch({
    # Tentative de parsing JSON direct
    parsed <- jsonlite::fromJSON(reponse_brute)

    # Validation de la structure
    if (!all(c("titre", "paragraphe") %in% names(parsed))) {
      stop("Structure JSON invalide")
    }

    return(list(
      titre = as.character(parsed$titre),
      paragraphe = as.character(parsed$paragraphe)
    ))

  }, error = function(e) {
    # Fallback en cas d'erreur de parsing
    warning("Le LLM n'a pas retourné un JSON valide. Utilisation de valeurs par défaut.")
    return(list(
      titre = "Analyse des données",
      paragraphe = paste(
        "L'analyse automatique a rencontré un problème.",
        "Réponse brute du modèle:",
        substr(reponse_brute, 1, 200)
      )
    ))
  })
}
