##' Créer un summary contextualisé avec analyse LLM
##'
##' Génère un objet `summary_context` qui résume chaque variable d'un
##' `data.frame` (statistiques pour numériques, modalités pour caractères,
##' comptages pour logiques), puis interroge un LLM (via Ollama) pour
##' fournir un titre et une interprétation globale du jeu de données.
##'
##' @param data Un data.frame contenant les données.
##'
##' @return Un objet de classe `summary_context` contenant les informations
##' par variable, le nombre de `NA` et l'interprétation du LLM.
##'
##' @details
##' - Détecte le type des variables via `get_variable_type()`.
##' - Calcule des résumés adaptés au type.
##' - Appelle `interroger_ollama()` (serveur Ollama requis).
##'
##' @seealso [print.summary_context] pour l'affichage textuel.
##'
##' @examples
##' \dontrun{
##' data(mtcars)
##' sm <- summary_context(mtcars)
##' print(sm)
##' }
##'
##' @export

summary_context <- function(data) {
  # Validation du format de l'entrée
  if (!is.data.frame(data)) {
    stop("L'objet fourni doit être un data.frame")
  }

  # Obtenir le nom des variables
  vars <- names(data)

  # Identifier le type de la variable entre numérique et nominale
  var_type <- sapply(data, get_variable_type)

  # Les informations sorties dans le summary selon le type de la variable
  infos <- lapply(data, function(x) {
    if (is.integer(x) || is.double(x)) {

      list(
        min = min(x, na.rm = TRUE),
        max = max(x, na.rm = TRUE),
        mean = mean(x, na.rm = TRUE),
        median = median(x, na.rm = TRUE),
        q1 = quantile(x, 0.25, na.rm = TRUE),
        q3 = quantile(x, 0.75, na.rm = TRUE)
      )

    } else if (is.character(x)) {

      list(
        valeurs = unique(na.omit(x)),
        nb_modalites = length(unique(na.omit(x)))
      )

    } else if (is.logical(x)) {

      list(
        nb_TRUE = sum(x == TRUE, na.rm = TRUE),
        nb_FALSE = sum(x == FALSE, na.rm = TRUE)
      )

    } else {

      list(info = "Type non pris en charge")

    }
  })

  # Calculer le nombre de données manquantes dans le jeu de données
  nb_na <- sum(is.na(data))

  # Construction de l'objet S3
  resultat <- list(
    variables = vars,
    type = var_type,
    infos = infos,
    nb_na = nb_na,
    llm_interpretation = NULL
  )

  # Création de la classe de l'objet S3
  class(resultat) <- "summary_context"

  # Construction du prompt pour le LLM
  prompt_summary <- construire_prompt_summary(resultat)

  # Ajout de l'interprétation du LLM à l'objet S3
  resultat$llm_interpretation <- interroger_ollama(prompt_summary)

  return(resultat)
}

##' Identifier le type de la variable
##'
##' @param x une variable.
##'
##' @return Un libellé de type pour la variable (`integer`, `numeric`,
##' `character`, `logical` ou première classe).
##' @keywords internal
get_variable_type <- function(x) {
  if (is.integer(x)) {
    "integer"
  } else if (is.double(x)) {
    "numeric"
  } else if (is.character(x)) {
    "character"
  } else if (is.logical(x)) {
    "logical"
  } else {
    class(x)[1]
  }
}

##' Construire le prompt pour le LLM
##'
##' @param summary_obj Un objet de la classe summary_context.
##'
##' @return Une chaîne de caractères contenant le prompt.
##' @keywords internal

construire_prompt_summary <- function(summary_obj) {

  stopifnot(inherits(summary_obj, "summary_context"))

  var_descriptions <- mapply(
    FUN = function(var, type, info) {

      if (type %in% c("integer", "numeric")) {

        paste0(
          "- ", var, " (", type, ") : ",
          "min=", round(info$min, 2), ", ",
          "max=", round(info$max, 2), ", ",
          "moyenne=", round(info$mean, 2), ", ",
          "médiane=", round(info$median, 2), ", ",
          "Q1=", round(info$q1, 2), ", ",
          "Q3=", round(info$q3, 2)
        )

      } else if (type == "character") {

        paste0(
          "- ", var, " (character) : ",
          info$nb_modalites, " modalités (",
          paste(info$valeurs, collapse = ", "),
          ")"
        )

      } else if (type == "logical") {

        paste0(
          "- ", var, " (logical) : ",
          "TRUE=", info$nb_TRUE, ", ",
          "FALSE=", info$nb_FALSE
        )

      } else {

        paste0(
          "- ", var, " (", type, ") : type non pris en charge"
        )
      }

    },
    var = summary_obj$variables,
    type = summary_obj$type,
    info = summary_obj$infos,
    SIMPLIFY = TRUE
  )

  prompt_summary <- paste(
    "Voici un résumé statistique d'un jeu de données.",
    "",
    "Description des variables :",
    paste(var_descriptions, collapse = "\n"),
    "",
    paste0("Nombre total de données manquantes : ", summary_obj$nb_na, "."),
    "",
    "Tâche :",
    "1. Interpréter le rôle possible de chaque variable.",
    "2. Décrire les relations potentielles entre les variables.",
    "3. Signaler les problèmes éventuels (valeurs manquantes, distributions suspectes).",
    "",
    "Réponds en français, de manière claire et concise au format JSON strict suivant:\n",
    '{"titre": "ton titre", "paragraphe": "ton analyse"}',
    sep = "\n"
  )

  return(prompt_summary)
}
