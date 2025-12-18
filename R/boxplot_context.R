##' Créer un boxplot enrichi avec analyse LLM
##'
##' @param data Un data.frame contenant les données.
##' @param x_col Nom de la colonne catégorielle (variable explicative).
##' @param y_col Nom de la colonne numérique (variable expliquée).
##' @param context_description Contexte optionnel pour l'analyse (NULL par défaut).
##'
##' @return Un objet de classe boxplot_context.
##' @export
boxplot_context <- function(data, x_col, y_col, context_description = NULL) {
  # Validation des entrées
  if (!is.data.frame(data)) {
    stop("data doit être un data.frame")
  }

  if (!x_col %in% names(data)) {
    stop(paste("La colonne", x_col, "n'existe pas dans les données"))
  }

  if (!y_col %in% names(data)) {
    stop(paste("La colonne", y_col, "n'existe pas dans les données"))
  }

  # Nettoyage des NA dans la colonne numérique
  data <- data[!is.na(data[[y_col]]), ]
  if (nrow(data) == 0) {
    stop("Aucune donnée exploitable après suppression des NA dans la colonne numérique.")
  }

  # Conversion automatique de la colonne de groupement en facteur si nécessaire
  if (!is.factor(data[[x_col]])) {
    data[[x_col]] <- as.factor(data[[x_col]])
    warning(paste("La variable", x_col, "a été convertie en facteur pour le boxplot."))
  }

  if (!is.numeric(data[[y_col]])) {
    stop(paste("La colonne", y_col, "doit être numérique."))
  }

  # Calcul des statistiques descriptives
  stats_summary <- calculer_statistiques(data, x_col, y_col)

  # Avertissement pour groupes avec peu d'observations
  groupes_faibles <- names(stats_summary$par_groupe)[sapply(stats_summary$par_groupe, function(g) g$n < 3)]
  if (length(groupes_faibles) > 0) {
    warning(paste("Certains groupes ont moins de 3 valeurs :", paste(groupes_faibles, collapse = ", "), ". Statistiques peu fiables."))
  }

  # Construction du prompt pour le LLM
  prompt <- construire_prompt(stats_summary, x_col, y_col, context_description)

  # Appel au LLM Ollama
  reponse_llm <- interroger_ollama(prompt)

  # Construction de l'objet S3
  resultat <- list(
    data = data,
    x_col = x_col,
    y_col = y_col,
    context_description = context_description,
    statistiques = stats_summary,
    titre = reponse_llm$titre,
    interpretation = reponse_llm$paragraphe
  )

  class(resultat) <- "boxplot_context"

  return(resultat)
}

##' Calculer les statistiques descriptives par groupe
##'
##' @param data Le data.frame des données à plot.
##' @param x_col Nom de la colonne de groupement.
##' @param y_col Nom de la colonne numérique.
##'
##' @return Une liste contenant les statistiques par groupe.
##' @keywords internal
calculer_statistiques <- function(data, x_col, y_col) {
  # Statistiques globales
  stats_globales <- summary(data[[y_col]])

  # Statistiques par groupe
  groupes <- unique(data[[x_col]])
  stats_par_groupe <- lapply(groupes, function(groupe) {
    sous_donnees <- data[data[[x_col]] == groupe, y_col]
    # Conversion en numérique et suppression des NA
    sous_donnees_num <- suppressWarnings(as.numeric(sous_donnees))
    sous_donnees_num <- sous_donnees_num[!is.na(sous_donnees_num)]
    if (length(sous_donnees_num) == 0) {
      return(list(
        groupe = as.character(groupe),
        n = 0,
        min = NA,
        q1 = NA,
        mediane = NA,
        moyenne = NA,
        q3 = NA,
        max = NA,
        ecart_type = NA
      ))
    }
    list(
      groupe = as.character(groupe),
      n = length(sous_donnees_num),
      min = min(sous_donnees_num, na.rm = TRUE),
      q1 = quantile(sous_donnees_num, 0.25, na.rm = TRUE),
      mediane = median(sous_donnees_num, na.rm = TRUE),
      moyenne = mean(sous_donnees_num, na.rm = TRUE),
      q3 = quantile(sous_donnees_num, 0.75, na.rm = TRUE),
      max = max(sous_donnees_num, na.rm = TRUE),
      ecart_type = sd(sous_donnees_num, na.rm = TRUE)
    )
  })

  names(stats_par_groupe) <- as.character(groupes)

  return(list(
    globales = stats_globales,
    par_groupe = stats_par_groupe
  ))
}

##' Construire le prompt pour le LLM
##'
##' @param stats_summary Liste des statistiques calculées.
##' @param x_col Nom de la variable explicative.
##' @param y_col Nom de la variable expliquée.
##' @param context_description Contexte optionnel fourni par l'utilisateur (str).
##'
##' @return Une chaîne de caractères contenant le prompt.
##' @keywords internal
construire_prompt <- function(stats_summary, x_col, y_col, context_description) {
  # Construction du résumé statistique textuel
  resume_stats <- paste0(
    "Statistiques descriptives pour la variable '", y_col,
    "' en fonction de '", x_col, "':\n\n"
  )

  for (groupe_nom in names(stats_summary$par_groupe)) {
    groupe <- stats_summary$par_groupe[[groupe_nom]]
    resume_stats <- paste0(
      resume_stats,
      "Groupe '", groupe_nom, "' (n=", groupe$n, "):\n",
      "  - Minimum: ", round(groupe$min, 2), "\n",
      "  - Q1: ", round(groupe$q1, 2), "\n",
      "  - Médiane: ", round(groupe$mediane, 2), "\n",
      "  - Moyenne: ", round(groupe$moyenne, 2), "\n",
      "  - Q3: ", round(groupe$q3, 2), "\n",
      "  - Maximum: ", round(groupe$max, 2), "\n",
      "  - Écart-type: ", round(groupe$ecart_type, 2), "\n\n"
    )
  }

  # Construction du prompt selon le contexte
  if (is.null(context_description)) {
    prompt <- paste0(
      resume_stats,
      "En te basant uniquement sur ces statistiques, en français, génère:\n",
      "1. Un titre court et informatif pour le graphique (maximum 10 mots)\n",
      "2. Un paragraphe d'analyse statistique objective (3-4 phrases) décrivant ",
      "les différences entre groupes, la dispersion des données et les tendances observées.\n\n",
      "Réponds au format JSON strict suivant:\n",
      '{"titre": "ton titre", "paragraphe": "ton analyse"}'
    )
  } else {
    prompt <- paste0(
      "Contexte: ", context_description, "\n\n",
      resume_stats,
      "En te basant sur ces statistiques ET le contexte fourni, génère:\n",
      "1. Un titre court et informatif pour le graphique (maximum 10 mots)\n",
      "2. Un paragraphe d'interprétation (3-4 phrases) qui lie les observations ",
      "statistiques au contexte métier/scientifique fourni.\n\n",
      "Réponds au format JSON strict suivant:\n",
      '{"titre": "ton titre", "paragraphe": "ton interprétation"}'
    )
  }

  return(prompt)
}
