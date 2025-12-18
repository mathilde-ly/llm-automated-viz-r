
##' Créer un scatterplot enrichi avec analyse LLM
##'
##' @param data Un data.frame contenant les données.
##' @param x_col Nom de la colonne numérique en abscisse.
##' @param y_col Nom de la colonne numérique en ordonnée.
##' @param context_description Contexte optionnel pour l'analyse (NULL par défaut).
##'
##' @return Un objet de classe scatterplot_context.
##' @export
scatterplot_context <- function(data, x_col, y_col, context_description = NULL) {
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
	if (!is.numeric(data[[x_col]])) {
		stop(paste("La colonne", x_col, "doit être numérique."))
	}
	if (!is.numeric(data[[y_col]])) {
		stop(paste("La colonne", y_col, "doit être numérique."))
	}

	# Nettoyage des NA
	data <- data[!is.na(data[[x_col]]) & !is.na(data[[y_col]]), ]
	if (nrow(data) == 0) {
		stop("Aucune donnée exploitable après suppression des NA.")
	}

	# Calcul des statistiques descriptives
	stats_x <- summary(data[[x_col]])
	stats_y <- summary(data[[y_col]])
	cor_xy <- suppressWarnings(cor(data[[x_col]], data[[y_col]], use = "complete.obs"))

	# Construction du prompt pour le LLM
	prompt <- construire_prompt_scatter(stats_x, stats_y, cor_xy, x_col, y_col, context_description)

	# Appel au LLM Ollama
	reponse_llm <- interroger_ollama(prompt)

	# Construction de l'objet S3
	resultat <- list(
		data = data,
		x_col = x_col,
		y_col = y_col,
		context_description = context_description,
		stats_x = stats_x,
		stats_y = stats_y,
		cor_xy = cor_xy,
		titre = reponse_llm$titre,
		interpretation = reponse_llm$paragraphe
	)
	class(resultat) <- "scatterplot_context"
	return(resultat)
}

##' Construire le prompt pour le LLM (scatterplot)
##'
##' @param stats_x Résumé statistique de x.
##' @param stats_y Résumé statistique de y.
##' @param cor_xy Corrélation entre x et y.
##' @param x_col Nom de la variable x.
##' @param y_col Nom de la variable y.
##' @param context_description Contexte optionnel fourni par l'utilisateur.
##'
##' @return Une chaîne de caractères contenant le prompt.
##' @keywords internal
construire_prompt_scatter <- function(stats_x, stats_y, cor_xy, x_col, y_col, context_description) {
	resume_stats <- paste0(
		"Statistiques descriptives :\n",
		"- ", x_col, " : min=", round(stats_x[["Min."]], 2), ", max=", round(stats_x[["Max."]], 2), ", moy=", round(stats_x[["Mean"]], 2), ", médiane=", round(stats_x[["Median"]], 2), "\n",
		"- ", y_col, " : min=", round(stats_y[["Min."]], 2), ", max=", round(stats_y[["Max."]], 2), ", moy=", round(stats_y[["Mean"]], 2), ", médiane=", round(stats_y[["Median"]], 2), "\n",
		"Corrélation (Pearson) entre ", x_col, " et ", y_col, " : ", round(cor_xy, 3), "\n"
	)
	if (is.null(context_description)) {
		prompt <- paste0(
			resume_stats,
			"En te basant uniquement sur ces statistiques, en français, génère :\n",
			"1. Un titre court et informatif pour le graphique (max 10 mots)\n",
			"2. Un paragraphe d'analyse statistique objective (3-4 phrases) décrivant la dispersion, la relation entre les deux variables, la force et le sens de la corrélation, et toute tendance ou anomalie notable.\n\n",
			"Réponds au format JSON strict suivant :\n",
			'{"titre": "ton titre", "paragraphe": "ton analyse"}'
		)
	} else {
		prompt <- paste0(
			"Contexte : ", context_description, "\n\n",
			resume_stats,
			"En te basant sur ces statistiques ET le contexte fourni, génère :\n",
			"1. Un titre court et informatif pour le graphique (max 10 mots)\n",
			"2. Un paragraphe d'interprétation (3-4 phrases) décrivant la dispersion, la relation entre les deux variables, la force et le sens de la corrélation, et toute tendance ou anomalie notable et qui lie les observations statistiques au contexte métier/scientifique fourni.\n\n",
			"Réponds au format JSON strict suivant :\n",
			'{"titre": "ton titre", "paragraphe": "ton interprétation"}'
		)
	}
	return(prompt)
}

