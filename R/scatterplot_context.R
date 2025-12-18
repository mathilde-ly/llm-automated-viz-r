
##' Créer un scatterplot enrichi avec analyse LLM
##'
##' Construit un objet `scatterplot_context` à partir de deux variables
##' numériques (X et Y), calcule des résumés statistiques et la corrélation
##' de Pearson, puis interroge un LLM (via Ollama) pour proposer un titre
##' et une interprétation du nuage de points.
##'
##' @param data Un data.frame contenant les données.
##' @param x_col Nom de la colonne numérique en abscisse.
##' @param y_col Nom de la colonne numérique en ordonnée.
##' @param instruction Contexte optionnel pour l'analyse (NULL par défaut).
##' @param model Modèle LLM utilisé pour générer le contexte.
##' @param temperature Température utilisée pour la génération du texte.
##' @param seed Graine aléatoire pour la reproductibilité.
##'
##' @return Un objet de classe `scatterplot_context` avec statistiques,
##' corrélation, titre et interprétation.
##'
##' @details
##' - Valide que `x_col` et `y_col` sont numériques et présents.
##' - Supprime les observations avec `NA` sur X ou Y.
##' - Calcule `cor()` avec `use = "complete.obs"`.
##' - Appelle `interroger_ollama()` (serveur Ollama requis).
##'
##' @seealso [plot.scatterplot_context] pour le tracé et
##' [print.scatterplot_context] pour l'affichage textuel.
##'
##' @examples
##' \dontrun{
##' # Exemple avec mtcars
##' data(mtcars)
##' sc <- scatterplot_context(mtcars, x_col = "wt", y_col = "mpg")
##' print(sc)
##' plot(sc)
##' }
##'
##' @export
scatterplot_context <- function(data, x_col, y_col, instruction = NULL,
							   model = "mistral", temperature = NULL, seed = NULL) {
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
	prompt <- construire_prompt_scatter(stats_x, stats_y, cor_xy, x_col, y_col, instruction)

	# Appel au LLM Ollama
	reponse_llm <- interroger_ollama(prompt, model = model, temperature = temperature, seed = seed)

	# Construction de l'objet S3
	resultat <- list(
		data = data,
		x_col = x_col,
		y_col = y_col,
		instruction = instruction,
		stats_x = stats_x,
		stats_y = stats_y,
		cor_xy = cor_xy,
		titre = reponse_llm$titre,
		interpretation = reponse_llm$paragraphe,
		llm_model = model,
		llm_temperature = temperature,
		llm_seed = seed
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
##' @param instruction Contexte optionnel fourni par l'utilisateur.
##'
##' @return Une chaîne de caractères contenant le prompt.
##' @keywords internal
construire_prompt_scatter <- function(stats_x, stats_y, cor_xy, x_col, y_col, instruction) {
	resume_stats <- paste0(
		"Statistiques descriptives :\n",
		"- ", x_col, " : min=", round(stats_x[["Min."]], 2), ", max=", round(stats_x[["Max."]], 2), ", moy=", round(stats_x[["Mean"]], 2), ", médiane=", round(stats_x[["Median"]], 2), "\n",
		"- ", y_col, " : min=", round(stats_y[["Min."]], 2), ", max=", round(stats_y[["Max."]], 2), ", moy=", round(stats_y[["Mean"]], 2), ", médiane=", round(stats_y[["Median"]], 2), "\n",
		"Corrélation (Pearson) entre ", x_col, " et ", y_col, " : ", round(cor_xy, 3), "\n"
	)
	if (is.null(instruction)) {
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
			"Contexte : ", instruction, "\n\n",
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

