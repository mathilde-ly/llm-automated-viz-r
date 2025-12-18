# ========== METHODES POUR BOXPLOT CONTEXT ==============

##' Afficher l'interprétation d'un objet boxplot_context
##'
##' Affiche le titre généré par le LLM et l'interprétation associée,
##' ainsi que quelques métadonnées (variables, nombre de groupes).
##'
##' @param x Un objet de classe boxplot_context.
##' @param ... Arguments supplémentaires (non utilisés).
##'
##' @seealso [boxplot_context] pour créer l'objet et [plot.boxplot_context]
##' pour le tracé.
##'
##' @examples
##' \dontrun{
##' data(iris)
##' bx <- boxplot_context(iris, "Species", "Sepal.Length")
##' print(bx)
##' }
##'
##' @export
print.boxplot_context <- function(x, ...) {
  cat("\n")
  cat("========================================\n")
  cat("  ANALYSE BOXPLOT AVEC CONTEXTE LLM\n")
  cat("========================================\n\n")

  cat("Variable analysée:", x$y_col, "\n")
  cat("Variable de groupement:", x$x_col, "\n")
  cat("Nombre de groupes:", length(x$statistiques$par_groupe), "\n\n")

  if (!is.null(x$context_description)) {
    cat("Contexte fourni:\n")
    cat(strwrap(x$context_description, width = 70, prefix = "  "), sep = "\n")
    cat("\n\n")
  }

  cat("----------------------------------------\n")
  cat("INTERPRÉTATION GÉNÉRÉE PAR L'IA:\n")
  cat("----------------------------------------\n\n")

  # Formatage du paragraphe avec retour à la ligne
  paragraphe_formate <- strwrap(x$interpretation, width = 70, prefix = "")
  cat(paragraphe_formate, sep = "\n")
  cat("\n\n")

  cat("Utilisez plot() pour visualiser le graphique.\n")
  cat("\n")

  invisible(x)
}

##' Tracer le boxplot d'un objet boxplot_context
##'
##' @param x Un objet de classe boxplot_context.
##' @param ... Arguments supplémentaires passés à ggplot2.
##'
##' @return Un objet ggplot.
##'
##' @details Nécessite le package `ggplot2`.
##'
##' @seealso [boxplot_context] pour construire l'objet et
##' [print.boxplot_context] pour l'affichage textuel.
##'
##' @examples
##' \dontrun{
##' data(iris)
##' bx <- boxplot_context(iris, "Species", "Sepal.Length")
##' plot(bx)
##' }
##'
##' @export
plot.boxplot_context <- function(x, ...) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Le package ggplot2 est requis pour cette fonction.")
  }

  # Vérification du type de la variable de groupement
  if (!is.factor(x$data[[x$x_col]])) {
    x$data[[x$x_col]] <- as.factor(x$data[[x$x_col]])
    warning(paste("La variable", x$x_col, "a été convertie en facteur pour le boxplot."))
  }

  # Création du graphique de base
  p <- ggplot2::ggplot(
    x$data,
    ggplot2::aes(x = .data[[x$x_col]], y = .data[[x$y_col]])
  ) +
    ggplot2::geom_boxplot(fill = "lightblue", alpha = 0.7) +
    ggplot2::theme_minimal() +
    ggplot2::labs(
      title = x$titre,
      x = x$x_col,
      y = x$y_col
    ) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5, face = "bold", size = 14),
      axis.title = ggplot2::element_text(size = 12),
      axis.text = ggplot2::element_text(size = 10)
    )


  # Avertissement si moins de 2 groupes
  if (length(unique(x$data[[x$x_col]])) < 2) {
    warning("Le boxplot nécessite au moins deux groupes.")
  }


  return(p)
}

# ======== METHODES POUR SCATTERPLOT CONTEXT ===================


##' Méthode print pour scatterplot_context
##'
##' @param x Un objet de classe scatterplot_context.
##' @param ... Arguments supplémentaires (non utilisés).
##'
##' @seealso [scatterplot_context] pour créer l'objet et
##' [plot.scatterplot_context] pour le tracé.
##'
##' @examples
##' \dontrun{
##' data(mtcars)
##' sc <- scatterplot_context(mtcars, "wt", "mpg")
##' print(sc)
##' }
##'
##' @export
print.scatterplot_context <- function(x, ...) {
  cat("\n")
  cat("========================================\n")
  cat("  ANALYSE SCATTERPLOT AVEC CONTEXTE LLM\n")
  cat("========================================\n\n")

  cat("Variables analysées :", x$x_col, "(X) et", x$y_col, "(Y)\n")
  cat("Corrélation (Pearson) :", round(x$cor_xy, 3), "\n\n")

  if (!is.null(x$context_description)) {
    cat("Contexte fourni :\n")
    cat(strwrap(x$context_description, width = 70, prefix = "  "), sep = "\n")
    cat("\n\n")
  }

  cat("----------------------------------------\n")
  cat("INTERPRÉTATION GÉNÉRÉE PAR L'IA :\n")
  cat("----------------------------------------\n\n")

  paragraphe_formate <- strwrap(x$interpretation, width = 70, prefix = "")
  cat(paragraphe_formate, sep = "\n")
  cat("\n\n")

  cat("Utilisez plot() pour visualiser le graphique.\n")
  cat("\n")
  invisible(x)
}

##' Méthode plot pour scatterplot_context
##'
##' @param x Un objet de classe scatterplot_context.
##' @param ... Arguments supplémentaires passés à ggplot2.
##'
##' @return Un objet ggplot.
##'
##' @details Nécessite le package `ggplot2`.
##'
##' @seealso [scatterplot_context] pour construire l'objet et
##' [print.scatterplot_context] pour l'affichage.
##'
##' @examples
##' \dontrun{
##' data(mtcars)
##' sc <- scatterplot_context(mtcars, "wt", "mpg")
##' plot(sc)
##' }
##'
##' @export
plot.scatterplot_context <- function(x, ...) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Le package ggplot2 est requis pour cette fonction.")
  }
  p <- ggplot2::ggplot(
    x$data,
    ggplot2::aes(x = .data[[x$x_col]], y = .data[[x$y_col]])
  ) +
    ggplot2::geom_point(color = "lightgreen", alpha = 0.7, size = 2) +
    ggplot2::geom_smooth(method = "lm", se = FALSE, color = "purple", linetype = "dashed", size = 1) +
    ggplot2::theme_minimal() +
    ggplot2::labs(
      title = x$titre,
      x = x$x_col,
      y = x$y_col
    ) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5, face = "bold", size = 14),
      axis.title = ggplot2::element_text(size = 12),
      axis.text = ggplot2::element_text(size = 10)
    )
  return(p)
}


##' Afficher le résumé d'un objet summary_context
##'
##' Affiche un tableau synthétique des variables (type et résumé),
##' le total de données manquantes et, si disponible, l'interprétation
##' produite par le LLM.
##'
##' @param x Un objet de classe summary_context.
##' @param ... Arguments supplémentaires (non utilisés).
##'
##' @seealso [summary_context] pour créer l'objet.
##'
##' @export
print.summary_context <- function(x, ...) {

  cat("\n")
  cat("                        Résumé des variables\n")
  cat("======================================================================\n")

  # 1. Configuration de la largeur des colonnes
  # On définit une largeur fixe pour chaque colonne (ex: 18 caractères)
  col_width <- 18

  # 2. Préparation des données ligne par ligne
  # On transforme les listes d'infos en un format "grille" (4 lignes max par variable)

  info_grille <- mapply(FUN = function(info, type) {
    if (type %in% c("integer", "numeric")) {
      res <- c(
        paste0("min=", round(info$min, 2)),
        paste0("max=", round(info$max, 2)),
        paste0("moy=", round(info$mean, 2)),
        paste0("med=", round(info$median, 2))
      )
    } else if (type == "character" || type == "factor") {
      # On prend les premières valeurs pour ne pas déborder verticalement
      vals <- paste(info$valeurs, collapse = ", ")
      # On tronque si c'est trop long pour l'esthétique
      if(nchar(vals) > col_width - 2) vals <- paste0(substr(vals, 1, col_width - 5), "...")
      res <- c(vals, "", "", "")
    } else if (type == "logical") {
      res <- c(
        paste0("TRUE=", info$nb_TRUE),
        paste0("FALSE=", info$nb_FALSE),
        "", ""
      )
    } else {
      res <- c("Non supporté", "", "", "")
    }
    return(res)
  }, info = x$infos, type = x$type, SIMPLIFY = FALSE)

  # 3. Affichage de l'en-tête (Noms des variables)
  # format() garantit que chaque nom occupe exactement col_width espaces
  cat(paste(format(x$variables, width = col_width), collapse = ""), "\n")

  # 4. Affichage des types
  cat(paste(format(paste0("(", x$type, ")"), width = col_width), collapse = ""), "\n\n")

  # 5. Affichage des 4 lignes de statistiques
  for (i in 1:4) {
    # On extrait la i-ème ligne de chaque variable
    ligne_stats <- sapply(info_grille, function(v) v[i])
    # On l'affiche avec le bon espacement
    cat(paste(format(ligne_stats, width = col_width), collapse = ""), "\n")
  }

  cat("======================================================================\n")

  cat("\n")
  cat("                        Résumé général\n")
  cat("======================================================================\n")
  cat("Données manquantes :", x$nb_na, "\n")
  cat("----------------------------------------------------------------------\n")

  if (!is.null(x$llm_interpretation)) {
    cat("Interprétation des variables faite par le LLM :\n\n")

    # strwrap découpe proprement le texte pour qu'il ne dépasse pas 70 caractères
    paragraphe_formate <- strwrap(x$llm_interpretation$paragraphe, width = 70)
    cat(paragraphe_formate, sep = "\n")
    cat("\n")

  } else {
    cat("Interprétation des variables faite par le LLM : (non disponible)\n")
  }

  cat("======================================================================\n")

  invisible(x)
}
