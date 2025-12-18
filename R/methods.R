# ========== METHODES POUR BOXPLOT CONTEXT ==============

#' Afficher l'interprétation d'un objet boxplot_context
#'
#' @param x Un objet de classe boxplot_context
#' @param ... Arguments supplémentaires (non utilisés)
#'
#' @export
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

#' Tracer le boxplot d'un objet boxplot_context
#'
#' @param x Un objet de classe boxplot_context
#' @param ... Arguments supplémentaires passés à ggplot2
#'
#' @return Un objet ggplot
#' @export
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


#' Méthode print pour scatterplot_context
#'
#' @param x Un objet de classe scatterplot_context
#' @param ... Arguments supplémentaires (non utilisés)
#' @export
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

#' Méthode plot pour scatterplot_context
#'
#' @param x Un objet de classe scatterplot_context
#' @param ... Arguments supplémentaires passés à ggplot2
#' @return Un objet ggplot
#' @export
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


# ======== METHODES POUR SUMMARY CONTEXT ===================

print.summary_context <- function(x, ...) {

  cat("\n")
  cat("                Résumé des variables\n")
  cat("=====================================================\n")

  # En-tête des variables
  cat(paste0("\t", paste(x$variables, collapse = "\t\t")), "\n")

  # Types
  cat(paste0("\t", paste(x$type, collapse = "\t\t")), "\n")

  info_line <- mapply(
    FUN = function(info, type) {

      if (type %in% c("integer", "numeric")) {

        paste0(
          "min=", round(info$min, 2), ", ",
          "max=", round(info$max, 2), ", ",
          "moy=", round(info$mean, 2), ", ",
          "med=", round(info$median, 2)
        )

      } else if (type == "character") {

        paste(info$valeurs, collapse = ", ")

      } else if (type == "logical") {

        paste0(
          "TRUE=", info$nb_TRUE, ", ",
          "FALSE=", info$nb_FALSE
        )

      } else {

        "Type non pris en charge"
      }

    },
    info = x$infos,
    type = x$type,
    SIMPLIFY = TRUE
  )

  cat(paste0("\t", paste(info_line, collapse = "\t")), "\n")

  cat("=====================================================\n")

  cat("\n")
  cat("                   Résumé général\n")
  cat("=====================================================\n")
  cat("Données manquantes :", x$nb_na, "\n")

  if (!is.null(x$llm_interpretation)) {
    cat("Interprétation des variables faite par le LLM :\n\n")

    paragraphe_formate <- strwrap(x$llm_interpretation$paragraphe, width = 70, prefix = "")
    cat(paragraphe_formate, sep = "\n")
    cat("\n\n")

  } else {
    cat("Interprétation des variables faite par le LLM : (non disponible)\n")
  }

  invisible(x)
}

