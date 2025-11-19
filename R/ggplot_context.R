ggplot_context <- function(data, mapping, context = "", model = "mistral") {
  stopifnot(is.data.frame(data))

  # 1. Création du graphique de base : boxplot
  plt <- ggplot2::ggplot(data, mapping) +
    ggplot2::geom_boxplot()

  # 2. Petit résumé statistique pour le LLM
  var_y <- rlang::as_name(rlang::get_expr(mapping$y))
  var_x <- if (!is.null(mapping$x)) rlang::as_name(rlang::get_expr(mapping$x)) else NULL

  summary_stats <- utils::capture.output(summary(data[[var_y]]))
  summary_stats <- paste(summary_stats, collapse = "\n")

  # 3. Construction du prompt
  prompt <- paste0(
    "Tu es un assistant qui analyse un boxplot.\n",
    "Contexte fourni par l'utilisateur :\n", context, "\n\n",
    "Variable représentée : ", var_y, "\n",
    if (!is.null(var_x)) paste0("Groupes : ", var_x, "\n") else "",
    "Résumé statistique :\n", summary_stats, "\n\n",
    "Donne un titre court, un sous-titre descriptif, et une interprétation du boxplot.\n",
    "Répond strictement dans ce format :\n",
    "TITRE: ...\n",
    "SOUS_TITRE: ...\n",
    "COMMENTAIRE: ..."
  )

  # 4. Appel à Ollama local
  body = toJSON(list(
    model = model,
    prompt = prompt,
    stream = FALSE   # important
  ), auto_unbox = TRUE)


  res <- httr::POST(
    url = "http://localhost:11434/api/generate",
    body = body,
    encode = "json"
  )

  out <- jsonlite::fromJSON(httr::content(res, "text"))$response

  # 5. Extraction du texte structuré
  titre        <- sub(".*TITRE: (.*)\nSOUS_TITRE:.*", "\\1", out)
  sous_titre   <- sub(".*SOUS_TITRE: (.*)\nCOMMENTAIRE:.*", "\\1", out)
  commentaire  <- sub(".*COMMENTAIRE: (.*)", "\\1", out)

  # 6. Ajout titre / sous-titre au graphique
  plt <- plt +
    ggplot2::labs(
      title = titre,
      subtitle = sous_titre
    )

  # 7. Construction de l’objet S3
  structure(
    list(
      plot = plt,
      interpretation = commentaire,
      context = context,
      variable = var_y,
      groups = var_x
    ),
    class = "ggplot_context"
  )
}
