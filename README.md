# LamaPlotContext

## Présentation

LamaPlotContext est un package R permettant de générer des visualisations et analyses enrichies (boxplot, scatterplot, summary) avec 
interprétation automatique par un LLM local (Ollama). Il facilite l'analyse statistique et la compréhension des données grâce
à des résumés et commentaires générés automatiquement.

Cette version du package est fonctionnelle mais en cours de développement. De nouvelles fonctionnalités et types de graphiques sont
en cours de développement, restez branchés :D

## Comment ça fonctionne ?

Notre LamaPlotContext vous permet de faire appel à vos fonctions graphiques favories (pour l'instant la boîte à moustache et 
le nuage de point), en y ajoutant un titre et une interprétation générée par le modèle LLM Mistral (avec un Ollama local). Il permet
également de générer un `summary` amélioré de la même façon.

A chaque appel d'une des fonctions ou méthodes de notre package, vous faites une requête à votre Ollama local, qui renvoie une liste
de tous les éléments que vous auriez du gérer vous même autrement : titre, interprétation, légende ... Alors assurez vous de bien avoir
téléchargé Ollama sur votre machine et d'utiliser le modèle compatible (mistral). Ci-dessous vous trouverez les instructions nécessaires
au bon fonctionnement du package.

---

## Installation du package

### Depuis une archive ZIP (lamaplotcontext.zip)

1. Téléchargez le fichier `lamaplotcontext_0.1.0.tar.gz` et placez-le sur votre ordinateur (par exemple sur le Bureau).
2. Dans R ou RStudio, installez le package avec la commande suivante (en adaptant le chemin) :

```r
install.packages("C:/Users/votre_nom/Bureau/lamaplotcontext_0.1.0.tar.gz", repos = NULL, type = "source")
```

Le package sera alors disponible comme n'importe quel package R, vous pourrez l'utiliser dans votre environnement à
l'aide de la commande `library(lamaplot.context)`.

Un ficher d'exemples `demo.R` est disponible dans le dossier `examples` du zip. Vous y trouverez une petite fonction pour tester votre
connexion à Ollama (n'hésitez pas à retourner sur le site de Ollama : https://docs.ollama.com/)

---

## Prérequis LLM (Ollama)

L'interprétation automatique nécessite l'installation d'Ollama (LLM local) :

1. Télécharger et installer Ollama : https://ollama.com/download
2. Démarrer Ollama (le service doit tourner sur http://localhost:11434)
3. Télécharger un modèle compatible (ex : mistral), la commande est à entrer dans le terminal de votre ordinateur, pas la console R :
	```sh
	ollama pull mistral
	```
4. Vérifiez que le port 11434 est accessible.

---

## Auteurs
- Clara Noël : résumé contextuel et summary
- Mathilde Lafay : boxplot et scatterplot
