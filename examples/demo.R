# exemple d'utilisation du package
library(lamaplot.context)

# test BOXPLOT

res_iris <- boxplot_context(iris, x_col = "Species", y_col = "Sepal.Length")
print(res_iris)
plot(res_iris)

# test SCATTERPLOT

nuage_cars <- scatterplot_context(mtcars, x_col = "wt", y_col = "mpg")
print(nuage_cars)
plot(nuage_cars)

# test SUMMARY

df <- data.frame(
  age = c(20L, 30L, 40L, NA),
  taille = c(1.65, 1.80, 1.75, 1.70),
  sexe = c("F", "H", "F", "H"),
  fumeur = c(TRUE, FALSE, FALSE, TRUE)
)

res_summary <- summary_context(df)
print(res_summary)
