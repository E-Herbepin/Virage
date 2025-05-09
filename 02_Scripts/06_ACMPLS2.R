#Ce script a pour objectif de produire une ACM pls2 sur les variables sociodémo controlées par les variables de victimation

# Liste des variables socio démo :
#  c(
#   "sexe", "age", "situa", "TailleAglo", "Statumigration", "Diplome",  "CSP_3", "Couple", "dureecouple", "revenus", "religion", "importance_religion"
#  )
# Liste des variables de victimation :
#   c(
# paste0("TE", c(1:4, 7:8, 10:11, 13, 15, 17:18)),
# paste0("T", c(1:5, 7:9, 11:12, 14:17)),
# paste0("P", c(1:11)),
# paste0("C", c(2:12, 15:16, 20:24, 26:28, 30:31)),
# paste0("C18", c("a", "b", "c", "d", "e", "f", "g")),
# paste0("E3", c("a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n")),
# paste0("F", c(1:3, 5:9)),
# paste0("V", c(1:9))
# )
# FOnciton : pls2_nipals {chemometrics}
install.packages(chemometrics)
library(chemometrics)
#TODO ajouter les variables de contexte ?
pls2_nipals(
  X = c(
    paste0("TE", c(1:4, 7:8, 10:11, 13, 15, 17:18)),
    paste0("T", c(1:5, 7:9, 11:12, 14:17)),
    paste0("P", c(1:11)),
    paste0("C", c(2:12, 15:16, 20:24, 26:28, 30:31)),
    paste0("C18", c("a", "b", "c", "d", "e", "f", "g")),
    paste0("E3", c("a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n")),
    paste0("F", c(1:3, 5:9)),
    paste0("V", c(1:9))
  ),
  Y = c(
      "sexe", "age", "situa", "TailleAglo", "Statumigration", "Diplome",  "CSP_3", "Couple", "dureecouple", "revenus", "religion", "importance_religion"
     ),
  a = 8, #Nombre d'axe à garder
  it = 150, #Nombre d'itératiops
  tol =  1e-08, #Tolérance
  scale = TRUE #Normalisation complete (sinon justye centralisation)
)
