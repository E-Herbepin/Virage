#### PACKAGES ET OPTIONS #####

# PACKAGES ----

c("bookdown", "broom", "carData", "ClustOfVar", "cluster", "effects", "esquisse", "extrafont", "explor", "FactoMineR", "factoextra", "FactoInvestigate", "Factoshiny", "flextable", "gtools", "gmodels", "ggrepel", "ggtrace", "GDAtools", "grid", "gt", "gtsummary", "here", "janitor", "kableExtra", "knitr", "labelled", "magrittr", "mapsf", "Matrix", "missMDA", "nnet", "officer", "openxlsx", "printr", "questionr", "RColorBrewer", "rmarkdown", "rmdformats", "rticles", "scales", "sf", "shiny", "survey", "survival", "tidyverse") |>
  lapply(library, character.only = TRUE) |>
  invisible()

# OPTIONS ----

set_flextable_defaults(decimal.mark = ",", big.mark = " ", na_st = "-")

options(OutDec= ",",
        scipen=999) # pour désactiver l'écriture scientifique des nombres

# on indique à gtsummary un affichage en français
theme_gtsummary_language(
  "fr",
  decimal.mark = ",", # séparateur de décimales
  big.mark = " " # séparateur de milliers
) %>%
  invisible()
theme_gtsummary_compact() %>%
  invisible()

# pour empêcher knitr d'afficher les messages et les warnings dans les .Rmd et .qmd

knitr::opts_chunk$set(
  warning = FALSE, 
  message = FALSE) 

loadfonts(device = "win") %>%
  invisible()