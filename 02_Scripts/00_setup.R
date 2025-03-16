#### PACKAGES ET OPTIONS #####

# PACKAGES ----

library(bookdown)
library(broom)
library(carData)
library(ClustOfVar)
library(cluster)
library(effects)
library(esquisse)
library(extrafont)
library(explor)
library(FactoMineR)
library(factoextra)
library(FactoInvestigate)
library(Factoshiny)
library(flextable)
library(gtools)
library(gmodels)
library(ggrepel)
library(ggtrace)
library(GDAtools)
library(grid)
library(gt)
library(gtsummary)
library(here)
library(janitor)
library(kableExtra)
library(knitr)
library(labelled)
library(magrittr)
library(mapsf)
library(Matrix)
library(missMDA)
library(nnet)
library(officer)
library(openxlsx)
library(printr)
library(questionr)
library(RColorBrewer)
library(rmarkdown)
library(rmdformats)
library(rticles)
library(scales)
library(sf)
library(shiny)
library(survey)
library(survival)
library(tidyverse)

library(furrr)


# OPTIONS ----

set_flextable_defaults(decimal.mark = ",", big.mark = " ", na_st = "-")

options(OutDec= ",")

options(scipen=999) # pour désactiver l'écriture scientifique des nombres

# on indique à gtsummary un affichage en français
theme_gtsummary_language(
  "fr",
  decimal.mark = ",", # séparateur de décimales
  big.mark = " " # séparateur de milliers
)
theme_gtsummary_compact()

# pour empêcher knitr d'afficher les messages et les warnings dans les .Rmd et .qmd

knitr::opts_chunk$set(
  warning = FALSE, 
  message = FALSE
) 

loadfonts(device = "win", quiet = TRUE)
