Var2 <- Var[-c(31:33)]
base <- PC18s[, Var2]
library(GDAtools)
library(ggrepel)
exclu <- c(paste0(Var2[c(1, 4:30)], ".Non"), paste0(Var2[c(2:3)], ".0"))
Resultat <-
  csMCA(data = base,
        subcloud = PC18s$age_classe == "18-34",
        excl = exclu)


ggplot(res_ACM$var$coord, aes(x = res_ACM$var$coord[,1], y = res_ACM$var$coord[,2], color = ExtInt[-c((length(ExtInt)-23):length(ExtInt))])) +
  geom_point() +
  theme_minimal()


vcloud <-
  ggcloud_variables(Resultat,
                    shapes = FALSE,
                    legend = "none",
                    textsize = 1)
ggadd_supvars(vcloud, Resultat, PC18s[, c("sexe_r", "age_classe", "situa_rec")])
tabcontrib(Resultat, dim = 1)


library(FactoInvestigate)
Investigate(res_ACM)








Sans tunning et peche/chasse
Deux axes intéréssant (on garde le second, le premier est clairement une distinction)
```{r}
Var <- names(PC18s %>%
               select(NbConcert_dic,nbvisites_cat,nbautresorties_cat,
                      jardin,potag,brideco,cuisin,
                      collec,journ,ecritRomPo,
                      Montage,Scien,JV,TV,EcouMus,Serie,Bibl,Cinema,
                      Bal,SpecSpor,BdN,SpecDan,SpecCirq,SpecRue,SpecThea, sexe_r, 
                      age_classe, situa_rec,PCS_menage,UU_cat,diplome))
labels <- c("Concert dans l'année", "Visites de lieux culturels",
            "Nombre d'autres sorties", "Jardinage", "Potager",
            "Bricolage/décoration", "Cuisine", "Collection",
            "Journal intime", "Ecriture de romans/poèmes", "Montage audiovisuel",
            "Activité scientifique", "Jeux vidéo", "Télévision", "Ecoute de musique",
            "Séries", "Bibliothèque", "Cinéma", "Bal", "Spectacle sportif",
            "Boite de Nuit", "Spectacle de danse", "Spectacle de cirque", "Spectacle de rue", "Spectacle théâtral")

Var_sup <- c("sexe_r", "age_classe", "situa_rec", "PCS_menage", "UU_cat", "diplome")
excl <- c(paste0(Var[c(1,3:(length(Var)-length(Var_sup)))],
                 "_Non"),
          paste0(Var[c(2)],"_0")) # On définit les modalités à enlever (toutes les modalités de non pratique)

res_ACM <- MCA(PC18s[,Var],quali.sup=Var_sup, ncp=5, graph=F, excl=excl) # On réalise l'ACM
explor(res_ACM)

ExtInt<-c("Hors Domicile", "Hors Domicile", "Hors Domicile", "Hors Domicile", "Domestique", "Domestique", "Domestique", "Hors Domicile", "Domestique", "Domestique","Hors Domicile", "Hors Domicile", "Hors Domicile", "Hors Domicile", "Hors Domicile", "Domestique", "Domestique", "Domestique", "Domestique", "Domestique","Domestique", "Domestique", "Hors Domicile", "Hors Domicile", "Hors Domicile", "Hors Domicile", "Hors Domicile", "Domestique")


symbol_vars <- c(rep("Active",length(VarNoSup)+3))

labelsmodal<- c("Bal", "Boite de Nuit", "Bibliothèque", "Cinéma", "Ecoute de musique", "Jeux vidéo", "Montage audiovisuel", "Concert dans l'année", "Activité scientifique", "Séries", "Spectacle de cirque", "Spectacle de danse", "Spectacle de rue", "Spectacle sportif", "Spectacle théâtral", "Télévision", "Bricolage/décoration", "Collection", "Cuisine", "Ecriture de romans/poèmes", "Jardinage", "Journal intime", "Nombre d'autres sorties 0", "Nombre d'autres sorties 1 ou 2", "Nombre d'autres sorties 3 et +", "Visites de lieux culturels 1 à 2", "Visites de lieux culturels 3 et +", "Potager")
GraphExtInt(res = res_ACM,  ExtInt = ExtInt,  xax = 2,  yax = 1,  var_sup = FALSE,
            var_sup_choice = Var_sup,  var_lab_min_contrib = 0,  col_var = "Colors",  labels = labelsmodal,  symbol_variables = NULL,  size_var = NULL,  size_range = c(10, 3000), labels_size = 7,  point_size = 56,  transitions = TRUE,  labels_positions = "auto",  labels_prepend_var = FALSE,  xlim = c(-1.39, 2.23),  ylim = c(-1.39, 2.22))
```
sans journal et ecriture de roman/poemes
```{r}
Var <- names(PC18s %>%
               select(NbConcert_dic,nbvisites_cat,nbautresorties_cat,jardin,potag,brideco,cuisin,collec,Montage,Scien,JV,TV,EcouMus,Serie,Bibl,Cinema,Bal,SpecSpor,BdN,SpecDan,SpecCirq,SpecRue,SpecThea,sexe_r,age_classe, situa_rec,PCS_menage,UU_cat,diplome))
labels <- c("Concert dans l'année", "Visites de lieux culturels", "Nombre d'autres sorties", "Jardinage", "Potager", "Bricolage/décoration", "Cuisine", "Collection", "Montage audiovisuel", "Activité scientifique", "Jeux vidéo", "Télévision", "Ecoute de musique", "Séries", "Bibliothèque", "Cinéma", "Bal", "Spectacle sportif", "Boite de Nuit", "Spectacle de danse", "Spectacle de cirque", "Spectacle de rue", "Spectacle théâtral")

Var_sup <- c("sexe_r", "age_classe", "situa_rec", "PCS_menage", "UU_cat", "diplome")
excl <- c(paste0(Var[c(1,3:(length(Var)-length(Var_sup)))],
                 "_Non"),
          paste0(Var[c(2)],"_0")) # On définit les modalités à enlever (toutes les modalités de non pratique)

res_ACM <- MCA(PC18s[,Var],quali.sup=Var_sup, ncp=3, graph=F, excl=excl) # On réalise l'ACM
explor(res_ACM)

ExtInt<-c("Hors Domicile", "Hors Domicile", "Hors Domicile", "Hors Domicile", "Domestique", "Domestique", "Domestique", "Hors Domicile", "Domestique", "Domestique", "Hors Domicile", "Hors Domicile", "Hors Domicile", "Hors Domicile", "Hors Domicile", "Domestique", "Domestique", "Domestique", "Domestique", "Domestique", "Hors Domicile", "Hors Domicile", "Hors Domicile", "Hors Domicile", "Hors Domicile", "Domestique")


symbol_vars <- c(rep("Active",length(Var)-length(Var_sup)+3)) #Le +3 correspond aux variable NBSorties et NBlieux culturels qui ont plusieurs modalités

labelsmodal<- c("Bal", "Boite de Nuit", "Bibliothèque", "Cinéma", "Ecoute de musique", "Jeux vidéo", "Montage audiovisuel", "Concert dans l'année", "Activité scientifique", "Séries", "Spectacle de cirque", "Spectacle de danse", "Spectacle de rue", "Spectacle sportif", "Spectacle théâtral", "Télévision", "Bricolage/décoration", "Collection", "Cuisine", "Jardinage", "Nombre d'autres sorties 0", "Nombre d'autres sorties 1 ou 2", "Nombre d'autres sorties 3 et +", "Visites de lieux culturels 1 à 2", "Visites de lieux culturels 3 et +", "Potager")
GraphExtInt(res = res_ACM,  ExtInt = ExtInt,  xax = 2,  yax = 1,  var_sup = FALSE,
            var_sup_choice = Var_sup,  var_lab_min_contrib = 0,  col_var = "Colors",  labels = labelsmodal,  symbol_variables = NULL,  size_var = NULL,  size_range = c(10, 3000), labels_size = 7,  point_size = 56,  transitions = TRUE,  labels_positions = "auto",  labels_prepend_var = FALSE,  xlim = c(-1.39, 2.23),  ylim = c(-1.39, 2.22))
```
sans montage, recherche et science : on perd le coté distinctif de l'axe 2
```{r}
Var <- names(PC18s %>%
               select(NbConcert_dic,nbvisites_cat,nbautresorties_cat,jardin,potag,brideco,cuisin,collec,JV,TV,EcouMus,Serie,Bibl,Cinema,Bal,SpecSpor,BdN,SpecDan,SpecCirq,SpecRue,SpecThea,sexe_r,age_classe, situa_rec,PCS_menage,UU_cat,diplome))
labels <- c("Concert dans l'année", "Visites de lieux culturels", "Nombre d'autres sorties", "Jardinage", "Potager", "Bricolage/décoration", "Cuisine", "Collection", "Jeux vidéo", "Télévision", "Ecoute de musique", "Séries", "Bibliothèque", "Cinéma", "Bal", "Spectacle sportif", "Boite de Nuit", "Spectacle de danse", "Spectacle de cirque", "Spectacle de rue", "Spectacle théâtral")

Var_sup <- c("sexe_r", "age_classe", "situa_rec", "PCS_menage", "UU_cat", "diplome")
excl <- c(paste0(Var[c(1,3:(length(Var)-length(Var_sup)))],
                 "_Non"),
          paste0(Var[c(2,3)],"_0")) # On définit les modalités à enlever (toutes les modalités de non pratique)

res_ACM <- MCA(PC18s[,Var],quali.sup=Var_sup, ncp=3, graph=F, excl=excl) # On réalise l'ACM
explor(res_ACM)

ExtInt<-c("Hors Domicile", "Hors Domicile", "Hors Domicile", "Hors Domicile", "Domestique", "Domestique", "Hors Domicile", "Domestique", "Hors Domicile", "Hors Domicile", "Hors Domicile", "Hors Domicile", "Hors Domicile", "Domestique", "Domestique", "Domestique", "Domestique", "Domestique
          ","Hors Domicile", "Hors Domicile", "Hors Domicile", "Hors Domicile", "Hors Domicile", "Domestique")


symbol_vars <- c(rep("Active",length(Var)-length(Var_sup)+3)) #Le +3 correspond aux variable NBSorties et NBlieux culturels qui ont plusieurs modalités

labelsmodal<- c("Bal", "Boite de Nuit", "Bibliothèque", "Cinéma", "Ecoute de musique", "Jeux vidéo", "Concert dans l'année", "Activité scientifique", "Séries", "Spectacle de cirque", "Spectacle de danse", "Spectacle de rue", "Spectacle sportif", "Spectacle théâtral", "Télévision", "Bricolage/décoration", "Collection", "Cuisine", "Jardinage", "Nombre d'autres sorties 0", "Nombre d'autres sorties 1 ou 2", "Nombre d'autres sorties 3 et +", "Visites de lieux culturels 1 à 2", "Visites de lieux culturels 3 et +", "Potager")
GraphExtInt(res = res_ACM,  ExtInt = ExtInt,  xax = 2,  yax = 1,  var_sup = FALSE,
            var_sup_choice = Var_sup,  var_lab_min_contrib = 0,  col_var = "Colors",  labels = labelsmodal,  symbol_variables = NULL,  size_var = NULL,  size_range = c(10, 3000), labels_size = 7,  point_size = 56,  transitions = TRUE,  labels_positions = "auto",  labels_prepend_var = FALSE,  xlim = c(-1.39, 2.23),  ylim = c(-1.39, 2.22))
```
