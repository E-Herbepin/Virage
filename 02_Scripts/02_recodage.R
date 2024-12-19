#### Socio démo ####

#Pondération
PC18s <- PC18s %>% 
  filter(!is.na(POND)) %>% 
  mutate(POND_r = POND/(sum(POND)/nrow(.))
  ) #Pour l'ACM et les tableau : le poids somme à la taille de l'échantillon

PC18s <- PC18s %>% 
  filter(!is.na(POND)) %>% 
  mutate(Poids = POND/(sum(PC18s$POND)/length(PC18s))
) #Pour la régression, le poids somme à 1

sum(PC18s$Poids) #Vérification
#Sexe
PC18s <- mutate(PC18s,
               sexe_r = factor(SEXE, c(1, 2),
                               labels = c("Homme", "Femme")))

#Age
PC18s <- mutate(PC18s,
               age_classe = cut(
                 AGE,
                 breaks = c(15, 35,55,75,120),
                 labels = c("15 à 35 ans", "36 à 55 ans", "56 à 75 ans", "76 ans et +"),
                 include.lowest = TRUE
               ))

#Situation
PC18s <- mutate(PC18s,
               situa_rec = factor(
                 SITUA,
                 c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
                 labels = c(
                   "Occupe un emploi",
                   "Etudiant,Apprenti,Eleve",
                   "Etudiant,Apprenti,Eleve",
                   "Inactif (chomage, invalidité...)",
                   "Retraité ou retiré des affaires ou en préretraite",
                   "Inactif (chomage, invalidité...)",# Dans notre cadre, les personnes aux foyers, par la plus grande gestion possible de leur temps concernant les pratiques culturelles, seront associées aux inactifs
                   "Inactif (chomage, invalidité...)",
                   "Inactif (chomage, invalidité...)",
                   "Occupe un emploi", # 3  pers, on suppose qu'elle ne savait pas si leur activité était considéré comme un emploi
                   "Inactif (chomage, invalidité...)" # REF: 1 pers, on suppose qu'elle ne voulait pas dire qu'elle était inactive
                 )
               ))

PC18s$SITUAr <- PC18s$SITUA
PC18s$SITUAr[PC18s$SITUA %in% c(7)] <- ifelse(PC18s$AGE[PC18s$SITUA %in% c(7)] > 62, 5, 1)
PC18s$SITUAr[PC18s$SITUA %in% c(8)] <- ifelse(PC18s$AGE[PC18s$SITUA %in% c(8)] > 62, 5, ifelse(PC18s$AGE[PC18s$SITUA %in% c(8)] < 19, 2, 1))
PC18s$SITUAr[PC18s$SITUA %in% c(9)] <- 5 #au vu de leurs ages, nous les considérons comme retraités
PC18s$SITUAr[PC18s$SITUA %in% c(10)] <- 1

PC18s <- mutate(PC18s,
               situa = factor(
                 SITUAr,
                 c(1, 2, 3, 4, 5, 6),
                 labels = c(
                   "Actif",
                   "Etudiant",
                   "Etudiant",
                   "Actif",
                   "Retraité",
                   "Actif"
                 )
                 )
               )

PC18s <- mutate(PC18s,
               revenu = factor(
                 CRITREVENU,
                 c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12),
                 labels = c(
                   "Moins de 800 euros",
                   "De 800 à 999 euros",
                   "De 1000 à 1199 euros",
                   "De 1200 à 1499 euros",
                   "De 1500 à 1999 euros",
                   "De 2000 à 2499 euros",
                   "De 2500 à 2999 euros",
                   "De 3000 à 3999 euros",
                   "De 4000 à 5999 euros",
                   "6000 euros ou plus",
                   "Ne sait pas",
                   "Refus"
                 )
               ))

PC18s <- mutate(PC18s,
                revenu_cat = factor(
                  CRITREVENU,
                  c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12),
                  labels = c(
                    "Moins de 2000 euros",
                    "Moins de 2000 euros",
                    "Moins de 2000 euros",
                    "Moins de 2000 euros",
                    "Moins de 2000 euros",
                    "De 2000 à 4000 euros",
                    "De 2000 à 4000 euros",
                    "De 2000 à 4000 euros",
                    "4000 euros ou plus",
                    "4000 euros ou plus",
                    "Ne sait pas",
                    "Refus"
                  )
                ))

PC18s <- mutate(PC18s,
               UU = factor(
                 TUU2016,
                 c(0, 1, 2, 3, 4, 5, 6, 7, 8),
                 labels = c(
                   "Commune rurale",
                   "moins de 5 000 habitants",
                   "5 000 à 9 999 habitants",
                   "10 000 à 19 999 habitants",
                   "20 000 à 49 999 habitants",
                   "50 000 à 99 999 habitants",
                   "100 000 à 199 999 habitants",
                   "200 000 à 1 999 999 habitants",
                   "Paris"
                 )
               ))

#Recodage du type de commune
PC18s <- 
  PC18s %>% 
  mutate(UU_cat = 
           case_when(TUU2016 %in% 0:2 ~ "Com. rurale et petite unité urbaine",
                     TUU2016 %in% 3:6 ~ "Unité urbaine moyenne",
                     TUU2016 %in% 7:8 ~ "Grande unité urbaine",
                     .default = NA),
         UU_cat = fct_relevel(UU_cat, 
                                    "Com. rurale et petite unité urbaine",
                                    "Unité urbaine moyenne",
                                    "Grande unité urbaine"))

PC18s <- mutate(PC18s,
               diplome = factor(PC18s$DIPLOM,
                 c(1:15),
                 labels = c(
                   "Inf.Brevet", # Les précisions ne sont pas nécessaires
                   "Inf.Brevet",
                   "Inf.Brevet",
                   "Inf.Brevet",
                   "Brevet/CAP/Bac",
                   "Brevet/CAP/Bac",
                   "Brevet/CAP/Bac",
                   "Brevet/CAP/Bac",
                   "Brevet/CAP/Bac",
                   "Sup.Bac",
                   "Sup.Bac",
                   "Sup.Bac",
                   "Sup.Bac",
                   "Brevet/CAP/Bac", # On intègre les NSP au niveau de diplome < au bac
                   "Brevet/CAP/Bac" # On intègre les REF au niveau de diplome < au bac
                 )
               ))

PC18s$diplome[PC18s$DIPLOMACT %in% c(3:8)] <- "Sup.Bac"

PC18s <- mutate(PC18s,
                DIPLOM_r = factor(
                  DIPLOM,
                  c(1:15),
                  labels = c(
                    "Vous n avez jamais été à l'école ou vous l'avez quittée avant la fin du primaire",
                    "Aucun diplôme et scolarité interrompue à la fin du primaire ou avant la fin du collège",
                    "Aucun diplôme et scolarité jusqu à la fin du collège et au-delà",
                    "CEP",
                    "BEPC, brevet élémentaire, brevet des collèges, DNB",
                    "CAP, BEP ou diplôme équivalent",
                    "Baccalauréat général ou technologique, brevet supérieur",
                    "Capacité en droit, DAEU, ESEU",
                    "Baccalauréat professionnel, brevet professionnel, de technicien ou d enseignement, diplôme équivalent",
                    "BTS, DUT, DEUST, diplôme de la santé ou social de niveau Bac+2 ou diplôme équivalent",
                    "Licence, licence pro, maîtrise ou autre diplôme de niveau Bac+3 ou 4 ou diplôme équivalent",
                    "Master, DEA, DESS, diplôme grande école de niveau Bac+5, doctorat de santé",
                    "Doctorat de recherche (hors santé)",
                    "NSP",
                    "REF"
                  )
                ))

PC18s <- mutate(PC18s,
                TpsPro = S11_C_1 + S12_C_1/60,
                TpsPro_cat = case_when(
                  TpsPro < 35 ~ "<35",
                  TpsPro <40 ~ "35-39",
                  TpsPro >=40 ~ ">=40",
                  is.na(TpsPro) ~ "Sans emploi"),
                TpsPro_cat = factor(TpsPro_cat, levels = c("Sans emploi","<35", "35-39", ">=40")))


# PCS

PC18s <-
  PC18s %>% 
  mutate(
    PCS =
      case_when(
        situa=="Etudiant" ~ "Etudiants",
        CSTOT %/% 10 == 1 ~ "Agriculteurs exploitants",
        CSTOT %/% 10 == 2 ~ "Artisans, commerçants, chefs d’entreprise",
        CSTOT %/% 10 == 3 ~ "Cadres et professions intellectuelles supérieures",
        CSTOT %/% 10 == 4 ~ "Professions intermédiaires",
        CSTOT %/% 10 == 5 ~ "Employés",
        CSTOT %/% 10 == 6 ~ "Ouvriers"),
    PCS = fct_relevel(PCS, "Agriculteurs exploitants",
                          "Artisans, commerçants, chefs d’entreprise",
                          "Cadres et professions intellectuelles supérieures",
                          "Professions intermédiaires",
                          "Employés",
                          "Ouvriers",
                          "Etudiants"))




#### Recodage nb concert ####

for (x in names(select(PC18s, contains("val")))) {
  PC18s[x][is.na(PC18s[x])] <- 0}

for (x in names(select(PC18s, contains("unit")))) {
  PC18s[x] <- case_when(
    PC18s[x] == 1 ~ 52,
    PC18s[x] == 2 ~ 12,
    is.na(PC18s[x]) ~ 1,
    TRUE ~ 1)}

for (x in names(select(PC18s, contains("unit")))) {
  y<-paste0(substring(x,0,3),substring(x,8))
  PC18s[y]<-PC18s[x]*PC18s[paste0(substring(x,0,3),"val",substring(x,8))]}


PC18s <- mutate(
  PC18s,
  NbConcert =
  G26unit_variet_francaise * G26val_variet_francaise +
    G26unit_musiques_monde * G26val_musiques_monde +
    G26unit_musiques_tradi * G26val_musiques_tradi +
    G26unit_variet_internationale * G26val_variet_internationale +
    G26unit_rnb * G26val_rnb + 
    G26unit_electro_techno * G26val_electro_techno +
    G26unit_hip_hop_rap * G26val_hip_hop_rap +
    G26unit_metal_hard * G26val_metal_hard +
    G26unit_pop_rock * G26val_pop_rock +
    G26unit_jazz * G26val_jazz +
    G26unit_opera * G26val_opera +
    G26unit_musique_classique * G26val_musique_classique
)

PC18s$NbConcert_dic <- case_when(
  PC18s$NbConcert > 0 ~ "Oui",
  TRUE ~ "Non"
)

## Recodage nb spectacle


PC18s <- mutate(
  PC18s,
  NbSpe =
    G14unit_theatre * G14val_theatre +
    G14unit_spectacle_rue * G14val_spectacle_rue +
    G14unit_cirque * G14val_cirque +
    G14unit_danse * G14val_danse
)

PC18s$NbSpe_cat <- case_when(PC18s$NbSpe == 0 ~ "Jamais",
                             PC18s$NbSpe %in% c(1, 2) ~ "1 ou 2 par an",
                             TRUE ~ "3 ou plus par an")


## Recodage autre sorties


PC18s <- mutate(
  PC18s,
  nbautresorties =
    ifelse(is.na(H204), 0, H204) +
    ifelse(is.na(H205), 0, H205) +
    ifelse(is.na(H206), 0, H206) +
    ifelse(is.na(H206), 0, H207) +
    ifelse(is.na(H208), 0, H208) +
    ifelse(is.na(H206), 0, H209) +
    ifelse(is.na(H210), 0, H210) +
    ifelse(is.na(H211), 0, H211))


PC18s$nbautresorties_cat <-
  case_when(PC18s$nbautresorties >= 3 ~ "3 et +",
            PC18s$nbautresorties >= 1 ~ "1 ou 2",
            TRUE ~ "0")


## Recodage visites


PC18s <- mutate(
  PC18s,
  nbvisites =
    ifelse(is.na(H1001), 0, H1001) +
    ifelse(is.na(H1002), 0, H1002) +
    ifelse(is.na(H1003), 0, H1003) +
    ifelse(is.na(H1004), 0, H1004) +
    ifelse(is.na(H1005), 0, H1005) +
    ifelse(is.na(H1006), 0, H1006))

PC18s$nbvisites_cat <- case_when(PC18s$nbvisites == 0 ~ "0",
                                 PC18s$nbvisites %in% 1:2 ~ "1 ou 2",
                                 TRUE ~ "3 et +")


# Recodages supplémentaires
PC18s <- mutate(
  PC18s,
  pechasse = ifelse(A1008==1,'Oui','Non'),
  jardin = ifelse(A1007==1,'Oui','Non'),
  potag = ifelse(A1006==1,'Oui','Non'),
  brideco = ifelse(A1005==1,'Oui','Non'),
  cuisin = ifelse(A1004==1,'Oui','Non'),
  jxsoc = ifelse(A1002==1,'Oui','Non'),
  tunn = ifelse(A1010==1,'Oui','Non'),
  collec = ifelse(A1009==1,'Oui','Non')
)

PC18s <- mutate(
  PC18s,
  journ = ifelse(is.na(A21_journal),'Non',ifelse(A21_journal==1,'Oui','Non')),
  ecritRomPo = ifelse(is.na(A21_romans),'Non',ifelse(A21_romans==1,'Oui','Non')),
  Montage = ifelse(is.na(A21_montages),'Non',ifelse(A21_montages==1,'Oui','Non')),
  Cirq = ifelse(is.na(A21_cirque),'Non',ifelse(A21_cirque==1,'Oui','Non')),
  Theat = ifelse(is.na(A21_theatre),'Non',ifelse(A21_theatre==1,'Oui','Non')),
  Recher = ifelse(is.na(A21_genealogie),'Non',ifelse(A21_genealogie==1,'Oui','Non')),
  Scien = ifelse(is.na(A21_activite_scientifique),'Non',ifelse(A21_activite_scientifique==1,'Oui','Non'))
)

PC18s <- mutate(
  PC18s,
  JV = ifelse(B1==1,'Oui','Non'),
  TV = ifelse(C1 %in% 1:2,'Oui','Non'), # On considère que regarder la TV est une activité d'intérieur lorsqu'elle prend une certaine ampleur (+ de 3 fois par semaine) temporelle pour différencier notre population
  EcouMus = ifelse(E17 %in% 1:2,'Oui','Non'), # De même pour l'écoute à la maison de musique avec un seuil de 'Oui, de temps en temps'
  Serie = ifelse(C31 %in% 1:3,'Oui','Non'), # De même pour les séries, avec un seuil de 'Au moins une fois par mois'
  Bibl = ifelse(F3 %in% 1:4,'Oui','Non'),
  Cinema = ifelse(G3A ==1 ,'Oui','Non'), # NSP = doute, si doute alors ce n'est pas une activité culturelle marquante
  )

PC18s <- mutate(
  PC18s,
  Bal = ifelse(G213==1|is.na(G213),'Oui','Non'),
  SpecSpor = ifelse(G214==1|is.na(G214),'Oui','Non'),
  BdN = ifelse(G215==1|is.na(G215),'Oui','Non'),
  SpecDan = ifelse(G131==1|is.na(G131),'Oui','Non'),
  SpecCirq = ifelse(G132==1|is.na(G132),'Oui','Non'),
  SpecRue = ifelse(G133==1|is.na(G133),'Oui','Non'),
  SpecThea = ifelse(G134==1|is.na(G134),'Oui','Non')
)

PC18s <- mutate(
  PC18s,
  Radio_15_35 = ifelse(PC18s$age_classe != "15 à 35 ans" | PC18s$E1 %in% c(4:7), "0", "1"),
  Radio_36_55 = ifelse(age_classe != "36 à 55 ans" | E1 %in% c(4:7), "0", "1"),
  Radio_56_75 = ifelse(age_classe != "56 à 75 ans" | E1 %in% c(4:7), "0", "1"),
  Radio_75_plus = ifelse(age_classe != "76 ans et plus" | E1 %in% c(4:7), "0", "1"))


## Recodage diversité activité d'intérieur

PC18s <- mutate(
  PC18s,
  nbactint =
    ifelse(jardin == "Oui", 1, 0) +
    ifelse(potag == "Oui", 1, 0) +
    ifelse(brideco == "Oui", 1, 0) +
    ifelse(cuisin == "Oui", 1, 0) +
    ifelse(tunn == "Oui", 1, 0) +
    ifelse(collec == "Oui", 1, 0) +
    ifelse(journ == "Oui", 1, 0) +
    ifelse(ecritRomPo == "Oui", 1, 0) +
    ifelse(Montage == "Oui", 1, 0) +
    ifelse(Scien == "Oui", 1, 0) +
    ifelse(Serie == "Oui", 1, 0) +
    ifelse(JV == "Oui", 1, 0) +
    ifelse(TV == "Oui", 1, 0) +
    ifelse(EcouMus == "Oui", 1, 0)
)


PC18s$nbactint_cat <-
  case_when(
    PC18s$nbactint %in% 0:3 ~ "<=3",
    PC18s$nbactint %in% 4:5 ~ "4 à 5",
    TRUE ~ ">=6"
  )
PC18s$nbactint_cat <-
  case_when(
    PC18s$nbactint %in% 0:2 ~ "<=2",
    PC18s$nbactint %in% 3:4 ~ "3 à 4",
    PC18s$nbactint %in% 5:6 ~ "5 à 6",
    TRUE ~ ">=7"
  )

PC18s$nbactint_cat <- factor(PC18s$nbactint_cat, levels = c("<=2", "3 à 4", "5 à 6", ">=7"))

## Recodage diversité activité d'extérieur

PC18s <- mutate(
  PC18s,
  nbactiviteexterieur =
    ifelse(NbConcert_dic == "Oui", 1, 0) +
    ifelse(nbvisites > 0, 1, 0) +
    ifelse(nbautresorties > 0, 1, 0) +
    ifelse(pechasse == "Oui", 1, 0) +
    ifelse(Bibl == "Oui", 1, 0) +
    ifelse(Cinema == "Oui", 1, 0) +
    ifelse(Bal == "Oui", 1, 0) +
    ifelse(SpecSpor == "Oui", 1, 0) +
    ifelse(BdN == "Oui", 1, 0) +
    ifelse(SpecDan == "Oui", 1, 0) +
    ifelse(SpecCirq == "Oui", 1, 0) +
    ifelse(SpecRue == "Oui", 1, 0) +
    ifelse(SpecThea == "Oui", 1, 0)
)
PC18s$nbactiviteexterieur_cat <-
  case_when(
    PC18s$nbactiviteexterieur %in% 0:2 ~ "<=2",
    PC18s$nbactiviteexterieur %in% 3:4 ~ "3 à 4",
    PC18s$nbactiviteexterieur %in% 5:6 ~ "5 à 6",
    TRUE ~ ">=7"
  )

PC18s$nbactiviteexterieur_cat <- factor(PC18s$nbactiviteexterieur_cat, levels = c("<=2", "3 à 4", "5 à 6", ">=7"))



# Labelisation =================================================================

var_label(PC18s) <- list(
  sexe_r = "Sexe",
  age_classe = "Tranche d'âge",
  situa_rec = "Situation professionnelle",
  revenu = "Revenu",
  PCS_menage = "Catégorie socioprofessionnelle du ménage",
  UU = "Unité urbaine",
  UU_cat = "Type d'unité urbaine",
  diplome = "Diplôme le plus élevé",
  NbConcert_dic = "A assisté à un concert dans l'année",
  NbSpe_cat = "Nombre de spectacles par an",
  nbactint_cat = "Nombre d'activités d'intérieur",
  nbactiviteexterieur_cat = "Nombre d'activités d'extérieur",
  nbautresorties_cat = "Nombre d'autres sorties",
  nbvisites_cat = "Nombre de visites de lieux culturels",
  pechasse = "Pêche ou chasse",
  jardin = "Jardinage",
  potag = "Potager",
  brideco = "Bricolage/décoration",
  cuisin = "Cuisine",
  jxsoc = "Jeux de société",
  tunn = "Tunning",
  collec = "Collection",
  journ = "Ecriture dans un journal",
  ecritRomPo = "Ecriture de romans/poèmes",
  Montage = "Montage audiovisuel",
  Cirq = "Pratique du cirque",
  Theat = "Pratique du théâtre",
  Recher = "Recherche généalogique",
  Scien = "Activité scientifique",
  JV = "Jeux vidéo",
  TV = "Télévision",
  EcouMus = "Ecoute de musique",
  Serie = "Séries",
  Bibl = "Bibliothèque",
  Cinema = "Cinéma",
  Bal = "Bal",
  SpecSpor = "Spectacle sportif",
  BdN = "Boite de Nuit",
  SpecDan = "Spectacle de danse",
  SpecCirq = "Spectacle de cirque",
  SpecRue = "Spectacle de rue",
  SpecThea = "Spectacle de théâtre",
  Radio_15_35 = "Radio 15-35 ans",
  Radio_36_55 = "Radio 36-55 ans",
  Radio_56_75 = "Radio 56-75 ans",
  Radio_75_plus = "Radio 76 ans et plus",
  TpsPro_cat = "Temps de travail"
)

