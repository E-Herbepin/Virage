# Socio démo ----

### Pondération ----
Basetemp <- Basetemp %>% 
  filter(!is.na(poids_cal)) %>% 
  mutate(POND = poids_cal/(sum(poids_cal)/nrow(.))
  ) #Pour l'ACM et les tableaux : le poids somme à la taille de l'échantillon

Basetemp <- Basetemp %>% 
  filter(!is.na(poids_cal)) %>% 
  mutate(PONDUNI = poids_cal/sum(poids_cal)
) #Pour la régression, le poids somme à 1

### Sexe ----
  
Basetemp <- mutate(Basetemp, sexe = factor(Q1, c(1, 2), labels = c("Homme", "Femme")))

### Age ----

Basetemp <- mutate(Basetemp,
               age = Q19E_age)

### Situation ----

Basetemp <- mutate(Basetemp,
               situa = factor(
                 Q25E,
                 c(1:13),
                 labels = c(
                   "En emploi",
                   "Au chômage avec indemnités",
                   "Au chômage sans indemnités",
                   "En retraite",
                   "En congé parental ou de solidarité familiale",
                   "Autre congé de longue durée",
                   "Etudiant-e, élève",
                   "Etudiant-e, élève avec stage non rémunéré",
                   "Etudiant-e, élève avec emploi",
                   "Inactif-ve ou au foyer ayant déjà travaillé",
                   "Inactif-ve ou au foyer n'ayant jamais travaillé",
                   "NVPD",
                   "NSP"
                 )))

Basetemp <- mutate(Basetemp,
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

Basetemp <- mutate(Basetemp,
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

Basetemp <- mutate(Basetemp,
               TailleAglo = factor(
                 Q1,
                 c(1, 2, 3, 4, 5, 6, 7, 88, 99),
                 labels = c(
                   "Paris",
                   "plus d'un million d'habitants",
                   "200 000 à un million habitants",
                   "100 000 à 200 000 habitants",
                   "20 000 à 100 000 habitants",
                   "moins de 20 000 habitants",
                   "moins de 2 000 habitants (village)",
                   "NVPD",
                   "NSP"
                 )
               ))

#Recodage du type de commune
# Basetemp <- 
#   Basetemp %>% 
#   mutate(UU_cat = 
#            case_when(TUU2016 %in% 0:2 ~ "Com. rurale et petite unité urbaine",
#                      TUU2016 %in% 3:6 ~ "Unité urbaine moyenne",
#                      TUU2016 %in% 7:8 ~ "Grande unité urbaine",
#                      .default = NA),
#          UU_cat = fct_relevel(UU_cat, 
#                                     "Com. rurale et petite unité urbaine",
#                                     "Unité urbaine moyenne",
#                                     "Grande unité urbaine"))

### Nationalité ----

Basetemp <- mutate(Basetemp,
                   Natio = case_when(
                     Q22E_01 == 1 ~ "Française de naissances",
                     Q22E_02 == 1 ~ "Française par acquisition",
                     Q22E_03 == 1 ~ "Etranger",
                     Q22E_04 == 1 ~ "Apatride",
                     Q22E_01 == 88 | Q22E_02 == 88 | Q22E_03 == 88 | Q22E_04 == 88 ~ "NVPD",
                     Q22E_01 == 99 | Q22E_02 == 99 | Q22E_03 == 99 | Q22E_04 == 99 ~ "NSP"
                   ))

### Diplôme ----

Basetemp <- mutate(Basetemp,
                Diplome = factor(
                  Q29E,
                  c(0,10,20,21,22,23,24,30,31,32,33,40,41,42,43,44,50,51,52,53,54,55,56,60,61,62,63,70,71,72,73,74,75,76,77,78,80,81,82,83,88,99),
                  labels = c(
                    "Aucun diplôme",
                    "Primaire : CEP",
                    "Secondaire : BEPC",
                    "Secondaire : CAP",
                    "Secondaire : BEP",
                    "Secondaire : Diplômes d'état d'assistant-e familial-e, d'aide-soignant-e, d'auxiliaire de vie sociale, d'aide médico-psychologique",
                    "Secondaire : Autre diplôme de niveau collège ou lycée",
                    "Bac : Baccalauréat général",
                    "Bac : Baccalauréat technologique ou professionnel",
                    "Bac : Capacité en droit, DAEU, ESEU, Brevet de Technicien, Brevet des métiers d'art…",
                    "Bac : Autre diplôme équivalent au bac",
                    "Bac +2 : DEUG",
                    "Bac +2 : BTS, DUT, DEUST, DSTS, DEIS (ingénierie sociale)",
                    "Bac +2 : Diplôme des professions sociales et de la santé de niveau bac+2 (assistant-e social-e, éducateur-trice, infirmier-ère…)",
                    "Bac +2 : Diplômes de 1er cycle du CNAM, Diplôme des métiers d'art",
                    "Bac +2 : Autre",
                    "Bac +3 : Licence (L3)",
                    "Bac +3 : Certificat d'aptitude pédagogique, Diplôme d'études supérieures d'instituteur",
                    "Bac +3 : Bachelor en école de commerce",
                    "Bac +3 : Diplôme d'état d'infirmier-e, puéricultrice, masseurkinésithérapeute, psychomotricien-ne, capacité d'orthophonie…",
                    "Bac +3 : Diplôme de formation générale en sciences maïeutiques
                    (DFGSMa) (sage femmes)",
                    "Bac +3 : Diplôme de formation générale en sciences médicales (DFGSM3),
                    pharmaceutiques (DFGSP), d’odontologie (DFGSO)",
                    "Bac +3 : Autre",
                    "Bac +4 : Maîtrise, MST, licence en 4 ans",
                    "Bac +4 : Diplôme d'une grande école de niveau bac +4 (ingénieur, commerce...)",
                    "Bac +4 : Diplômes d'études supérieures du CNAM",
                    "Bac +4 : Autre",
                    "Bac +5 : CAPES, CAPA, Agrégation (pour enseigner au lycée)",
                    "Bac +5 : Master enseignement (MEEF)",
                    "Bac +5 : Master professionnel ou recherche (M2, y compris master IEP)",
                    "Bac +5 : DESS, DEA, DESup",
                    "Bac +5 : Diplôme d'une grande école de niveau master (ingénieur, commerce...)",
                    "Bac +5 : Diplôme d’ingénieur universitaire",
                    "Bac +5 : Diplôme d’état de sage-femme, diplôme de formation approfondie en sciences maïeutiques (DFASMa)",
                    "Bac +5 : Diplôme de recherche technologique",
                    "Bac +5 : Autre diplôme de niveau bac +5",
                    "Bac +6 et plus : Doctorat, HDR, Agrégation",
                    "Bac +6 et plus : Domaine médical (y compris DU et DIU de spécialités)",
                    "Bac +6 et plus : Autre (architecte DPLG…)",
                    "Bac +6 et plus : Autre",
                    "NVPD",
                    "NSP"
                  )
                ))









Basetemp <- mutate(Basetemp,
                TpsPro = S11_C_1 + S12_C_1/60,
                TpsPro_cat = case_when(
                  TpsPro < 35 ~ "<35",
                  TpsPro <40 ~ "35-39",
                  TpsPro >=40 ~ ">=40",
                  is.na(TpsPro) ~ "Sans emploi"),
                TpsPro_cat = factor(TpsPro_cat, levels = c("Sans emploi","<35", "35-39", ">=40")))


# PCS

Basetemp <-
  Basetemp %>% 
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




## Recodage nb concert ----

for (x in names(select(Basetemp, contains("val")))) {
  Basetemp[x][is.na(Basetemp[x])] <- 0}

for (x in names(select(Basetemp, contains("unit")))) {
  Basetemp[x] <- case_when(
    Basetemp[x] == 1 ~ 52,
    Basetemp[x] == 2 ~ 12,
    is.na(Basetemp[x]) ~ 1,
    TRUE ~ 1)}

for (x in names(select(Basetemp, contains("unit")))) {
  y<-paste0(substring(x,0,3),substring(x,8))
  Basetemp[y]<-Basetemp[x]*Basetemp[paste0(substring(x,0,3),"val",substring(x,8))]}


Basetemp <- mutate(
  Basetemp,
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

Basetemp$NbConcert_dic <- case_when(
  Basetemp$NbConcert > 0 ~ "Oui",
  TRUE ~ "Non"
)

## Recodage nb spectacle


Basetemp <- mutate(
  Basetemp,
  NbSpe =
    G14unit_theatre * G14val_theatre +
    G14unit_spectacle_rue * G14val_spectacle_rue +
    G14unit_cirque * G14val_cirque +
    G14unit_danse * G14val_danse
)

Basetemp$NbSpe_cat <- case_when(Basetemp$NbSpe == 0 ~ "Jamais",
                             Basetemp$NbSpe %in% c(1, 2) ~ "1 ou 2 par an",
                             TRUE ~ "3 ou plus par an")


## Recodage autre sorties


Basetemp <- mutate(
  Basetemp,
  nbautresorties =
    ifelse(is.na(H204), 0, H204) +
    ifelse(is.na(H205), 0, H205) +
    ifelse(is.na(H206), 0, H206) +
    ifelse(is.na(H206), 0, H207) +
    ifelse(is.na(H208), 0, H208) +
    ifelse(is.na(H206), 0, H209) +
    ifelse(is.na(H210), 0, H210) +
    ifelse(is.na(H211), 0, H211))


Basetemp$nbautresorties_cat <-
  case_when(Basetemp$nbautresorties >= 3 ~ "3 et +",
            Basetemp$nbautresorties >= 1 ~ "1 ou 2",
            TRUE ~ "0")


## Recodage visites


Basetemp <- mutate(
  Basetemp,
  nbvisites =
    ifelse(is.na(H1001), 0, H1001) +
    ifelse(is.na(H1002), 0, H1002) +
    ifelse(is.na(H1003), 0, H1003) +
    ifelse(is.na(H1004), 0, H1004) +
    ifelse(is.na(H1005), 0, H1005) +
    ifelse(is.na(H1006), 0, H1006))

Basetemp$nbvisites_cat <- case_when(Basetemp$nbvisites == 0 ~ "0",
                                 Basetemp$nbvisites %in% 1:2 ~ "1 ou 2",
                                 TRUE ~ "3 et +")


# Recodages supplémentaires
Basetemp <- mutate(
  Basetemp,
  pechasse = ifelse(A1008==1,'Oui','Non'),
  jardin = ifelse(A1007==1,'Oui','Non'),
  potag = ifelse(A1006==1,'Oui','Non'),
  brideco = ifelse(A1005==1,'Oui','Non'),
  cuisin = ifelse(A1004==1,'Oui','Non'),
  jxsoc = ifelse(A1002==1,'Oui','Non'),
  tunn = ifelse(A1010==1,'Oui','Non'),
  collec = ifelse(A1009==1,'Oui','Non')
)

Basetemp <- mutate(
  Basetemp,
  journ = ifelse(is.na(A21_journal),'Non',ifelse(A21_journal==1,'Oui','Non')),
  ecritRomPo = ifelse(is.na(A21_romans),'Non',ifelse(A21_romans==1,'Oui','Non')),
  Montage = ifelse(is.na(A21_montages),'Non',ifelse(A21_montages==1,'Oui','Non')),
  Cirq = ifelse(is.na(A21_cirque),'Non',ifelse(A21_cirque==1,'Oui','Non')),
  Theat = ifelse(is.na(A21_theatre),'Non',ifelse(A21_theatre==1,'Oui','Non')),
  Recher = ifelse(is.na(A21_genealogie),'Non',ifelse(A21_genealogie==1,'Oui','Non')),
  Scien = ifelse(is.na(A21_activite_scientifique),'Non',ifelse(A21_activite_scientifique==1,'Oui','Non'))
)

Basetemp <- mutate(
  Basetemp,
  JV = ifelse(B1==1,'Oui','Non'),
  TV = ifelse(C1 %in% 1:2,'Oui','Non'), # On considère que regarder la TV est une activité d'intérieur lorsqu'elle prend une certaine ampleur (+ de 3 fois par semaine) temporelle pour différencier notre population
  EcouMus = ifelse(E17 %in% 1:2,'Oui','Non'), # De même pour l'écoute à la maison de musique avec un seuil de 'Oui, de temps en temps'
  Serie = ifelse(C31 %in% 1:3,'Oui','Non'), # De même pour les séries, avec un seuil de 'Au moins une fois par mois'
  Bibl = ifelse(F3 %in% 1:4,'Oui','Non'),
  Cinema = ifelse(G3A ==1 ,'Oui','Non'), # NSP = doute, si doute alors ce n'est pas une activité culturelle marquante
  )

Basetemp <- mutate(
  Basetemp,
  Bal = ifelse(G213==1|is.na(G213),'Oui','Non'),
  SpecSpor = ifelse(G214==1|is.na(G214),'Oui','Non'),
  BdN = ifelse(G215==1|is.na(G215),'Oui','Non'),
  SpecDan = ifelse(G131==1|is.na(G131),'Oui','Non'),
  SpecCirq = ifelse(G132==1|is.na(G132),'Oui','Non'),
  SpecRue = ifelse(G133==1|is.na(G133),'Oui','Non'),
  SpecThea = ifelse(G134==1|is.na(G134),'Oui','Non')
)

Basetemp <- mutate(
  Basetemp,
  Radio_15_35 = ifelse(Basetemp$age_classe != "15 à 35 ans" | Basetemp$E1 %in% c(4:7), "0", "1"),
  Radio_36_55 = ifelse(age_classe != "36 à 55 ans" | E1 %in% c(4:7), "0", "1"),
  Radio_56_75 = ifelse(age_classe != "56 à 75 ans" | E1 %in% c(4:7), "0", "1"),
  Radio_75_plus = ifelse(age_classe != "76 ans et plus" | E1 %in% c(4:7), "0", "1"))


## Recodage diversité activité d'intérieur

Basetemp <- mutate(
  Basetemp,
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


Basetemp$nbactint_cat <-
  case_when(
    Basetemp$nbactint %in% 0:3 ~ "<=3",
    Basetemp$nbactint %in% 4:5 ~ "4 à 5",
    TRUE ~ ">=6"
  )
Basetemp$nbactint_cat <-
  case_when(
    Basetemp$nbactint %in% 0:2 ~ "<=2",
    Basetemp$nbactint %in% 3:4 ~ "3 à 4",
    Basetemp$nbactint %in% 5:6 ~ "5 à 6",
    TRUE ~ ">=7"
  )

Basetemp$nbactint_cat <- factor(Basetemp$nbactint_cat, levels = c("<=2", "3 à 4", "5 à 6", ">=7"))

## Recodage diversité activité d'extérieur

Basetemp <- mutate(
  Basetemp,
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
Basetemp$nbactiviteexterieur_cat <-
  case_when(
    Basetemp$nbactiviteexterieur %in% 0:2 ~ "<=2",
    Basetemp$nbactiviteexterieur %in% 3:4 ~ "3 à 4",
    Basetemp$nbactiviteexterieur %in% 5:6 ~ "5 à 6",
    TRUE ~ ">=7"
  )

Basetemp$nbactiviteexterieur_cat <- factor(Basetemp$nbactiviteexterieur_cat, levels = c("<=2", "3 à 4", "5 à 6", ">=7"))



# Labelisation =================================================================

var_label(Basetemp) <- list(
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

