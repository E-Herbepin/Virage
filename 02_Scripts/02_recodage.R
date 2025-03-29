# Socio démo ----

### Pondération ----
DB <- DB %>% 
  filter(!is.na(poids_cal)) %>% 
  mutate(POND = poids_cal/(sum(poids_cal)/nrow(.))
  ) #Pour l'ACM et les tableaux : le poids somme à la taille de l'échantillon

DB <- DB %>% 
  filter(!is.na(poids_cal)) %>% 
  mutate(PONDUNI = poids_cal/sum(poids_cal)
) #Pour la régression, le poids somme à 1

### Sexe ----
  
DB <- mutate(DB, sexe = factor(Q1, c(1, 2), labels = c("Homme", "Femme")))

### Age ----

DB <- mutate(DB,
               age = Q19E_age)

### Situation ----

DB <- mutate(DB,
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
                       "NSP")),
                   TypeEmp = factor(
                     EMP3,
                     c(1:8,88,99),
                     labels = c(
                       "Salarié-e État",
                       "Salarié-e  collectivité locale, des HLM ou des hôpitaux publics",
                       "Salarié-e entreprise, artisan",
                       "Salarié-e association",
                       "Salarié-e particulier",
                       "Aide à la famille sans rémunération",
                       "Chef-fe entreprise salarié-e, PDG, gérant-e minoritaire, associé-e",
                       "Indépendant-e, à votre compte",
                       "NVPD",
                       "NSP"
                     )))

DB <- mutate(DB,
               TailleAglo = factor(
                 Q3,
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

### Nationalité ----

DB <- mutate(DB,
                   Natio = case_when(
                     Q22E_01 == 1 ~ "Française de naissances",
                     Q22E_02 == 1 ~ "Française par acquisition",
                     Q22E_03 == 1 ~ "Etranger",
                     Q22E_04 == 1 ~ "Etranger",#7 apatrides
                     Q22E_01 == 88 | Q22E_02 == 88 | Q22E_03 == 88 | Q22E_04 == 88 ~ "Etranger", #9 NVPD dont 7 Nés à l'étranger et 2 NVPD > etranger
                     Q22E_01 == 99 | Q22E_02 == 99 | Q22E_03 == 99 | Q22E_04 == 99 ~ "Etranger" #1 NSP née dans un autre pays  > Etranger
                   ),
                   Statumigration = factor(
                     Mig_e,
                     c(1, 2, 3, 4, 5, 99),
                     labels = c(
                       "Majoritaire",
                       "Né-e dans un DOM",
                       "Immigré-e",
                       "Descendant-e d'1 immigré-e",
                       "Descendant-e de 2 immigré-e-s",
                       "Inclassable"
                     )
                   )
)

### Diplôme ----

DB <- mutate(DB,
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

### CSP ----

DB <- 
  DB %>%
  mutate(CSP_3 = factor(CS_E_NIV3,
                        c(11,12,21,22,23,31,33,34,35,37,38,42,43,44,45,46,47,48,52,53,54,55,56,62,63,64,65,67,68,69,71,72,74,75,77,78,81,84,85,86,91,92,93,94,98,99),
                        labels = c(
                          "Agriculteurs petite exploitation",
                          "Agriculteurs moyenne exploitation",
                          "Artisans",
                          "Commerçants et assimilés",
                          "Chefs d'entreprise de 10+ salariés",
                          "Professions libérales",
                          "Cadres de la fonction publique",
                          "Professeurs, professions scientifiques",
                          "Professions de l'information, des arts et des spectacles",
                          "Cadres administratifs et commerciaux d'entreprise",
                          "Ingénieurs et cadres techniques d'entreprise",
                          "Professeurs des écoles, instituteurs et assimilés",
                          "PI de la santé et du travail social",
                          "Clergé, religieux",
                          "PI administratives de la fonction publique",
                          "PI administratives et commerciales des entreprises",
                          "Techniciens",
                          "Contremaîtres, agents de maîtrise",
                          "Employés civils et agents de service de la fonction publique",
                          "Policiers et militaires",
                          "Employés administratifs d'entreprise",
                          "Employés de commerce",
                          "Personnels des services directs aux particuliers",
                          "Ouvriers qualifiés de type industriel",
                          "Ouvriers qualifiés de type artisanal",
                          "Chauffeurs",
                          "Ouvriers qualifiés de la manutention, du magasinage et du transport",
                          "Ouvriers non qualifiés de type industriel",
                          "Ouvriers non qualifiés de type artisanal",
                          "Ouvriers agricoles",
                          "Anciens agriculteurs exploitants",
                          "Anciens artisans, commerçants, chefs d'entreprise",
                          "Anciens cadres",
                          "Anciennes PI",
                          "Anciens employés",
                          "Anciens ouvriers",
                          "Chômeurs n'ayant jamais travaillé",
                          "Élèves, étudiants",
                          "Sans activité professionnelle de moins de 60 ans (sauf retraités)",
                          "Sans activité professionnelle de 60 ans et plus (sauf retraités)",
                          "Actifs PCS indéterminée",
                          "Au chômage avec indemnité PCS indéterminée",
                          "Au chômage sans indemnité PCS indéterminée",
                          "Retraité PCS indéterminée",
                          "NVPR",
                          "NSP")),
         CSP_1 = factor(CS_E_NIV1,
                        c(1, 2, 3, 4, 5, 6, 7, 8, 9),
                        labels = c(
                          "Agriculteurs exploitants",
                          "Artisans, commerçants et chefs d'entreprise",
                          "Cadres et professions intellectuelles supérieures",
                          "Professions Intermédiaires",
                          "Employés",
                          "Ouvriers",
                          "Retraités",
                          "Autres personnes sans activité professionnelle",
                          "Indéterminé"
                        )
         )
  )

## Situation matrimoniale ----

DB <- DB %>%
  mutate(
    
    Couple = case_when(
      Q16 <= 120 ~ "Célibat < 1an",# 1 an semble être le début de la queue de distribution
      Q16 > 120 ~ "Célibat > 1an",
      TRUE ~ factor(Q6,
                    c(1, 2, 3, 4, 88, 99),
                    labels = c(
                      "Oui",
                      "Oui",# On ne garde pas la distinction avec les polya
                      "Non aucune, déjà eu une relation de couple",
                      "Célibat total",
                      "NVPD",
                      "NSP" ))) %>% 
      factor( levels = c("Oui", "Célibat < 1an", "Célibat > 1an","Célibat total", "NVPD", "NSP")),
    
    Etat_matrimonial = factor(Etatmat,
                              c(1, 2, 3, 4, 88, 99),
                              labels = c(
                                "Célibataire",
                                "Marié-e",
                                "Divorcé-e",
                                "Veuf-ve",
                                "NVPD",
                                "NSP"
                              )),
    
    typecouple = factor(Situmat,
                        c(1, 2, 3, 4, 88, 99),
                        labels = c(
                          "Célibataire",
                          "Marié-e",
                          "Pacsé-e",
                          "En union libre",
                          "NVPD",
                          "NSP"
                        )),
    
    habitecouple = factor(Q8,
                          c(0, 1, 88, 99),
                          labels = c(
                            "Non",
                            "Oui",
                            "NVPD",
                            "NSP")),
    
    dureecouple = factor(Dur_relconj,
                         c(0, 1, 2, 3, 88, 99),
                         labels = c(
                           "Moins d'un mois",
                           "1 à 11 mois",
                           "1 à 55 ans",
                           "55 ans ou plus",
                           "NVPD",
                           "NSP")),
    
    dureecohab = factor(Dur_cohab,
                        c(0, 1, 2, 3, 88, 99),
                        labels = c(
                          "Moins d'un mois",
                          "1 à 11 mois",
                          "1 à 55 ans",
                          "55 ans ou plus",
                          "NVPD",
                          "NSP")),
    
    logement = factor(
      Q18a,
      levels = c(1:10, 88, 99),
      labels = c(
        "Propriétaire ensemble (ou en accès à la propriété)",
        "Vous seul-e propriétaire (ou en accès à la propriété)",
        "Votre conjoint-e seul-e propriétaire",
        "Locataires avec bail aux deux noms",
        "Locataire avec bail à mon seul nom",
        "Locataire avec bail au seul nom de votre conjoint-e",
        "En colocation",
        "En logement de fonction",
        "Hébergé à titre gratuit (y compris les enfants chez leurs parents)",
        "Autre",
        "NVPD",
        "NSP"
      )
    ),
    
    # Recodage de Q5 : Nombre de personnes vivant dans le logement
    nb_personnes_logement = factor(
      Q5,
      levels = c(1:12, 16),
      labels = c(
        "1 personne",
        "2 personnes",
        "3 personnes",
        "4 personnes",
        "5 personnes",
        "6 personnes",
        "7 personnes",
        "8+ personnes",
        "8+ personnes",
        "8+ personnes",
        "8+ personnes",
        "8+ personnes",
        "8+ personnes"
      )
    ),
    
    # Recodage de Q5b : Nombre de personnes de moins de 16 ans
        nb_personnes_moins16 = ifelse(Q5b == 88, 0, as.numeric(Q5b)), # 3 NVPD
    
    # Recodage de REV2 : Revenus mensuels
    revenus = factor(
      REV2,
      levels = c(0:8, 88, 99),
      labels = c(
        "Aucun revenu",
        "Moins de 700 euros",
        "De 700 à moins de 1000 euros",
        "De 1000 à moins de 1300 euros",
        "De 1300 à moins de 1600 euros",
        "De 1600 à moins de 2000 euros",
        "De 2000 à moins de 2500 euros",
        "De 2500 à moins de 3000 euros",
        "Plus de 3000 euros",
        "NVPD",
        "NSP"
      )
    ),
    
    # Recodage de Enf1 : Nombre total d'enfants de l'enquêté-e
    nb_enfants = as.numeric(Enf1),
    
    # Recodage de Typmen_5mod : Type de ménage en 5 modalités
    type_menage_5mod = factor(
      Typmen_5mod,
      levels = c(1:5),
      labels = c(
        "Personne seule",
        "Couple sans enfant",
        "Famille monoparentale",
        "Couple avec enfant",
        "Autre type de ménage"
      )
    ),
    
    # Recodage de Typmen_9mod : Type de ménage en 9 modalités
    type_menage_9mod = factor(
      Typmen_9mod,
      levels = c(1:9),
      labels = c(
        "Personne seule",
        "Couple sans enfant",
        "Famille monoparentale - Ego parent",
        "Famille monoparentale - Ego enfant",
        "Couple avec enfant - Ego parent",
        "Couple avec enfant - Ego enfant",
        "Autre type de ménage avec conjoint ou ex-conjoint",
        "Autre type de ménage avec autre membre parenté",
        "Autre type de ménage sans membre parenté"
      )
    ),
    
    # Recodage de CF6
    mains_lancer_casser = factor(
      CF6,
      levels = c(0:3, 88, 99),
      labels = c(
        "Non",
        "Seulement vous",
        "Seulement votre conjoint-e",
        "Tous / toutes les deux",
        "NVPD",
        "NSP"
      )
    ),
    
    # Recodage de CF7a
    crier_insulter = factor(
      CF7a,
      levels = c(1:3, 88, 99),
      labels = c(
        "Non",
        "Parfois",
        "Souvent",
        "NVPD",
        "NSP"
      )
    ),
    
    # Recodage de CF7b
    gifler_enfant = factor(
      CF7b,
      levels = c(1:3, 88, 99),
      labels = c(
        "Non",
        "Parfois",
        "Souvent",
        "NVPD",
        "NSP"
      )
    ),
    
    # Recodage de CF7c
    gifler_adulte = factor(
      CF7c,
      levels = c(1:3, 88, 99),
      labels = c(
        "Non",
        "Parfois",
        "Souvent",
        "NVPD",
        "NSP"
      )
    ),
    
    # Recodage de SOC1a
    confie_conjoint = factor(
      SOC1a,
      levels = c(0:2, 88, 99),
      labels = c(
        "Non",
        "Oui",
        "Ça dépend du problème",
        "NVPD",
        "NSP"
      )
    ),
    
    # Recodage de SOC2a
    contacts_famille = factor(
      SOC2a,
      levels = c(0:3, 88, 99),
      labels = c(
        "Non",
        "De temps en temps",
        "Souvent",
        "Très souvent",
        "NVPD",
        "NSP"
      )
    ),
    
    # Recodage de SOC2b
    contacts_amis = factor(
      SOC2b,
      levels = c(0:3, 88, 99),
      labels = c(
        "Non",
        "De temps en temps",
        "Souvent",
        "Très souvent",
        "NVPD",
        "NSP"
      )
    ),
    
    # Recodage de SOC2c
    activites_loisir = factor(
      SOC2c,
      levels = c(0:3, 88, 99),
      labels = c(
        "Non",
        "De temps en temps",
        "Souvent",
        "Très souvent",
        "NVPD",
        "NSP"
      )
    ),
    
    # Recodage de SOC2d
    activites_associatives = factor(
      SOC2d,
      levels = c(0:3, 88, 99),
      labels = c(
        "Non",
        "De temps en temps",
        "Souvent",
        "Très souvent",
        "NVPD",
        "NSP"
      )
    ),
    
    # Recodage de SOC5
    peur_seul = factor(
      SOC5,
      levels = c(0:1, 88, 99),
      labels = c(
        "Non",
        "Oui",
        "NVPD",
        "NSP"
      )
    ),
    
    # Recodage de SOC6
    surveille_intrusion = factor(
      SOC6,
      levels = c(0:1, 88, 99),
      labels = c(
        "Non",
        "Oui",
        "NVPD",
        "NSP"
      )
    )
  )


# Labelisation =================================================================

var_label(DB) <- list(
  sexe = "Sexe",
  age = "Age",
  situa = "Situation professionnelle",
  TypeEmp = "Type d'emploi",
  TailleAglo = "Taille de l'agglomération",
  Natio = "Nationalité",
  Statumigration = "Statut migratoire",
  Diplome = "Diplôme",
  CSP_3 = "Catégorie socio-professionnelle (3 niveaux)",
  CSP_1 = "Catégorie socio-professionnelle (1 niveau)",
  Couple = "Situation de couple",
  Etat_matrimonial = "Etat matrimonial",
  typecouple = "Type de couple",
  habitecouple = "Habite en couple",
  dureecouple = "Durée de la relation de couple",
  dureecohab = "Durée de la cohabitation"
)

table(DB$sexe)
table(DB$age)
table(DB$situa)# Aucun NVPD ou NSP ?
table(DB$TypeEmp)
table(DB$TailleAglo)
table(DB$Natio) #Enlever les apatrides NSP et NVPD
table(DB$Statumigration)
table(DB$Diplome) # Recatégoriser, voir les var construite
table(DB$CSP_3) # Vérifier avec les données du dico
table(DB$CSP_1)
table(DB$Couple)
table(DB$Etat_matrimonial)
table(DB$typecouple)
table(DB$habitecouple) # intégrer nvpd et nsp à la proportionnelle ?
table(DB$dureecouple) #PB trop peu d'effectif
table(DB$dureecohab) #Pareil

