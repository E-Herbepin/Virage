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

### Nationalité ----

DB <- mutate(DB,
                   Natio = case_when(
                     Q22E_01 == 1 ~ "Française de naissances",
                     Q22E_02 == 1 ~ "Française par acquisition",
                     Q22E_03 == 1 ~ "Etranger",
                     Q22E_04 == 1 ~ "Apatride",
                     Q22E_01 == 88 | Q22E_02 == 88 | Q22E_03 == 88 | Q22E_04 == 88 ~ "NVPD",
                     Q22E_01 == 99 | Q22E_02 == 99 | Q22E_03 == 99 | Q22E_04 == 99 ~ "NSP"
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

DB <- mutate(DB,
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
                                     "NSP" ))),
                   Couple = factor(Couple, levels = c("Oui", "Célibat < 1an", "Célibat > 1an","Célibat total", "NVPD", "NSP")),
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
                   matricouple = factor(Q7a,
                                        c(1, 2, 3, 4, 88, 99),
                                        labels = c(
                                          "Célibataire",
                                          "Marié-e",
                                          "Divorcé-e",
                                          "Veuf-ve",
                                          "NVPD",
                                          "NSP")),
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
                                         "NSP")))


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
  matricouple = "Matrimonial du couple",
  habitecouple = "Habite en couple",
  dureecouple = "Durée de la relation de couple",
  dureecohab = "Durée de la cohabitation"
)

