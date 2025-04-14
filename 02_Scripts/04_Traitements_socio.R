# Caractérisation sociale ----------------------------

# Liste des variables :
#  c(
#   "sexe", "age", "situa", "TypeEmp", "TailleAglo", "Natio", "Statumigration", "Diplome", "CSP_3", "CSP_1", "Couple", "Etat_matrimonial", "typecouple", "habitecouple", "dureecouple", "dureecohab", "logement", "nb_personnes_logement", "nb_personnes_moins16", "revenus", "nb_enfants", "type_menage_5mod", "type_menage_9mod", "religion", "importance_religion", "religion_conjoint", "importance_religion_conjoint", "age_1_mariage"
#  )

#Ce script aura pour objectif de faire des tris à plats de chacunes des variables socio-démographiques, et de faire des barplot lorsque nécessaire (nottament les variablees numeriques)

# On commence par charger les librairies nécessaires
library(tidyverse)
# Sexe

DB %>%
  group_by(sexe) %>%
  summarize(count = n(), prop = n() / nrow(DB))

# Age
DB %>%
  group_by(age) %>%
  summarize(count = n(), prop = 100 * count / nrow(DB)) %>%
  ggplot(aes(x = age, y = prop)) +
  geom_bar(stat = "identity") +
  labs(title = "Répartition des âges", x = "Âge", y = "Proportion") +
  theme_minimal()

# On continue avec la situation professionnelle
DB %>%
  group_by(situa) %>%
  summarize(count = n(), prop = 100 * count / nrow(DB))

DB %>%
  group_by(situarec) %>%
  summarize(count = n(), prop = 100 * count / nrow(DB))

# type d'emploi
DB %>%
  summarize(count = n(),
            prop = 100 * count / nrow(DB),
            .by = TypeEmp)

DB %>%
  filter(!is.na(TypeEmprec)) %>%
  summarize(count = n(),
            prop = 100 * count / nrow(DB),
            .by = TypeEmprec)

# On continue avec la taille de l'agglomération
DB %>%
  summarize(count = n(),
            prop = 100 * count / nrow(DB),
            .by = TailleAglo) %>%
  ggplot(aes(x = TailleAglo, y = prop)) +
  geom_bar(stat = "identity") +
  labs(title = "Répartition des tailles d'agglomération", x = "Taille d'agglomération", y = "Proportion") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# On continue avec la nationalité
DB %>%
  group_by(Natio) %>%
  summarize(count = n(), prop = 100 * count / nrow(DB))

# statut de migration
DB %>%
  group_by(Statumigration) %>%
  summarize(count = n(), prop = 100 * count / nrow(DB))
# à recoder

# Diplôme
DB %>%
  group_by(Diplome) %>%
  summarize(count = n(), prop = 100 * count / nrow(DB)) %>%
  ggplot(aes(x = Diplome, y = prop)) +
  geom_bar(stat = "identity") +
  labs(title = "Répartition des diplômes", x = "Diplôme", y = "Proportion") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
# à recoder

# On continue avec la CSP_3
DB %>%
  group_by(CSP_3) %>%
  summarize(count = n(), prop = 100 * count / nrow(DB)) %>%
  ggplot(aes(x = CSP_3, y = prop)) +
  geom_bar(stat = "identity") +
  labs(title = "Répartition des CSP_3", x = "CSP_3", y = "Proportion") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
# à recoder

# On continue avec la CSP_1
DB %>%
  group_by(CSP_1) %>%
  summarize(count = n(), prop = 100 * count / nrow(DB)) %>%
  ggplot(aes(x = CSP_1, y = prop)) +
  geom_bar(stat = "identity") +
  labs(title = "Répartition des CSP_1", x = "CSP_1", y = "Proportion") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# couple
DB %>%
  group_by(Couple) %>%
  summarize(count = n(), prop = 100 * count / nrow(DB))

# On continue avec l'état matrimonial
DB %>%
  group_by(Etat_matrimonial) %>%
  summarize(count = n(), prop = 100 * count / nrow(DB))

# type de couple
DB %>%
  group_by(typecouple) %>%
  summarize(count = n(), prop = 100 * count / nrow(DB))

# Habiter en couple
DB %>%
  filter(!is.na(habitecouple)) %>%
  summarize(count = n(), .by = habitecouple) %>%
  mutate(prop = 100 * count / sum(count))

# Durée du couple
DB %>%
  filter(!is.na(dureecouple)) %>%
  summarize(count = n(), .by = dureecouple) %>%
  mutate(prop = 100 * count / sum(count))
#Très peu de couple ?

# Durée de cohabitation
DB %>%
  filter(!is.na(dureecohab)) %>%
  summarize(count = n(), .by = dureecohab) %>%
  mutate(prop = 100 * count / sum(count))
#Très peu de cohabitation ?

# Type de logement
DB %>%
  filter(!is.na(logement)) %>%
  summarize(count = n(),
            prop = 100 * count / nrow(DB),
            .by = logement)

# Nombre de personnes dans le logement
DB %>%
  filter(!is.na(nb_personnes_logement)) %>%
  summarize(count = n(), .by = nb_personnes_logement) %>%
  mutate(prop = 100 * count / sum(count))

# Nombre de personnes de moins de 16 ans dans le logement
DB %>%
  filter(!is.na(nb_personnes_moins16)) %>%
  summarize(count = n(), .by = nb_personnes_moins16) %>%
  mutate(prop = 100 * count / sum(count))

# Revenus
DB %>%
  filter(!is.na(revenus)) %>%
  summarize(count = n(), .by = revenus) %>%
  mutate(prop = 100 * count / sum(count)) %>%
  ggplot(aes(x = revenus, y = prop)) +
  geom_bar(stat = "identity") +
  labs(title = "Répartition des revenus", x = "Revenus", y = "Proportion") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Nombre d'enfants
DB %>%
  filter(!is.na(nb_enfants)) %>%
  summarize(count = n(), .by = nb_enfants) %>%
  mutate(prop = 100 * count / sum(count)) %>%
  ggplot(aes(x = nb_enfants, y = prop)) +
  geom_bar(stat = "identity") +
  labs(title = "Répartition du nombre d'enfants", x = "Nombre d'enfants", y = "Proportion") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
# à recoder

# Type de ménage 5 mod
DB %>%
  filter(!is.na(type_menage_5mod)) %>%
  summarize(count = n(), .by = type_menage_5mod) %>%
  mutate(prop = 100 * count / sum(count))

# Type de ménage 9 mod
DB %>%
  filter(!is.na(type_menage_9mod)) %>%
  summarize(count = n(), .by = type_menage_9mod) %>%
  mutate(prop = 100 * count / sum(count))

# Religion
DB %>%
  filter(!is.na(religion)) %>%
  summarize(count = n(), .by = religion) %>%
  mutate(prop = 100 * count / sum(count))

# Importance de la religion
DB %>%
  filter(!is.na(importance_religion)) %>%
  summarize(count = n(), .by = importance_religion) %>%
  mutate(prop = 100 * count / sum(count))

# Religion du conjoint
DB %>%
  filter(!is.na(religion_conjoint)) %>%
  summarize(count = n(), .by = religion_conjoint) %>%
  mutate(prop = 100 * count / sum(count))

# Importance de la religion du conjoint
DB %>%
  filter(!is.na(importance_religion_conjoint)) %>%
  summarize(count = n(), .by = importance_religion_conjoint) %>%
  mutate(prop = 100 * count / sum(count))

# Age au premier mariage
DB %>%
  filter(!is.na(age_1_mariage)) %>%
  summarize(count = n(), .by = age_1_mariage) %>%
  mutate(prop = 100 * count / sum(count)) %>%
  ggplot(aes(x = age_1_mariage, y = prop)) +
  geom_bar(stat = "identity") +
  labs(title = "Répartition de l'âge au premier mariage", x = "Âge au premier mariage", y = "Proportion") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))