# T12m_cible. Filtre d'entrée du module Travail 12 mois
DB %<>%
  rename(
    t_ind = T12m_cible,
    t_humiliation = T1,
    t_reputation = T2,
    t_organisation = T3,
    t_communication = T4,
    t_sabotage = T5,
    t_intimidation = T7,
    t_frappe = T8,
    t_menace = T9,
    t_propos_sex = T11,
    t_propositions_sex = T12,
    t_exhib = T14,
    t_attouchements = T15,
    t_viol = T16
  ) %>%
  mutate(across(
    c(
      t_reputation,
      t_humiliation,
      t_organisation,
      t_communication,
      t_sabotage,
      t_intimidation,
      t_frappe,
      t_menace,
      t_propos_sex,
      t_propositions_sex,
      t_exhib,
      t_attouchements,
      t_viol
    ),
    ~ factor(
      .,
      levels = c(0:4, 77, 88, 99),
      labels = c(
        "Jamais",
        "Une fois",
        "Quelques fois (2 à 5 fois)",
        "Souvent (6 fois ou plus)",
        "Presque toutes les semaines",
        "Non concerné",
        "NVPD",
        "NSP"
      )
    )
  ),
  across(., ~ factor(
    .,
    levels = (0, 2, 3, 88, 99),
    labels = c("Pas grave", "Très grave", "Assez grave", "NVPD", "NSP")
  )),
  across(ends_with("dP"), ~ factor(
    .,
    levels = (0:3, 88, 99),
    labels = c(
      "Tous sans gravité",
      "Tous très graves",
      "Au moins un très grave",
      "Au moins un assez grave",
    )
  )))




DB %<>% mutate(# Recodage de CF6
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
  ))