## Violence Couple ----

DB %<>% mutate(
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
  crier_insulter =
    factor(
      CF7a,
      levels = c(1:3, 88, 99),
      labels = c("Non", "Parfois", "Souvent", "NVPD", "NSP")
    ),
  
  # Recodage de CF7b
  gifler_enfant =
    factor(
      CF7b,
      levels = c(1:3, 88, 99),
      labels = c("Non", "Parfois", "Souvent", "NVPD", "NSP")
    ),
  
  # Recodage de CF7c
  gifler_adulte =
    factor(
      CF7c,
      levels = c(1:3, 88, 99),
      labels = c("Non", "Parfois", "Souvent", "NVPD", "NSP")
    ),
  
  # Recodage de SOC1a
  confie_conjoint =
    factor(
      SOC1a,
      levels = c(0:2, 88, 99),
      labels = c("Non", "Oui", "Ça dépend du problème", "NVPD", "NSP")
    ),
  
  # Recodage de SOC2a
  contacts_famille =
    factor(
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
  contacts_amis =
    factor(
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
  activites_loisir =
    factor(
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
  activites_associatives =
    factor(
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
  peur_seul =
    factor(
      SOC5,
      levels = c(0:1, 88, 99),
      labels = c("Non", "Oui", "NVPD", "NSP")
    ),
  
  # Recodage de SOC6
  surveille_intrusion =
    factor(
      SOC6,
      levels = c(0:1, 88, 99),
      labels = c("Non", "Oui", "NVPD", "NSP")
    )
)