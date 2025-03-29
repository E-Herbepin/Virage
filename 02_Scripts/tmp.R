# Recode : T1

DB %<>% mutate(# Recodage de T1
  critiques_rabaissement_humiliations = factor(
    T1,
    levels = c(0:3, 88, 99),
    labels = c(
      "Non",
      "Oui, une fois",
      "Oui, plusieurs fois",
      "Oui, plusieurs fois",
      "NVPD",
      "NSP"
    )
  ))








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


  








c(paste0("T", 1:16), "T12m_cible")