### Ambiance familiale ----

DB %<>% mutate(
  # Recodage de EA9a : Privations matérielles ou négligences graves
  privations_materielles <- factor(
    EA9a,
    levels = c("00", "01", "77", "88", "99"),
    labels = c("Non", "Oui", "Non concerné-e", "NVPD", "NSP")
  ),
  
  # Recodage de EA9b : Conflit très grave avec le père
  conflit_pere <- factor(
    EA9b,
    levels = c("00", "01", "77", "88", "99"),
    labels = c("Non", "Oui", "Non concerné-e", "NVPD", "NSP")
  ),
  
  # Recodage de EA9c : Conflit très grave avec la mère
  conflit_mere <- factor(
    EA9c,
    levels = c("00", "01", "77", "88", "99"),
    labels = c("Non", "Oui", "Non concerné-e", "NVPD", "NSP")
  ),
  
  # Recodage de EA9d : Fugue ou expulsion du domicile parental
  fugue_expulsion <- factor(
    EA9d,
    levels = c("00", "01", "77", "88", "99"),
    labels = c("Non", "Oui", "Non concerné-e", "NVPD", "NSP")
  ),
  
  # Recodage de EA9e : Tensions ou violence entre les parents
  tensions_parents <- factor(
    EA9e,
    levels = c("00", "01", "77", "88", "99"),
    labels = c("Non", "Oui", "Non concerné-e", "NVPD", "NSP")
  ),
  
  # Violence au cours de l'enfance ----
  
  # Recodage de EA10d_00 : Sévices, coups répétés sur un membre de la famille
  sevices_non <- factor(
    EA10d_00,
    levels = c("00", "01", "88", "99"),
    labels = c("Non", "Oui", "NVPD", "NSP")
  ),
  
  # Recodage de EA10d_01 : Sévices, coups répétés (Vous-même)
  sevices_vousmeme <- factor(
    EA10d_01,
    levels = c("00", "01", "88", "99"),
    labels = c("Non", "Oui", "NVPD", "NSP")
  ),
  
  # Recodage de EA10d_02 : Sévices, coups répétés (Frères et sœurs)
  sevices_freres_soeurs <- factor(
    EA10d_02,
    levels = c("00", "01", "88", "99"),
    labels = c("Non", "Oui", "NVPD", "NSP")
  ),
  
  # Recodage de EA10d_03 : Sévices, coups répétés (Père)
  sevices_pere <- factor(
    EA10d_03,
    levels = c("00", "01", "88", "99"),
    labels = c("Non", "Oui", "NVPD", "NSP")
  ),
  
  # Recodage de EA10d_04 : Sévices, coups répétés (Mère)
  sevices_mere <- factor(
    EA10d_04,
    levels = c("00", "01", "88", "99"),
    labels = c("Non", "Oui", "NVPD", "NSP")
  ),
  
  # Recodage de EA10d_05 : Sévices, coups répétés (Autres personnes)
  sevices_autres <- factor(
    EA10d_05,
    levels = c("00", "01", "88", "99"),
    labels = c("Non", "Oui", "NVPD", "NSP")
  ),
  
  # Recodage de EA10e1_01 : Lieu des sévices (Chez vous)
  sevices_lieu_chezvous <- factor(
    EA10e1_01,
    levels = c("00", "01"),
    labels = c("Non", "Oui")
  ),
  
  # Recodage de EA10e1_02 : Lieu des sévices (École)
  sevices_lieu_ecole <- factor(
    EA10e1_02,
    levels = c("00", "01"),
    labels = c("Non", "Oui")
  ),
  
  # Recodage de EA10e1_03 : Lieu des sévices (Autre lieu)
  sevices_lieu_autre <- factor(
    EA10e1_03,
    levels = c("00", "01"),
    labels = c("Non", "Oui")
  ),
  
  # Recodage de EA10e2a_02 : Auteur des sévices (Frères et sœurs)
  sevices_auteur_freres_soeurs <- factor(
    EA10e2a_02,
    levels = c("00", "01", "88", "99"),
    labels = c("Non", "Oui", "NVPD", "NSP")
  ),
  
  # Recodage de EA10e2a_03 : Auteur des sévices (Père)
  sevices_auteur_pere <- factor(
    EA10e2a_03,
    levels = c("00", "01", "88", "99"),
    labels = c("Non", "Oui", "NVPD", "NSP")
  ),
  
  # Recodage de EA10e2a_04 : Auteur des sévices (Mère)
  sevices_auteur_mere <- factor(
    EA10e2a_04,
    levels = c("00", "01", "88", "99"),
    labels = c("Non", "Oui", "NVPD", "NSP")
  ),
  
  # Recodage de EA10e2a_05 : Auteur des sévices (Autres personnes vivant avec vous)
  sevices_auteur_autres_famille <- factor(
    EA10e2a_05,
    levels = c("00", "01", "88", "99"),
    labels = c("Non", "Oui", "NVPD", "NSP")
  ),
  
  # Recodage de EA10e2a_06 : Auteur des sévices (Autres personnes)
  sevices_auteur_autres <- factor(
    EA10e2a_06,
    levels = c("00", "01", "88", "99"),
    labels = c("Non", "Oui", "NVPD", "NSP")
  ),
  
  # Recodage de EA10e2b_01 : Auteur des sévices sur frères et sœurs (Vous-même)
  sevices_frere_soeur_auteur_vousmeme <- factor(
    EA10e2b_01,
    levels = c("00", "01", "99"),
    labels = c("Non", "Oui", "NSP")
  ),
  
  # Recodage de EA10e2b_02 : Auteur des sévices sur frères et sœurs (Autres frères et sœurs)
  sevices_frere_soeur_auteur_frere_soeur <- factor(
    EA10e2b_02,
    levels = c("00", "01", "99"),
    labels = c("Non", "Oui", "NSP")
  ),
  
  # Recodage de EA10e2b_03 : Auteur des sévices sur frères et sœurs (Père)
  sevices_frere_soeur_auteur_pere <- factor(
    EA10e2b_03,
    levels = c("00", "01", "99"),
    labels = c("Non", "Oui", "NSP")
  ),
  
  # Recodage de EA10e2b_04 : Auteur des sévices sur frères et sœurs (Mère)
  sevices_frere_soeur_auteur_mere <- factor(
    EA10e2b_04,
    levels = c("00", "01", "99"),
    labels = c("Non", "Oui", "NSP")
  ),
  
  # Recodage de EA10e2b_05 : Auteur des sévices sur frères et sœurs (Autres personnes vivant avec vous)
  sevices_frere_soeur_auteur_autres_famille <- factor(
    EA10e2b_05,
    levels = c("00", "01", "99"),
    labels = c("Non", "Oui", "NSP")
  ),
  
  # Recodage de EA10e2b_06 : Auteur des sévices sur frères et sœurs (Autres personnes)
  sevices_frere_soeur_auteur_autres <- factor(
    EA10e2b_06,
    levels = c("00", "01", "99"),
    labels = c("Non", "Oui", "NSP")
  ),
  
  # Recodage de EA10e2c_01 : Auteur des sévices sur père (Vous-même)
  sevices_pere_auteur_vousmeme <- factor(
    EA10e2c_01,
    levels = c("00", "01", "88", "99"),
    labels = c("Non", "Oui", "NVPD", "NSP")
  ),
  
  # Recodage de EA10e2c_02 : Auteur des sévices sur père (Frères et sœurs)
  sevices_pere_auteur_freres_soeurs <- factor(
    EA10e2c_02,
    levels = c("00", "01", "88", "99"),
    labels = c("Non", "Oui", "NVPD", "NSP")
  ),
  
  # Recodage de EA10e2c_04 : Auteur des sévices sur père (Mère)
  sevices_pere_auteur_mere <- factor(
    EA10e2c_04,
    levels = c("00", "01", "88", "99"),
    labels = c("Non", "Oui", "NVPD", "NSP")
  ),
  
  # Recodage de EA10e2c_05 : Auteur des sévices sur père (Autres personnes vivant avec vous)
  sevices_pere_auteur_autres_famille <- factor(
    EA10e2c_05,
    levels = c("00", "01", "88", "99"),
    labels = c("Non", "Oui", "NVPD", "NSP")
  ),
  
  # Recodage de EA10e2c_06 : Auteur des sévices sur père (Autres personnes)
  sevices_pere_auteur_autres <- factor(
    EA10e2c_06,
    levels = c("00", "01", "88", "99"),
    labels = c("Non", "Oui", "NVPD", "NSP")
  ),
  
  # Recodage de EA10e2d_01 : Auteur des sévices sur mère (Vous-même)
  sevices_mere_auteur_vousmeme <- factor(
    EA10e2d_01,
    levels = c("00", "01", "88", "99"),
    labels = c("Non", "Oui", "NVPD", "NSP")
  ),
  
  # Recodage de EA10e2d_02 : Auteur des sévices sur mère (Frères et sœurs)
  sevices_mere_auteur_freres_soeurs <- factor(
    EA10e2d_02,
    levels = c("00", "01", "88", "99"),
    labels = c("Non", "Oui", "NVPD", "NSP")
  ),
  
  # Recodage de EA10e2d_03 : Auteur des sévices sur mère (Père)
  sevices_mere_auteur_pere <- factor(
    EA10e2d_03,
    levels = c("00", "01", "88", "99"),
    labels = c("Non", "Oui", "NVPD", "NSP")
  ),
  
  # Recodage de EA10e2d_05 : Auteur des sévices sur mère (Autres personnes vivant avec vous)
  sevices_mere_auteur_autres_famille <- factor(
    EA10e2d_05,
    levels = c("00", "01", "88", "99"),
    labels = c("Non", "Oui", "NVPD", "NSP")
  ),
  
  # Recodage de EA10e2d_06 : Auteur des sévices sur mère (Autres personnes)
  sevices_mere_auteur_autres <- factor(
    EA10e2d_06,
    levels = c("00", "01", "88", "99"),
    labels = c("Non", "Oui", "NVPD", "NSP")
  )
)
