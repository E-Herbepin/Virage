if (!file.exists("")) {
  data <- read.csv("01_Data/VIRAGE Principal/IE0245/Fichiers de données/CSV/base_virage.csv")
  DB <- data %>%
    select(
      c(
        poids_cal,
        Q1,
        Q19E_age,
        Q25E,
        EMP3,
        Q3,
        Q22E_01,
        Q22E_02,
        Q22E_03,
        Q22E_04,
        Mig_e,
        Q29E,
        CS_E_NIV3,
        CS_E_NIV1,
        Q16,
        Q6,
        Situmat,
        Q8,
        Dur_relconj,
        Dur_cohab,
        Q18a,
        Q5,
        Q5b,
        REV2,
        Enf1,
        Typmen_5mod,
        Typmen_9mod,
        REL1E,
        REL2E,
        REL1C,
        REL2C,
        VM1_age,
        Etatmat,
        EA9a,
        EA9b,
        EA9c,
        EA9d,
        EA9e,
        EA10d_00,
        EA10d_01,
        EA10d_02,
        EA10d_03,
        EA10d_04,
        EA10d_05,
        EA10e1_01,
        EA10e1_02,
        EA10e1_03,
        EA10e2a_02,
        EA10e2a_03,
        EA10e2a_04,
        EA10e2a_05,
        EA10e2a_06,
        EA10e2b_01,
        EA10e2b_02,
        EA10e2b_03,
        EA10e2b_04,
        EA10e2b_05,
        EA10e2b_06,
        EA10e2c_01,
        EA10e2c_02,
        EA10e2c_04,
        EA10e2c_05,
        EA10e2c_06,
        EA10e2d_01,
        EA10e2d_02,
        EA10e2d_03,
        EA10e2d_05,
        EA10e2d_06,
        CF6,
        CF7a,
        CF7b,
        CF7c,
        SOC1a,
        SOC2a,
        SOC2b,
        SOC2c,
        SOC2d,
        SOC5,
        SOC6,
        ends_with("S"),
        ends_with("P"),
        ends_with("Pa"),
        ends_with("Sa"),
        ends_with("Sc"),
        ends_with("Pc"),
        -EENFMINS,
        
        paste0("TE", c(1:4, 7:8, 10:11, 13, 15, 17:18)),
        paste0("T", c(1:5, 7:9, 11:12, 14:17)),
        paste0("P", c(1:11)),
        paste0("C", c(2:12, 15:16, 20:24, 26:28, 30:31)),
        paste0("C18", c("a", "b", "c", "d", "e", "f", "g")),
        paste0(
          "E3",
          c("a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n")
        ),
        paste0("F", c(1:3, 5:9)),
        paste0("V", c(1:9))
        
      )
    )
  write_rds(DB, "01_Data/VIRAGE Principal/Sous_base.rds")
} else {
  DB <- read.csv("01_Data/VIRAGE Principal/Sous_base.rds")
}
# Supprimer la sous-base :
# file.remove("01_Data/VIRAGE Principal/Sous_base.rds")