# Caractérisation sociale ----------------------------

# Liste des variables :
#  c(
#   "sexe", "age", "situa", "TypeEmp", "TailleAglo", "Natio", "Statumigration", "Diplome", "CSP_3", "CSP_1", "Couple", "Etat_matrimonial", "typecouple", "habitecouple", "dureecouple", "dureecohab", "logement", "nb_personnes_logement", "nb_personnes_moins16", "revenus", "nb_enfants", "type_menage_5mod", "type_menage_9mod", "religion", "importance_religion", "religion_conjoint", "importance_religion_conjoint", "age_1_mariage"
#  )

tb_plat <- function(data, var, title = "Répartition", rm = c("NA", "NVPD", "NSP"), round = 1, gt = TRUE) {
  # Filtrer les données selon les critères spécifiés dans rm
  data_filtered <- data %>%
    filter(
      (!"NA" %in% rm | !is.na({{ var }})) &
        (!"NVPD" %in% rm | {{ var }} != "NVPD") &
        (!"NSP" %in% rm | {{ var }} != "NSP")
    )
  
  # Calculer les résumés
  tb_plat <- data_filtered %>%
    summarize(count = n(), .by = {{ var }}) %>%
    mutate(prop = round(100 * count / sum(count), round))
  
  # Retourner le tibble ou le tableau gt en fonction du paramètre gt
  if (gt) {
    tb_plat %>%
      gt() %>%
      tab_header(title = title) %>%
      cols_label(count = "Nombre de personnes", prop = "Proportion (%)") %>%
      cols_align(align = "center", columns = everything()) %>%
      cols_width(everything() ~ px(150))
  } else {
    return(tb_plat)
  }
}

bar_plat <- function(data, var, title = "Répartition",  rm = c("NA", "NVPD", "NSP"), round = 1) {
  data |>
    filter(
      (!"NA" %in% rm | !is.na({{ var }})) &
        (!"NVPD" %in% rm | {{ var }} != "NVPD") &
        (!"NSP" %in% rm | {{ var }} != "NSP")
    ) |> 
    summarize(count = n(),
              .by = {{ var }}) |>
    mutate(prop = round(100 * count / sum(count), round)) |>
    ggplot(aes(x = {{ var }}, y = prop)) +
    geom_bar(stat = "identity") +
    labs(title = title, x = deparse(substitute(var)), y = "Proportion") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

tb_croise <- function(data, var1, var2, title = "Tableau croisé", rm = c("NA", "NVPD", "NSP"), round = 1, gt=TRUE) {
  
  # Filtrer les données selon les critères spécifiés dans rm
  data_filtered <- data %>%
    filter(
      (!"NA" %in% rm | !is.na({{ var1 }})) & (!"NA" %in% rm | !is.na({{ var2 }})) &
        (!"NVPD" %in% rm | {{ var1 }} != "NVPD") & (!"NVPD" %in% rm | {{ var2 }} != "NVPD") &
        (!"NSP" %in% rm | {{ var1 }} != "NSP") & (!"NSP" %in% rm | {{ var2 }} != "NSP")
    )
  
  # Calculer le tableau croisé avec proportions
  tb_croise <- data_filtered %>%
    group_by({{ var1 }}, {{ var2 }}) %>%
    summarize(count = n(), .groups = 'drop_last') %>%
    mutate(prop = round(100 * count / sum(count), round))
  
  
  # Retourner le tibble ou le tableau gt en fonction du paramètre gt
  if (gt) {
    tb_croise %>%
      gt() %>%
      tab_header(title = title) %>%
      cols_label(
        count = "Nombre de personnes",
        prop = "Proportion (%)"
      ) %>%
      cols_align(align = "center", columns = everything()) %>%
      cols_width(everything() ~ px(150)) %>%
      opt_all_caps() # Pour mettre en majuscule les en-têtes
  } else {
    return(tb_croise)
  }
}

# Exemple d'utilisation
# tb_croise(DB, situa, sexe, title = "Mon tableau croisé", rm = c("NA", "NVPD"), gt=FALSE)
