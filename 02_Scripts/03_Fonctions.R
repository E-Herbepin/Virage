















GraphExtInt <- function(res, xax = 1, yax = 2, var_sup = TRUE, ExtInt = "", var_sup_choice = NULL, 
                        var_lab_min_contrib = 0, point_size = 64,labels=NULL, labels_prepend_var = FALSE, 
                        col_var = NULL, symbol_variables = NULL, size_var = NULL, size_range = c(10,300), zoom_callback = NULL, in_explor = FALSE,col_lab=NULL, ...){
  res<-explor::prepare_results(res)
  
MCA_var_data <- function(res, xax = 1, yax = 2, var_sup = TRUE, var_sup_choice = NULL,
                         var_lab_min_contrib = 0, labels_prepend_var = FALSE) {
  
  tmp_x <- res$vars %>%
    arrange(Axis, Type, Variable) %>%
    filter(Axis == xax) %>%
    select("Variable", "Level", "Type", "Class", "Coord", "Contrib", "Cos2", "Count")
  tmp_y <- res$vars %>% 
    filter(Axis == yax) %>%
    select("Variable", "Level", "Type", "Class", "Coord", "Contrib", "Cos2", "Count")
  if (!(var_sup) || is.null(var_sup_choice)) {
    tmp_x <- tmp_x %>% filter(Type == 'Active')
    tmp_y <- tmp_y %>% filter(Type == 'Active')
  }
  if (var_sup && !is.null(var_sup_choice)) {
    tmp_x <- tmp_x %>% filter(Type == 'Active' | Variable %in% var_sup_choice)
    tmp_y <- tmp_y %>% filter(Type == 'Active' | Variable %in% var_sup_choice)
  }
  if (labels_prepend_var) {
    tmp_x$Level <- paste(tmp_x$Variable, "-", tmp_x$Level)
    tmp_y$Level <- paste(tmp_y$Variable, "-", tmp_y$Level)
  }
  tmp <- tmp_x %>%
    left_join(tmp_y, by = c("Variable", "Level", "Type", "Class", "Count")) %>%
    mutate(Contrib = Contrib.x + Contrib.y,
           Cos2 = Cos2.x + Cos2.y,
           tooltip = paste(paste0("<strong>", Level, "</strong><br />"),
                           paste0("<strong>",
                                  gettext("Variable", domain = "R-explor"),
                                  ":</strong> ", Variable, "<br />"),
                           paste0("<strong>Axis ",xax," :</strong> ", Coord.x, "<br />"),
                           paste0("<strong>Axis ", yax," :</strong> ", Coord.y, "<br />"),
                           ifelse(is.na(Cos2), "",
                                  paste0("<strong>",
                                         gettext("Squared cosinus", domain = "R-explor"),
                                         ":</strong> ", Cos2, "<br />")),
                           ifelse(is.na(Contrib), "",
                                  paste0("<strong>",
                                         gettext("Contribution:", domain = "R-explor"),
                                         "</strong> ", Contrib, "<br />")),
                           ifelse(is.na(Count), "",
                                  paste0("<strong>",
                                         gettext("Count:", domain = "R-explor"),
                                         "</strong> ", Count))),
           Lab = ifelse(Contrib >= as.numeric(var_lab_min_contrib) | 
                          (is.na(Contrib) & as.numeric(var_lab_min_contrib) == 0), Level, ""))
  data.frame(tmp)
}
  
html_id <- if (in_explor) 
    "explor_var"
  else NULL
  dom_id_svg_export <- if (in_explor) 
    "explor-var-svg-export"
  else NULL
  dom_id_lasso_toggle <- if (in_explor) 
    "explor-var-lasso-toggle"
  else NULL
  lasso <- if (in_explor) 
    TRUE
  else FALSE
  lasso_callback <- if (in_explor) 
    explor_multi_lasso_callback()
  else NULL
  zoom_callback <- if (in_explor) 
    explor_multi_zoom_callback(type = "var")
  else NULL
  

  var_data <- MCA_var_data(res, xax, yax, var_sup, var_sup_choice, 
                           var_lab_min_contrib, labels_prepend_var)
  if (!is.null(labels)) {
      var_data$Level[!(var_data$Variable%in%c(var_sup_choice))] <- labels
    }

  var_data$Colors <-ExtInt
  var_data$symbol_var <- symbol_variables

  if (!is.null(labels)) {
    for (i in 1:length(var_data$tooltip[!(var_data$Variable%in%c(Var_sup))])) {
      var_data$tooltip[!(var_data$Variable%in%c(Var_sup))][i] <- str_replace(var_data$tooltip[!(var_data$Variable%in%c(Var_sup))][i], var_data$Level[!(var_data$Variable%in%c(Var_sup))][i], labels[i])
    }
    var_data$Lab[!(var_data$Variable%in%c(Var_sup))] <- labels
    }
  
scatterD3::scatterD3(
  x = var_data[, "Coord.x"],
  y = var_data[, "Coord.y"],
  xlab = names(res$axes)[res$axes == xax],
  ylab = names(res$axes)[res$axes == yax],
  lab = var_data[,
                 "Lab"],
  point_size = point_size,
  point_opacity = 1,
  col_var = if (is.null(col_var))
    NULL
  else
    var_data[, col_var],
  col_lab = if (is.null(col_lab))
    col_var
  else
    col_lab,
  symbol_var = if (is.null(symbol_variables))
    NULL
  else
    var_data[, "symbol_var"],
  symbol_lab = symbol_variables,
  size_var = if (is.null(size_var))
    NULL
  else
    var_data[, size_var],
  size_lab = size_var,
  size_range = if (is.null(size_var))
    c(10, 300)
  else
    c(30, 400) * point_size / 32,
  tooltip_text = var_data[,
                          "tooltip"],
  type_var = ifelse(var_data[, "Class"] ==
                      "Quantitative", "arrow", "point"),
  unit_circle = var_sup &&
    "Quantitative" %in% var_data[, "Class"],
  key_var = paste(var_data[,
                           "Variable"], var_data[, "Level"], sep = "-"),
  fixed = TRUE,
  html_id = html_id,
  dom_id_svg_export = dom_id_svg_export,
  dom_id_lasso_toggle = dom_id_lasso_toggle,
  lasso = lasso,
  lasso_callback = lasso_callback,
  zoom_callback = zoom_callback
)
}

