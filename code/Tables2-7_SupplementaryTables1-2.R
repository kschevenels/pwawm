# Load data

load(here::here("data", "models.Rdata"))

# Create fixed variables

tract <- c(rep("AF", 4), rep("IFOF", 4))
laterality <- c("Left", "Left", "Right", "Right", "Left", "Left", "Right", "Right")
component <- rep(c("phonology", "semantics"),4)

# Create data frame concur acute

dataf_concur_acute <- lapply(concur_acute, function(model){
  beta <- model$estimate[2]
  sd <- model$std.error[2]
  statistic <- model$statistic[2]
  df <- model$df[2]
  p.value <- model$p.value[2]
  data.frame(beta, sd, statistic, df, p.value)
})

dataf_concur_acute <- data.frame(do.call(rbind, dataf_concur_acute)) %>% cbind(tract, laterality, component) %>%
  dplyr::select(tract, laterality, component, beta, sd, p.value) %>%
  pivot_wider(names_from = component, values_from = c(beta, sd, p.value)) %>%
  dplyr::select(tract, laterality, beta_phonology, sd_phonology, p.value_phonology, beta_semantics, sd_semantics, p.value_semantics) %>%
  mutate_at(vars(contains('p.value')), printp) %>%
  mutate_at(vars(contains('sd')), printnum) %>%
  mutate_at(vars(contains('beta')), numformat) 

table2 <- flextable(dataf_concur_acute) %>% 
  merge_v(j = c("tract", "laterality")) %>% 
  theme_vanilla %>% 
  set_header_labels(tract = "", laterality = "") %>%
  mk_par(j = 3, part = "header", value = as_paragraph(as_i(("β")))) %>% 
  mk_par(j = 4, part = "header", value = as_paragraph(as_i(("SE")))) %>% 
  mk_par(j = 5, part = "header", value = as_paragraph(as_i(("p")))) %>% 
  mk_par(j = 6, part = "header", value = as_paragraph(as_i(("β")))) %>% 
  mk_par(j = 7, part = "header", value = as_paragraph(as_i(("SE")))) %>% 
  mk_par(j = 8, part = "header", value = as_paragraph(as_i(("p")))) %>% 
  add_header_row(colwidths = c(2,3,3), values = c("", "Phonology", "Semantics")) %>%
  bold(part = "header") %>%
  align(align = "left", part = "all") 

table2

# Create data frame concur subacute

dataf_concur_subacute <- lapply(concur_subacute, function(model){
  beta <- model$estimate[2]
  sd <- model$std.error[2]
  statistic <- model$statistic[2]
  df <- model$df[2]
  p.value <- model$p.value[2]
  data.frame(beta, sd, statistic, df, p.value)
})

dataf_concur_subacute <- data.frame(do.call(rbind, dataf_concur_subacute)) %>% cbind(tract, laterality, component) %>%
  dplyr::select(tract, laterality, component, beta, sd, p.value) %>%
  pivot_wider(names_from = component, values_from = c(beta, sd, p.value)) %>%
  dplyr::select(tract, laterality, beta_phonology, sd_phonology, p.value_phonology, beta_semantics, sd_semantics, p.value_semantics) %>%
  mutate_at(vars(contains('p.value')), printp) %>%
  mutate_at(vars(contains('sd')), printnum) %>%
  mutate_at(vars(contains('beta')), numformat) 

table3 <- flextable(dataf_concur_subacute) %>% 
  merge_v(j = c("tract", "laterality")) %>% 
  theme_vanilla %>% 
  set_header_labels(tract = "", laterality = "") %>%
  mk_par(j = 3, part = "header", value = as_paragraph(as_i(("β")))) %>% 
  mk_par(j = 4, part = "header", value = as_paragraph(as_i(("SE")))) %>% 
  mk_par(j = 5, part = "header", value = as_paragraph(as_i(("p")))) %>% 
  mk_par(j = 6, part = "header", value = as_paragraph(as_i(("β")))) %>% 
  mk_par(j = 7, part = "header", value = as_paragraph(as_i(("SE")))) %>% 
  mk_par(j = 8, part = "header", value = as_paragraph(as_i(("p")))) %>% 
  add_header_row(colwidths = c(2,3,3), values = c("", "Phonology", "Semantics")) %>%
  bold(part = "header") %>%
  align(align = "left", part = "all") 

table3

# Create data frame change subacute

dataf_change_subacute <- lapply(change_subacute, function(model){
  beta <- model$estimate[2]
  sd <- model$std.error[2]
  statistic <- model$statistic[2]
  df <- model$df[2]
  p.value <- model$p.value[2]
  data.frame(beta, sd, statistic, df, p.value)
})

dataf_change_subacute <- data.frame(do.call(rbind, dataf_change_subacute)) %>% cbind(tract, laterality, component) %>%
  dplyr::select(tract, laterality, component, beta, sd, p.value) %>%
  pivot_wider(names_from = component, values_from = c(beta, sd, p.value)) %>%
  dplyr::select(tract, laterality, beta_phonology, sd_phonology, p.value_phonology, beta_semantics, sd_semantics, p.value_semantics) %>%
  mutate_at(vars(contains('p.value')), printp) %>%
  mutate_at(vars(contains('sd')), printnum) %>%
  mutate_at(vars(contains('beta')), numformat) %>% slice(1:2)

table4 <- flextable(dataf_change_subacute) %>% 
  merge_v(j = c("tract", "laterality")) %>% 
  theme_vanilla %>% 
  set_header_labels(tract = "", laterality = "") %>%
  mk_par(j = 3, part = "header", value = as_paragraph(as_i(("β")))) %>% 
  mk_par(j = 4, part = "header", value = as_paragraph(as_i(("SE")))) %>% 
  mk_par(j = 5, part = "header", value = as_paragraph(as_i(("p")))) %>% 
  mk_par(j = 6, part = "header", value = as_paragraph(as_i(("β")))) %>% 
  mk_par(j = 7, part = "header", value = as_paragraph(as_i(("SE")))) %>% 
  mk_par(j = 8, part = "header", value = as_paragraph(as_i(("p")))) %>% 
  add_header_row(colwidths = c(2,3,3), values = c("", "Phonology", "Semantics")) %>%
  bold(part = "header") %>%
  align(align = "left", part = "all") 

table4

# Create data frame change chronic

dataf_change_chronic <- lapply(change_chronic, function(model){
  beta <- model$estimate[2]
  sd <- model$std.error[2]
  statistic <- model$statistic[2]
  df <- model$df[2]
  p.value <- model$p.value[2]
  data.frame(beta, sd, statistic, df, p.value)
})

dataf_change_chronic <- data.frame(do.call(rbind, dataf_change_chronic)) %>% cbind(tract, laterality, component) %>%
  dplyr::select(tract, laterality, component, beta, sd, p.value) %>%
  pivot_wider(names_from = component, values_from = c(beta, sd, p.value)) %>%
  dplyr::select(tract, laterality, beta_phonology, sd_phonology, p.value_phonology, beta_semantics, sd_semantics, p.value_semantics) %>%
  mutate_at(vars(contains('p.value')), printp) %>%
  mutate_at(vars(contains('sd')), printnum) %>%
  mutate_at(vars(contains('beta')), numformat) %>% slice(1:2)

table5 <- flextable(dataf_change_chronic) %>% 
  merge_v(j = c("tract", "laterality")) %>% 
  theme_vanilla %>% 
  set_header_labels(tract = "", laterality = "") %>%
  mk_par(j = 3, part = "header", value = as_paragraph(as_i(("β")))) %>% 
  mk_par(j = 4, part = "header", value = as_paragraph(as_i(("SE")))) %>% 
  mk_par(j = 5, part = "header", value = as_paragraph(as_i(("p")))) %>% 
  mk_par(j = 6, part = "header", value = as_paragraph(as_i(("β")))) %>% 
  mk_par(j = 7, part = "header", value = as_paragraph(as_i(("SE")))) %>% 
  mk_par(j = 8, part = "header", value = as_paragraph(as_i(("p")))) %>% 
  add_header_row(colwidths = c(2,3,3), values = c("", "Phonology", "Semantics")) %>%
  bold(part = "header") %>%
  align(align = "left", part = "all") 

table5

# Create data frame lagged subacute

dataf_lagged_subacute <- lapply(lagged_subacute, function(model){
  beta <- model$estimate[2]
  sd <- model$std.error[2]
  statistic <- model$statistic[2]
  df <- model$df[2]
  p.value <- model$p.value[2]
  data.frame(beta, sd, statistic, df, p.value)
})

dataf_lagged_subacute <- data.frame(do.call(rbind, dataf_lagged_subacute)) %>% cbind(tract, laterality, component) %>%
  dplyr::select(tract, laterality, component, beta, sd, p.value) %>%
  pivot_wider(names_from = component, values_from = c(beta, sd, p.value)) %>%
  dplyr::select(tract, laterality, beta_phonology, sd_phonology, p.value_phonology, beta_semantics, sd_semantics, p.value_semantics) %>%
  mutate_at(vars(contains('p.value')), printp) %>%
  mutate_at(vars(contains('sd')), printnum) %>%
  mutate_at(vars(contains('beta')), numformat) 

table6 <- flextable(dataf_lagged_subacute) %>% 
  merge_v(j = c("tract", "laterality")) %>% 
  theme_vanilla %>% 
  set_header_labels(tract = "", laterality = "") %>%
  mk_par(j = 3, part = "header", value = as_paragraph(as_i(("β")))) %>% 
  mk_par(j = 4, part = "header", value = as_paragraph(as_i(("SE")))) %>% 
  mk_par(j = 5, part = "header", value = as_paragraph(as_i(("p")))) %>% 
  mk_par(j = 6, part = "header", value = as_paragraph(as_i(("β")))) %>% 
  mk_par(j = 7, part = "header", value = as_paragraph(as_i(("SE")))) %>% 
  mk_par(j = 8, part = "header", value = as_paragraph(as_i(("p")))) %>% 
  add_header_row(colwidths = c(2,3,3), values = c("", "Phonology", "Semantics")) %>%
  bold(part = "header") %>%
  align(align = "left", part = "all") 

table6

# Create data frame lagged chronic

dataf_lagged_chronic <- lapply(lagged_chronic, function(model){
  beta <- model$estimate[2]
  sd <- model$std.error[2]
  statistic <- model$statistic[2]
  df <- model$df[2]
  p.value <- model$p.value[2]
  data.frame(beta, sd, statistic, df, p.value)
})

dataf_lagged_chronic <- data.frame(do.call(rbind, dataf_lagged_chronic)) %>% cbind(tract, laterality, component) %>%
  dplyr::select(tract, laterality, component, beta, sd, p.value) %>%
  pivot_wider(names_from = component, values_from = c(beta, sd, p.value)) %>%
  dplyr::select(tract, laterality, beta_phonology, sd_phonology, p.value_phonology, beta_semantics, sd_semantics, p.value_semantics) %>%
  mutate_at(vars(contains('p.value')), printp) %>%
  mutate_at(vars(contains('sd')), printnum) %>%
  mutate_at(vars(contains('beta')), numformat) 

table7 <- flextable(dataf_lagged_chronic) %>% 
  merge_v(j = c("tract", "laterality")) %>% 
  theme_vanilla %>% 
  set_header_labels(tract = "", laterality = "") %>%
  mk_par(j = 3, part = "header", value = as_paragraph(as_i(("β")))) %>% 
  mk_par(j = 4, part = "header", value = as_paragraph(as_i(("SE")))) %>% 
  mk_par(j = 5, part = "header", value = as_paragraph(as_i(("p")))) %>% 
  mk_par(j = 6, part = "header", value = as_paragraph(as_i(("β")))) %>% 
  mk_par(j = 7, part = "header", value = as_paragraph(as_i(("SE")))) %>% 
  mk_par(j = 8, part = "header", value = as_paragraph(as_i(("p")))) %>% 
  add_header_row(colwidths = c(2,3,3), values = c("", "Phonology", "Semantics")) %>%
  bold(part = "header") %>%
  align(align = "left", part = "all")

table7

# Create data frame predictive subacute

dataf_pred_subacute <- lapply(pred_subacute, function(model){
  beta <- model$estimate[2]
  sd <- model$std.error[2]
  statistic <- model$statistic[2]
  df <- model$df[2]
  p.value <- model$p.value[2]
  data.frame(beta, sd, statistic, df, p.value)
})

dataf_pred_subacute <- data.frame(do.call(rbind, dataf_pred_subacute)) %>% cbind(tract, laterality, component) %>%
  dplyr::select(tract, laterality, component, beta, sd, p.value) %>%
  pivot_wider(names_from = component, values_from = c(beta, sd, p.value)) %>%
  dplyr::select(tract, laterality, beta_phonology, sd_phonology, p.value_phonology, beta_semantics, sd_semantics, p.value_semantics) %>%
  mutate_at(vars(contains('p.value')), printp) %>%
  mutate_at(vars(contains('sd')), printnum) %>%
  mutate_at(vars(contains('beta')), numformat) 

SItable1 <- flextable(dataf_pred_subacute) %>% 
  merge_v(j = c("tract", "laterality")) %>% 
  theme_vanilla %>% 
  set_header_labels(tract = "", laterality = "") %>%
  mk_par(j = 3, part = "header", value = as_paragraph(as_i(("β")))) %>% 
  mk_par(j = 4, part = "header", value = as_paragraph(as_i(("SE")))) %>% 
  mk_par(j = 5, part = "header", value = as_paragraph(as_i(("p")))) %>% 
  mk_par(j = 6, part = "header", value = as_paragraph(as_i(("β")))) %>% 
  mk_par(j = 7, part = "header", value = as_paragraph(as_i(("SE")))) %>% 
  mk_par(j = 8, part = "header", value = as_paragraph(as_i(("p")))) %>% 
  add_header_row(colwidths = c(2,3,3), values = c("", "Phonology", "Semantics")) %>%
  bold(part = "header") %>%
  align(align = "left", part = "all")

SItable1

# Create data frame predictive chronic

dataf_pred_chronic <- lapply(pred_chronic, function(model){
  beta <- model$estimate[2]
  sd <- model$std.error[2]
  statistic <- model$statistic[2]
  df <- model$df[2]
  p.value <- model$p.value[2]
  data.frame(beta, sd, statistic, df, p.value)
})

dataf_pred_chronic <- data.frame(do.call(rbind, dataf_pred_chronic)) %>% cbind(tract, laterality, component) %>%
  dplyr::select(tract, laterality, component, beta, sd, p.value) %>%
  pivot_wider(names_from = component, values_from = c(beta, sd, p.value)) %>%
  dplyr::select(tract, laterality, beta_phonology, sd_phonology, p.value_phonology, beta_semantics, sd_semantics, p.value_semantics) %>%
  mutate_at(vars(contains('p.value')), printp) %>%
  mutate_at(vars(contains('sd')), printnum) %>%
  mutate_at(vars(contains('beta')), numformat)

SItable2 <- flextable(dataf_pred_chronic) %>% 
  merge_v(j = c("tract", "laterality")) %>% 
  theme_vanilla %>% 
  set_header_labels(tract = "", laterality = "") %>%
  mk_par(j = 3, part = "header", value = as_paragraph(as_i(("β")))) %>% 
  mk_par(j = 4, part = "header", value = as_paragraph(as_i(("SE")))) %>% 
  mk_par(j = 5, part = "header", value = as_paragraph(as_i(("p")))) %>% 
  mk_par(j = 6, part = "header", value = as_paragraph(as_i(("β")))) %>% 
  mk_par(j = 7, part = "header", value = as_paragraph(as_i(("SE")))) %>% 
  mk_par(j = 8, part = "header", value = as_paragraph(as_i(("p")))) %>% 
  add_header_row(colwidths = c(2,3,3), values = c("", "Phonology", "Semantics")) %>%
  bold(part = "header") %>%
  align(align = "left", part = "all")

SItable2
