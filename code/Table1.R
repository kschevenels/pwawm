# Load data

source(here::here("code","01_load_data.R"))

# Set theme

gtsummary::theme_gtsummary_compact()

# Create table

patientchars <- PWAWM %>% 
  dplyr::select(age, sex, handedness, education, 
                stroke_type, stroke_laterality, stroke_history, stroke_area,
                stroke_size, old_lesion_load, NIHSS_total, 
                ScreeLing_1_dpo, ScreeLing_2_dpo, ScreeLing_3_dpo) %>% 
  mutate_if(is.character, ~factor(.)) %>%
  gtsummary::tbl_summary(type = list(c(sex, handedness, stroke_type, stroke_laterality, stroke_history) ~ "dichotomous"),
                         value = list(sex ~ "female", handedness ~ "right-handed", stroke_type ~ "ischemia", stroke_laterality ~ "left", stroke_history ~ "yes"),
                         statistic = list(all_continuous() ~ "{mean} ({sd})_{median} ({min} - {max})",
                                          all_categorical() ~ "{n}"),
                         digits = list(c(age, education) ~ 1, c(NIHSS_total, ScreeLing_1_dpo, ScreeLing_2_dpo, ScreeLing_3_dpo) ~ 0, c(stroke_size, old_lesion_load) ~ 2), 
                         label  = list(age ~ "Age (years)",
                                       sex ~ "Sex (female/male)",
                                       handedness ~ "Handedness (right-handed/other)",
                                       education ~ "Education (years)",
                                       stroke_type ~ "Stroke type (ischemia/hemorrhage)",
                                       stroke_laterality ~ "Stroke laterality (left/bilateral)",
                                       stroke_history ~ "History of stroke (no/yes)",
                                       stroke_size ~ "Acute lesion volume (cc)", 
                                       old_lesion_load ~ "Old lesion load (cc)")) %>%
  modify_footnote(all_stat_cols() ~ "Descriptives printed for continuous variables: M (SD); Descriptives printed for categorical variables: n. Note: NIHSS = National Institutes of Health Stroke Scale (maximum score = 42, a higher score corresponds to a more severe stroke).") %>%
  modify_header(list(label ~ "**Variable**")) %>%
  bold_labels() %>%
  gtsummary::as_tibble() %>%
  dplyr::rename(Variable=1) %>%
  dplyr::rename(N=2) %>%
  separate(N, into=c("N","Median"), sep="_") %>% 
  filter(!Variable %in% "Unknown") %>%
  mutate(N = ifelse(Variable == "__Sex (female/male)__", paste0(N,"/",32-as.numeric(N)),N)) %>%
  mutate(N = ifelse(Variable == "__Handedness (right-handed/other)__", paste0(N,"/",32-as.numeric(N)),N)) %>%
  mutate(N = ifelse(Variable == "__Stroke type (ischemia/hemorrhage)__", paste0(N,"/",32-as.numeric(N)),N)) %>%
  mutate(N = ifelse(Variable == "__Stroke laterality (left/bilateral)__", paste0(N,"/",32-as.numeric(N)),N)) %>%
  mutate(N = ifelse(Variable == "__History of stroke (no/yes)__", paste0(32-as.numeric(N),"/",N),N)) %>%
  add_row(.after = 8, Variable = "ACM/ACP/Avert/Abas/multifocal", N = "22/5/1/1/3", Median = "") %>%
  filter(!Variable %in% c("arteria cerebri media","arteria cerebri posterior","arteria vertebralis",
                          "arteria basilaris","anterior choroidal artery","multifocal", "ABas")) %>%
  mutate(Unknown = as.factor(ifelse(Variable=="__Education (years)__",1, NA))) %>%
  dplyr::rename(`N = 32`=2) %>%
  dplyr::rename(`Median (Range)`=3) %>%
  dplyr::rename(`NA` = Unknown) %>%
  mutate(Variable = recode_factor(Variable, "__Education (years)__" = "Education (years)",
                                  "__Age (years)__" = "Age (years)", 
                                  "__stroke_area__" = "Affected circulation area",
                                  "__Sex (female/male)__" = "Sex (female/male)", 
                                  "__Handedness (right-handed/other)__" = "Handedness (right-handed/other)",
                                  "__Stroke type (ischemia/hemorrhage)__" = "Stroke type (ischemia/hemorrhage)", 
                                  "__Stroke laterality (left/bilateral)__" = "Stroke laterality (left/bilateral)", 
                                  "__Acute lesion volume (cc)__" = "Acute lesion volume (cm3)",
                                  "__Old lesion load (cc)__" = "Old lesion load (cm3)",
                                  "__History of stroke (no/yes)__" = "History of stroke (no/yes)",
                                  "__Acute NIHSS total score__" = "Acute NIHSS total score",
                                  "__Days post stroke (acute)__" = "Days post stroke (acute)",
                                  "__Days post stroke (subacute)__" = "Days post stroke (subacute)",
                                  "__Days post stroke (chronic)__" = "Days post stroke (chronic)"))

table_patientchars <- flextable(patientchars) %>% 
  autofit() %>%
  theme_booktabs(bold_header = T) %>%
  flextable::footnote(part="header",
                      i=1,j=2,
                      value=as_paragraph("N is reported for categorical variables; M (SD) is reported for continuous variables"),
                      ref_symbols = "a") %>%
  flextable::footnote(part="header",
                      i=1,j=4,
                      value=as_paragraph("N indicates the number of participants for which the corresponding data are missing"),
                      ref_symbols = "b") %>%
  flextable::footnote(part="body",
                      i=13:15,j=1,
                      value=as_paragraph("For behavioral language testing"),
                      ref_symbols = "c") %>%
  flextable::footnote(part="body",
                      i=2,j=2,
                      value=as_paragraph("Note. ACM = arteria cerebri media, ACP = arteria cerebri posterior, Avert = arteria vertebralis, Abas = arteria basilaris, NIHSS = National Institutes of Health Stroke Scale (a higher score corresponds to a more severe stroke)."),
                      ref_symbols = "") %>%
  align(i=1:15, j=2:4, align = "center", part="body") %>%
  align(j=2:4, align = "center", part="header") %>%
  set_table_properties(layout = "autofit") %>%
  compose(part="body", i=10, j=1, value=as_paragraph("Acute lesion volume (cm", as_sup("3"), ")")) %>%
  compose(part="body", i=11, j=1, value=as_paragraph("Old lesion load (cm", as_sup("3"), ")"))

table_patientchars
