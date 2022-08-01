# Load data

source(here::here("code","01_load_data.R"))

# Check assumptions

shapiro.test(PWAWM$ScreeLing_1_tot) 
shapiro.test(PWAWM$ScreeLing_1_fon) 
shapiro.test(PWAWM$ScreeLing_1_sem) 
shapiro.test(PWAWM$ScreeLing_2_tot) 
shapiro.test(PWAWM$ScreeLing_2_fon) 
shapiro.test(PWAWM$ScreeLing_2_sem) 
shapiro.test(PWAWM$ScreeLing_3_tot) 
shapiro.test(PWAWM$ScreeLing_3_fon) 
shapiro.test(PWAWM$ScreeLing_3_sem) 

shapiro.test(PWAWM$stroke_size) 
shapiro.test(PWAWM$old_lesion_load) 
shapiro.test(PWAWM$age) 
shapiro.test(PWAWM$education) 
shapiro.test(PWAWM$NIHSS_total)
shapiro.test(PWAWM$NIHSS_language) 
shapiro.test(PWAWM$eTIV_acute) 
shapiro.test(PWAWM$eTIV_subacute) 

shapiro.test(PWAWM$FBC_acute_AF_L) 
shapiro.test(PWAWM$FBC_acute_AF_R) 
shapiro.test(PWAWM$FBC_acute_IFOF_L) 
shapiro.test(PWAWM$FBC_acute_IFOF_R) 
shapiro.test(PWAWM$FBC_subacute_AF_L)
shapiro.test(PWAWM$FBC_subacute_AF_R)
shapiro.test(PWAWM$FBC_subacute_IFOF_L) 
shapiro.test(PWAWM$FBC_subacute_IFOF_R) 
shapiro.test(PWAWM$delta_FBC_AF_L)
shapiro.test(PWAWM$delta_FBC_AF_R)
shapiro.test(PWAWM$delta_FBC_IFOF_L) 
shapiro.test(PWAWM$delta_FBC_IFOF_R) 

# Plot big correlation matrix

allcor <- ggcorrmat(
  data = PWAWM,
  cor.vars = c("ScreeLing_1_fon", "ScreeLing_1_sem", "ScreeLing_2_fon", "ScreeLing_2_sem", "ScreeLing_3_fon", "ScreeLing_3_sem", "FBC_acute_AF_L", "FBC_acute_AF_R", "FBC_acute_IFOF_L", "FBC_acute_IFOF_R", "FBC_subacute_AF_L", "FBC_subacute_AF_R", "FBC_subacute_IFOF_L", "FBC_subacute_IFOF_R", "delta_FBC_AF_L", "delta_FBC_AF_R", "delta_FBC_IFOF_L", "delta_FBC_IFOF_R", "stroke_size", "old_lesion_load", "age"), 
  cor.vars.names = c("Phonology (acute)", "Semantics (acute)", "Phonology (subacute)", "Semantics (subacute)", "Phonology (chronic)", "Semantics (chronic)", "FBC left AF (acute)", "FBC right AF (acute)", "FBC left IFOF (acute)", "FBC right IFOF (acute)", "FBC left AF (subacute)", "FBC right AF (subacute)", "FBC left IFOF (subacute)", "FBC right IFOF (subacute)", "Delta FBC left AF", "Delta FBC right AF", "Delta FBC left IFOF", "Delta FBC right IFOF", "Acute lesion volume", "Old lesion load", "Age"),
  type = "robust",
  ggtheme = ggplot2::theme_light(), 
  partial = F, 
  p.adjust.method = "none",
  sig.level = 0.01,
  caption = "For description of the variables, we refer to the main text of the study."
)

ggsave(filename=here("figs","Supplementary_Figure3.pdf"), allcor, width=11, height=8.5, dpi=600)
