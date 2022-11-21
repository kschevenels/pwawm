# Load data

source(here::here("code","01_load_data.R"))
load(here::here("data","langrecov.RData"))
load(here::here("data","concur_acute.RData"))
load(here::here("data","concur_subacute.RData"))
load(here::here("data","lagged_subacute.RData"))
load(here::here("data","lagged_chronic.RData"))
load(here::here("data","pred_subacute.RData"))
load(here::here("data","pred_chronic.RData"))
load(here::here("data","change_time.RData"))
load(here::here("data","change_subacute.RData"))
load(here::here("data","change_chronic.RData"))
load(here::here("data","robust.RData"))

# Create long dataset of behavioral & brain data 

PWAWM_long_beh <- PWAWM %>% 
  dplyr::select("patient", "stroke_type", "stroke_size", "stroke_laterality", 
                "ScreeLing_1_fon", "ScreeLing_1_sem", 
                "ScreeLing_2_fon", "ScreeLing_2_sem", 
                "ScreeLing_3_fon", "ScreeLing_3_sem") %>%
  pivot_longer(cols = starts_with("ScreeLing"), 
               names_to = c("Assessment", "TimeBeh", "Component"), 
               names_pattern = "(.*)_(.)_(.*)", 
               values_to = "score") %>% 
  mutate_if(is.character, as.factor) %>%
  filter(!(is.na(score))) %>%
  mutate(Component = recode_factor(Component, "fon" = "phonology", "sem" = "semantics")) %>%
  mutate(TimeBeh = recode_factor(TimeBeh, "1" = "acute", "2" = "subacute", "3" = "chronic")) %>%
  group_by(TimeBeh, Component) %>% # scaling has to occur per time point and language component 
  mutate(score_z = scale(score)) %>%
  ungroup() 

PWAWM_long_FBC <- PWAWM %>%
  dplyr::select("patient", "stroke_type", "stroke_size", "stroke_laterality",  
                "FBC_acute_AF_L", "FBC_acute_AF_R", "FBC_acute_IFOF_L", "FBC_acute_IFOF_R", 
                "FBC_subacute_AF_L", "FBC_subacute_AF_R", "FBC_subacute_IFOF_L", "FBC_subacute_IFOF_R") %>%
  pivot_longer(cols = starts_with("FBC"), 
               names_to = c(".value", "TimeBrain", "Tract", "Laterality"),
               names_pattern = "(.*)_(.*)_(.*)_(.)") %>%
  mutate_if(is.character, as.factor) %>%
  filter(!(is.na(FBC))) %>%
  mutate(Laterality = recode_factor(Laterality, "L" = "left", "R" = "right")) %>%
  group_by(TimeBrain, Tract, Laterality) %>% # scaling has to occur per time point, tract and hemisphere
  mutate(FBC_z = scale(FBC)) %>%
  ungroup() 

PWAWM_long <- left_join(PWAWM_long_beh, PWAWM_long_FBC) 

# Plot concurrent results 

PWAWM_long_beh_acute <- PWAWM_long_beh %>% filter(TimeBeh == "acute")
PWAWM_long_FBC_acute <- PWAWM_long_FBC %>% filter(TimeBrain == "acute")
PWAWM_long_acute <- left_join(PWAWM_long_beh_acute, PWAWM_long_FBC_acute)

concur_text <- data.frame(
  label = c(paste0("Left: \u03b2 = ", numformat(concur_1_LAF_fon$estimate[2]), "*"), paste0("Right: \u03b2 = ", numformat(concur_1_RAF_fon$estimate[2])),
            paste0("Left: \u03b2 = ", numformat(concur_1_LAF_sem$estimate[2]), " "), paste0("Right: \u03b2 = ", numformat(concur_1_RAF_sem$estimate[2])), 
            paste0("Left: \u03b2 = ", numformat(concur_1_LIFOF_fon$estimate[2])), paste0("Right: \u03b2 = ", numformat(concur_1_RIFOF_fon$estimate[2])),
            paste0("Left: \u03b2 = ", numformat(concur_1_LIFOF_sem$estimate[2]), "*"), paste0("Right: \u03b2 = ", numformat(concur_1_RIFOF_sem$estimate[2]), "*")),
  Tract = c(rep("AF",4), rep("IFOF",4)),
  Component = rep(c("phonology", "phonology", "semantics", "semantics"),2),
  Laterality = rep(c("left", "right"),4),
  x = rep(c(-0.58, -0.47), 4),
  y = rep(c(2.7, 2.3),4)
)

concur_matrix <- PWAWM_long_acute %>%  
  ggplot(aes(x=FBC_z, y=score_z, color=Laterality, fill=Laterality)) +
  geom_point(size=2.5) +
  geom_smooth(method=rlm, se = T, alpha = 0.2) +
  scale_colour_manual(values = c("#bdbdbd", "black")) +
  scale_fill_manual(values = c("#bdbdbd", "black")) +
  guides(colour = "none", shape = "none", size = "none", fill = "none") +
  labs(x="Acute FBC (scaled)", y = "Acute language outcome (scaled)")+
  theme_classic() +
  facet_grid(Tract ~ Component) +
  theme(strip.text = element_text(size = 14), axis.text = element_text(size=14), axis.title = element_text(size=14)) +
  geom_text(
    data = concur_text, size = 5.5,
    mapping = aes(x = x, y = y, label = label)
  )

tiff(here("figs", "Figure3.tiff"), units="mm", width=250, height=250, res=600, compression="lzw")
concur_matrix
dev.off()

# Create dataset to plot change results

PWAWM$subacute_fon_resid<-resid(rlm(scale(ScreeLing_2_fon) ~ scale(ScreeLing_1_fon),data=PWAWM, na.action = na.exclude))
PWAWM$subacute_sem_resid<-resid(rlm(scale(ScreeLing_2_sem) ~ scale(ScreeLing_1_sem),data=PWAWM, na.action = na.exclude))

AF_delta <- PWAWM %>% 
  dplyr::select("patient", "stroke_type", "stroke_size", "stroke_laterality", 
                "subacute_fon_resid", "subacute_sem_resid", 
                "delta_FBC_AF_L", "delta_FBC_AF_R") %>%
  pivot_longer(cols = starts_with("delta"), 
               names_to = c("Laterality"), 
               names_pattern = "delta_FBC_AF_(.)", 
               values_to = "delta_FBC_AF") %>% 
  mutate_if(is.character, as.factor) %>%
  mutate(Laterality = recode_factor(Laterality, "L" = "left", "R" = "right")) %>% 
  pivot_longer(cols = ends_with("resid"),
               names_to = ("Component"),
               names_pattern = ("subacute_(...)_resid"),
               values_to = ("subacute_resid")) %>%
  mutate(Component = recode_factor(Component, "fon" = "phonology", "sem" = "semantics")) %>%
  group_by(Laterality) %>% # scaling has to occur per hemisphere
  mutate(delta_FBC_AF_z = scale(delta_FBC_AF)) %>%
  ungroup() 

# Plot change results 

change_text <- data.frame(
  label = c(paste0("Left: \u03b2 = ", numformat(cl_beh_deltaLAF_fon$estimate[2])),
            paste0("Right: \u03b2 = ", numformat(cl_beh_deltaRAF_fon$estimate[2]), " "),
            paste0("Left: \u03b2 = ", numformat(cl_beh_deltaLAF_sem$estimate[2])),
            paste0("Right: \u03b2 = ", numformat(cl_beh_deltaRAF_sem$estimate[2]))),
  Component = c("phonology", "phonology", "semantics", "semantics"),
  Laterality = rep(c("left", "right"),2),
  x = rep(c(-2.2, -2.08), 2),
  y = rep(c(-1.7, -2.05),2)
)

change_matrix <- AF_delta %>%  
  ggplot(aes(x=scale(delta_FBC_AF_z), y=subacute_resid, color=Laterality, fill=Laterality)) +
  geom_point(size=2.5) +
  geom_smooth(method=rlm, se = T, alpha = 0.2) +
  scale_colour_manual(values = c("#bdbdbd", "black")) +
  scale_fill_manual(values = c("#bdbdbd", "black")) +
  guides(colour = "none", shape = "none", size = "none", fill = "none") +
  labs(x="Early change in FBC of the AF (scaled)", y = "Subacute language outcome (corrected for AR effect)")+
  theme_classic() +
  facet_grid(~ Component) +
  theme(strip.text = element_text(size = 14), axis.text = element_text(size=14), axis.title = element_text(size=14)) +
  geom_text(
    data = change_text, size = 5.5,
    mapping = aes(x = x, y = y, label = label)
  )

tiff(here("figs", "Figure5.tiff"), units="mm", width=250, height=125, res=600, compression="lzw")
change_matrix
dev.off()

# Plot predictive results 

PWAWM_long_beh_subacute <- PWAWM_long_beh %>% filter(TimeBeh == "subacute")
PWAWM_long_FBC_acute <- PWAWM_long_FBC %>% filter(TimeBrain == "acute")
PWAWM_long_pred <- left_join(PWAWM_long_beh_subacute, PWAWM_long_FBC_acute)

pred_text <- data.frame(
  label = c(paste0("Left: \u03b2 = ", numformat(pred_beh_LAF_fon$estimate[2]), " "), paste0(" Right: \u03b2 = ", numformat(pred_beh_RAF_fon$estimate[2])),
            paste0("Left: \u03b2 = ", numformat(pred_beh_LAF_sem$estimate[2]), "  "), paste0("Right: \u03b2 = ", numformat(pred_beh_RAF_sem$estimate[2]), "*"), 
            paste0("Left: \u03b2 = ", numformat(pred_beh_LIFOF_fon$estimate[2])), paste0("Right: \u03b2 = ", numformat(pred_beh_RIFOF_fon$estimate[2])),
            paste0("Left: \u03b2 = ", numformat(pred_beh_LIFOF_sem$estimate[2]), "*"), paste0("Right: \u03b2 = ", numformat(pred_beh_RIFOF_sem$estimate[2]), " ")),
  Tract = c(rep("AF",4), rep("IFOF",4)),
  Component = rep(c("phonology", "phonology", "semantics", "semantics"),2),
  Laterality = rep(c("left", "right"),4),
  x = rep(c(-0.58, -0.5), 4),
  y = rep(c(-2.8, -3.3),4)
)

pred_matrix <- PWAWM_long_pred %>%  
  ggplot(aes(x=FBC_z, y=score_z, color=Laterality, fill=Laterality)) +
  geom_point(size=3) +
  geom_smooth(method=rlm, se = T, size = 1.5, alpha = 0.2) +
  scale_colour_manual(values = c("#bdbdbd", "black")) +
  scale_fill_manual(values = c("#bdbdbd", "black")) +
  guides(colour = "none", shape = "none", size = "none", fill = "none") +
  labs(x="Acute FBC (scaled)", y = "Subacute language outcome (scaled)")+
  theme_classic() +
  facet_grid(Tract ~ Component) +
  theme(strip.text = element_text(size = 14), axis.text = element_text(size=14), axis.title = element_text(size=14)) +
  geom_text(
    data = pred_text, size = 5.5,
    mapping = aes(x = x, y = y, label = label)
  )

tiff(here("figs", "Supplementary_Figure4.tiff"), units="mm", width=250, height=250, res=600, compression="lzw")
pred_matrix
dev.off()
