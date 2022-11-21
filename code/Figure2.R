# Load data

source(here::here("code","01_load_data.R"))

# Create long dataset to plot

datafr <- PWAWM %>% 
  dplyr::select("patient", "ScreeLing_1_fon", "ScreeLing_1_sem", "ScreeLing_2_fon", "ScreeLing_2_sem", "ScreeLing_3_fon", "ScreeLing_3_sem") %>%
  pivot_longer(cols = !patient, 
               names_to = c("Assessment", "Time", "Component"), 
               names_pattern = "(.*)_(.)_(.*)", 
               values_to = "score") %>% 
  mutate_if(is.character, as.factor) %>%
  mutate(Component = recode_factor(Component, "fon" = "phonology", "sem" = "semantics")) 

# Plot language evolution over time

Individual_evolution_linechart <- datafr %>%
  filter(!(is.na(score))) %>%
  group_by(Time, Component) %>%
  summarize(mean_score = mean(score), sd_score = sd(score)) %>% 
  ggplot(mapping = aes(x = as.numeric(as.character(Time)), y = score, color = Component)) +
  geom_path(data = datafr, aes(group = patient), alpha = .2) + 
  geom_line(aes(y = mean_score, color = Component), size = 1.5, alpha = .8) +
  geom_point(aes(y = mean_score, color = Component), size = 3, alpha = .8) +
  geom_errorbar(aes(y = mean_score, ymin = mean_score - sd_score, ymax = mean_score + sd_score), width = .1, size = 1, alpha = .8) +
  geom_point(data = datafr, size = 1.5) + 
  theme_classic() + 
  labs(x = "Phase post stroke", y = "Language score (/24)") + 
  scale_colour_manual(values = c("#bdbdbd", "#636363")) +
  scale_x_discrete(limits = c("acute", "subacute", "chronic")) +
  facet_grid(. ~ Component) + 
  geom_hline(yintercept = 22, color = cbbPalette[1], size = 0.7, linetype="dashed") + 
  theme(legend.position="none") 

tiff(here("figs", "Figure2.tiff"), units="mm", width=180, height=150, res=600, compression="lzw")
Individual_evolution_linechart
dev.off()
  
