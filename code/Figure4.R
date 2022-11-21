# Load data

source(here::here("code","01_load_data.R"))

# Create long dataset to plot

FBC_over_time_long <- PWAWM %>% 
  dplyr::select("patient", "FBC_acute_AF_L", "FBC_acute_AF_R", "FBC_acute_IFOF_L", "FBC_acute_IFOF_R", 
                "FBC_subacute_AF_L", "FBC_subacute_AF_R", "FBC_subacute_IFOF_L", "FBC_subacute_IFOF_R") %>%
  pivot_longer(cols = !patient, 
               names_to = c(".value", "Time", "Tract", "Laterality"),
               names_pattern = "(.*)_(.*)_(.*)_(.)") %>%
  mutate_if(is.character, as.factor) %>%
  filter(!(patient %in% c("S0253", "S0380", "S0639", "S0668", "S0683"))) %>% #no follow-up scan
  mutate(Laterality = recode_factor(Laterality, "L" = "Left", "R" = "Right"), 
         Time = recode_factor(Time, "acute" = "Acute", "subacute" = "Subacute")) 

# Create plot 

FBC_evolution_boxplot_withsig <- ggplot(FBC_over_time_long, aes(Laterality, FBC)) +
  geom_boxplot(aes(color = Time))+
  geom_point(aes(fill=NULL, color = Time), position = position_jitterdodge(), size = 2) +
  facet_wrap(~Tract) +
  scale_colour_manual(values = c("#bdbdbd", "#636363")) +
  stat_compare_means(aes(group = Time), label = "p.signif", paired = T) + 
  theme_classic() +
  labs(y = "FBC (a.u.)") 

tiff(here("figs", "Figure4.tiff"), units="mm", width=150, height=150, res=600, compression="lzw")
FBC_evolution_boxplot_withsig
dev.off()
