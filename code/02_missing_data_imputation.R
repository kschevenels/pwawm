# Load data

source(here::here("code","01_load_data.R"))

# Create subset of data for missing data imputation

PWAWM_miss <- PWAWM %>% dplyr::select(sex, age, education, 
                                      stroke_type, stroke_size, stroke_laterality, stroke_history, old_lesion_load, NIHSS_total, NIHSS_language, 
                                      ScreeLing_1_tot, ScreeLing_1_sem, ScreeLing_1_fon, ScreeLing_1_synt, ANTAT_1_A, 
                                      ScreeLing_2_tot, ScreeLing_2_sem, ScreeLing_2_fon, ScreeLing_2_synt, ANTAT_2_A, 
                                      ScreeLing_3_tot, ScreeLing_3_sem, ScreeLing_3_fon, ScreeLing_3_synt, ANTAT_3_A)

not <- names(PWAWM %>% dplyr::select(-patient, -sex, -age, -education, 
                                     -stroke_type, -stroke_size, -stroke_laterality, -stroke_history, -old_lesion_load, -NIHSS_total, -NIHSS_language, 
                                     -ScreeLing_1_sem, -ScreeLing_1_fon, -ScreeLing_1_synt, -ANTAT_1_A, 
                                     -ScreeLing_2_sem, -ScreeLing_2_fon, -ScreeLing_2_synt, -ANTAT_2_A, 
                                     -ScreeLing_3_sem, -ScreeLing_3_fon, -ScreeLing_3_synt, -ANTAT_3_A)) 

# Visualize missing data pattern

missing_pattern_plot <- aggr(PWAWM_miss, col = c('skyblue', 'red'),
                             numbers = TRUE, sortVars = TRUE, bars = TRUE, combined = TRUE,
                             labels = names(PWAWM_miss), cex.axis = .62, ylab = c("Missing data pattern"))

md.pattern(PWAWM_miss) #to display all different missing data patterns

# Summarize the missing data pattern

fluxplot(PWAWM_miss) #higher outflux = more powerful predictors <-> higher influx = depend strongly on the imputation model

# Create predictor matrix

ini <- mice(PWAWM, maxit=0, print=F) #to get initial predictor matrix
pred <- ini$pred #save as an object
pred[,c(not)] <- 0 #eliminate the influence of all variables in vector "not" in imputing missing values

# Create methods matrix and make sure that total scores are kept 

meth <- ini$method #save the imputation method per variable as an object
meth[c(not)] <- "" #to state that the variables in vector "not" should not be imputed
meth["ScreeLing_1_tot"]<- "~I(ScreeLing_1_fon + ScreeLing_1_sem + ScreeLing_1_synt)" # ! the tot variable has to come after the subscores in the dataset ! 
meth["ScreeLing_2_tot"]<- "~I(ScreeLing_2_fon + ScreeLing_2_sem + ScreeLing_2_synt)" # ! the tot variable has to come after the subscores in the dataset ! 
meth["ScreeLing_3_tot"]<- "~I(ScreeLing_3_fon + ScreeLing_3_sem + ScreeLing_3_synt)" # ! the tot variable has to come after the subscores in the dataset ! 
meth["ScreeLing_t2t1_fon"]<- "~I(ScreeLing_2_fon - ScreeLing_1_fon)"  
meth["ScreeLing_t2t1_sem"]<- "~I(ScreeLing_2_sem - ScreeLing_1_sem)" 
meth["ScreeLing_t2t1_synt"]<- "~I(ScreeLing_2_synt - ScreeLing_1_synt)" 
meth["ScreeLing_t2t1_tot"]<- "~I(ScreeLing_2_tot - ScreeLing_1_tot)"  
meth["ScreeLing_t3t1_fon"]<- "~I(ScreeLing_3_fon - ScreeLing_1_fon)"  
meth["ScreeLing_t3t1_sem"]<- "~I(ScreeLing_3_sem - ScreeLing_1_sem)" 
meth["ScreeLing_t3t1_synt"]<- "~I(ScreeLing_3_synt - ScreeLing_1_synt)" 
meth["ScreeLing_t3t1_tot"]<- "~I(ScreeLing_3_tot - ScreeLing_1_tot)"  
meth["ScreeLing_t3t2_fon"]<- "~I(ScreeLing_3_fon - ScreeLing_2_fon)"  
meth["ScreeLing_t3t2_sem"]<- "~I(ScreeLing_3_sem - ScreeLing_2_sem)" 
meth["ScreeLing_t3t2_synt"]<- "~I(ScreeLing_3_synt - ScreeLing_2_synt)" 
meth["ScreeLing_t3t2_tot"]<- "~I(ScreeLing_3_tot - ScreeLing_2_tot)" 
meth["ANTAT_t2t1_A"]<- "~I(ANTAT_2_A - ANTAT_1_A)" 
meth["ANTAT_t3t1_A"]<- "~I(ANTAT_3_A - ANTAT_1_A)" 
meth["ANTAT_t3t2_A"]<- "~I(ANTAT_3_A - ANTAT_2_A)" 

# Impute missing values 

imputed_MICE <- mice(PWAWM, m = 10, maxit = 50, method = meth, seed = 500, pred = pred) 
# imputed_MICE$loggedEvents #should be NULL (if not: remove 'out' variables from dataframe)
# summary(imputed_MICE)
# imputed_MICE$imp #to check imputed values 
# View(complete(imputed_MICE, 1, include = F) %>% select(patient,ScreeLing_1_fon, ScreeLing_2_fon, ScreeLing_t2t1_fon)) #to check whether relations are correct

# Inspect imputed values

# summary(PWAWM)
# summary(complete(imputed_MICE))
# plot(imputed_MICE) #to study convergence: we would like the streams to intermingle and be free of any trends at the later iterations
# stripplot(imputed_MICE, pch=20, cex=2) #if the data are MCAR, then the imputations should have the same distribution as the observed data
# densityplot(imputed_MICE, ~ ANTAT_3_A | .imp, lwd = 3) #to compare distributions

# Get long dataframe behavioral data

all_dats <- complete(imputed_MICE, action = "long", include = TRUE)
working_beh <- list()
for(i in 0:max(all_dats$.imp)) {
  working_beh[[i+1]] <- 
    all_dats %>%
    subset(.imp == i) %>%
    pivot_longer(cols = c("ScreeLing_1_fon", "ScreeLing_1_sem", "ScreeLing_2_sem", "ScreeLing_2_fon", "ScreeLing_3_sem", "ScreeLing_3_fon"),
                 names_to = c("Assessment", "Time", "Component"), 
                 names_pattern = "(.*)_(.)_(.*)", 
                 values_to = "score") %>%
    mutate(Time = recode_factor(Time, "1" = "acute", "2" = "subacute", "3" = "chronic")) %>%
    select(.imp, .id, patient, Assessment, Time, Component, score) %>%
    mutate(.id = 1:nrow(.))
}
behav_long <- as.mids(do.call(rbind, working_beh))
#view(complete(behav_long, 1, include = F))

# Get long dataframe brain data

working_brain <- list()
for(i in 0:max(all_dats$.imp)) {
  working_brain[[i+1]] <- 
    all_dats %>%
    subset(.imp == i) %>%
    pivot_longer(cols = c("FBC_acute_AF_L", "FBC_acute_AF_R", "FBC_acute_IFOF_L", "FBC_acute_IFOF_R", "FBC_subacute_AF_L", "FBC_subacute_AF_R", "FBC_subacute_IFOF_L", "FBC_subacute_IFOF_R"),
                 names_to = c(".value", "Time", "Tract", "Laterality"), 
                 names_pattern = "(.*)_(.*)_(.*)_(.)") %>%
    select(.imp, .id, patient, Time, Tract, Laterality, FBC) %>%
    mutate(.id = 1:nrow(.))
}
brain_long <- as.mids(do.call(rbind, working_brain))
#view(complete(brain_long, 1, include = F))

save(imputed_MICE, behav_long, brain_long, file = here("data", "imputed_data.RData"))
