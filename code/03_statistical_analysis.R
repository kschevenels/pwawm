# Load data

source(here::here("code", "01_load_data.R"))
load(here::here("data", "imputed_data.RData"))

# Language recovery over time

recov <- joint_tests(with(data=behav_long, exp = lmer(score ~ Time*Component + (1|patient))))

time <- as.data.frame(emmeans(with(data=behav_long, exp = lmer(score ~ Time*Component + (1|patient))),
                              specs = pairwise ~ Time, type = "response")$contrasts %>% summary(infer=T))

comp <- as.data.frame(emmeans(with(data=behav_long, exp = lmer(score ~ Time*Component + (1|patient))),
                specs = pairwise ~ Component, type = "response")$contrasts %>% summary(infer=T))

save(recov, time, comp, file = here("data", "langrecov.RData"))

# Concurrent regressions

## Timepoint 1 

concur_1_LAF_fon <- summary(pool(with(data=imputed_MICE, exp = rlm(scale(ScreeLing_1_fon) ~ scale(FBC_acute_AF_L), maxit=50)))) 
concur_1_LAF_sem <- summary(pool(with(data=imputed_MICE, exp = rlm(scale(ScreeLing_1_sem) ~ scale(FBC_acute_AF_L), maxit=50))))
concur_1_RAF_fon <- summary(pool(with(data=imputed_MICE, exp = rlm(scale(ScreeLing_1_fon) ~ scale(FBC_acute_AF_R), maxit=50))))
concur_1_RAF_sem <- summary(pool(with(data=imputed_MICE, exp = rlm(scale(ScreeLing_1_sem) ~ scale(FBC_acute_AF_R), maxit=50))))
concur_1_LIFOF_fon <- summary(pool(with(data=imputed_MICE, exp = rlm(scale(ScreeLing_1_fon) ~ scale(FBC_acute_IFOF_L), maxit=50)))) 
concur_1_LIFOF_sem <- summary(pool(with(data=imputed_MICE, exp = rlm(scale(ScreeLing_1_sem) ~ scale(FBC_acute_IFOF_L), maxit=50)))) 
concur_1_RIFOF_fon <- summary(pool(with(data=imputed_MICE, exp = rlm(scale(ScreeLing_1_fon) ~ scale(FBC_acute_IFOF_R), maxit=50)))) 
concur_1_RIFOF_sem <- summary(pool(with(data=imputed_MICE, exp = rlm(scale(ScreeLing_1_sem) ~ scale(FBC_acute_IFOF_R), maxit=50)))) 

concur_acute = list(concur_1_LAF_fon, concur_1_LAF_sem, concur_1_RAF_fon, concur_1_RAF_sem, 
                    concur_1_LIFOF_fon, concur_1_LIFOF_sem, concur_1_RIFOF_fon, concur_1_RIFOF_sem)

save(concur_1_LAF_fon, concur_1_LAF_sem, concur_1_RAF_fon, concur_1_RAF_sem, concur_1_LIFOF_fon, concur_1_LIFOF_sem, concur_1_RIFOF_fon, concur_1_RIFOF_sem, file = here("data", "concur_acute.RData"))

interactionAF <- joint_tests(with(data=AF_long, exp = lmer(score ~ FBC*Component + (1|patient))))
emt_intAF <- emtrends(with(data=AF_long, exp = lmer(score ~ FBC*Component + (1|patient))), "Component", var = "FBC")
pairs_intAF <- as.data.frame(pairs(emt_intAF) %>% summary(infer = T))

interactionLIFOF <- joint_tests(with(data=IFOF_L_long, exp = lmer(score ~ FBC*Component + (1|patient))))
emt_intLIFOF <- emtrends(with(data=IFOF_L_long, exp = lmer(score ~ FBC*Component + (1|patient))), "Component", var = "FBC")
pairs_intLIFOF <- as.data.frame(pairs(emt_intLIFOF) %>% summary(infer = T))

interactionRIFOF <- joint_tests(with(data=IFOF_R_long, exp = lmer(score ~ FBC*Component + (1|patient))))
emt_intRIFOF <- emtrends(with(data=IFOF_R_long, exp = lmer(score ~ FBC*Component + (1|patient))), "Component", var = "FBC")
pairs_intRIFOF <- as.data.frame(pairs(emt_intRIFOF) %>% summary(infer = T))

## Timepoint 2 

concur_2_LAF_fon <- summary(pool(with(data=imputed_MICE, exp = rlm(scale(ScreeLing_2_fon) ~ scale(FBC_subacute_AF_L), maxit=50)))) 
concur_2_LAF_sem <- summary(pool(with(data=imputed_MICE, exp = rlm(scale(ScreeLing_2_sem) ~ scale(FBC_subacute_AF_L), maxit=50))))
concur_2_RAF_fon <- summary(pool(with(data=imputed_MICE, exp = rlm(scale(ScreeLing_2_fon) ~ scale(FBC_subacute_AF_R), maxit=50))))
concur_2_RAF_sem <- summary(pool(with(data=imputed_MICE, exp = rlm(scale(ScreeLing_2_sem) ~ scale(FBC_subacute_AF_R), maxit=50))))
concur_2_LIFOF_fon <- summary(pool(with(data=imputed_MICE, exp = rlm(scale(ScreeLing_2_fon) ~ scale(FBC_subacute_IFOF_L), maxit=50)))) 
concur_2_LIFOF_sem <- summary(pool(with(data=imputed_MICE, exp = rlm(scale(ScreeLing_2_sem) ~ scale(FBC_subacute_IFOF_L), maxit=50)))) 
concur_2_RIFOF_fon <- summary(pool(with(data=imputed_MICE, exp = rlm(scale(ScreeLing_2_fon) ~ scale(FBC_subacute_IFOF_R), maxit=50)))) 
concur_2_RIFOF_sem <- summary(pool(with(data=imputed_MICE, exp = rlm(scale(ScreeLing_2_sem) ~ scale(FBC_subacute_IFOF_R), maxit=50)))) 

concur_subacute = list(concur_2_LAF_fon, concur_2_LAF_sem, concur_2_RAF_fon, concur_2_RAF_sem, 
                       concur_2_LIFOF_fon, concur_2_LIFOF_sem, concur_2_RIFOF_fon, concur_2_RIFOF_sem)

save(concur_2_LAF_fon, concur_2_LAF_sem, concur_2_RAF_fon, concur_2_RAF_sem, concur_2_LIFOF_fon, concur_2_LIFOF_sem, concur_2_RIFOF_fon, concur_2_RIFOF_sem, file = here("data", "concur_subacute.RData"))

# Lagged regressions

## Brain to behavior time point 2

cl_beh_LAF_fon <- summary(pool(with(data=imputed_MICE, exp = rlm(scale(ScreeLing_2_fon) ~ scale(FBC_acute_AF_L) + scale(ScreeLing_1_fon), maxit=50))))
#check_collinearity(rlm(scale(ScreeLing_2_fon) ~ scale(FBC_acute_AF_L) + scale(ScreeLing_1_fon), data = PWAWM))
cl_beh_LAF_sem <- summary(pool(with(data=imputed_MICE, exp = rlm(scale(ScreeLing_2_sem) ~ scale(FBC_acute_AF_L) + scale(ScreeLing_1_sem), maxit=50))))
#check_collinearity(rlm(scale(ScreeLing_2_sem) ~ scale(FBC_acute_AF_L) + scale(ScreeLing_1_sem), data = PWAWM))
cl_beh_RAF_fon <- summary(pool(with(data=imputed_MICE, exp = rlm(scale(ScreeLing_2_fon) ~ scale(FBC_acute_AF_R) + scale(ScreeLing_1_fon), maxit=50)))) 
#check_collinearity(rlm(scale(ScreeLing_2_fon) ~ scale(FBC_acute_AF_R) + scale(ScreeLing_1_fon), data = PWAWM))
cl_beh_RAF_sem <- summary(pool(with(data=imputed_MICE, exp = rlm(scale(ScreeLing_2_sem) ~ scale(FBC_acute_AF_R) + scale(ScreeLing_1_sem), maxit=50)))) 
#check_collinearity(rlm(scale(ScreeLing_2_sem) ~ scale(FBC_acute_AF_R) + scale(ScreeLing_1_sem), data = PWAWM))
cl_beh_LIFOF_fon <- summary(pool(with(data=imputed_MICE, exp = rlm(scale(ScreeLing_2_fon) ~ scale(FBC_acute_IFOF_L) + scale(ScreeLing_1_fon), maxit=50))))
#check_collinearity(rlm(scale(ScreeLing_2_fon) ~ scale(FBC_acute_IFOF_L) + scale(ScreeLing_1_fon), data = PWAWM))
cl_beh_LIFOF_sem <- summary(pool(with(data=imputed_MICE, exp = rlm(scale(ScreeLing_2_sem) ~ scale(FBC_acute_IFOF_L) + scale(ScreeLing_1_sem), maxit=50))))
#check_collinearity(rlm(scale(ScreeLing_2_sem) ~ scale(FBC_acute_IFOF_L) + scale(ScreeLing_1_sem), data = PWAWM))
cl_beh_RIFOF_fon <- summary(pool(with(data=imputed_MICE, exp = rlm(scale(ScreeLing_2_fon) ~ scale(FBC_acute_IFOF_R) + scale(ScreeLing_1_fon), maxit=50)))) 
#check_collinearity(rlm(scale(ScreeLing_2_fon) ~ scale(FBC_acute_IFOF_R) + scale(ScreeLing_1_fon), data = PWAWM))
cl_beh_RIFOF_sem <- summary(pool(with(data=imputed_MICE, exp = rlm(scale(ScreeLing_2_sem) ~ scale(FBC_acute_IFOF_R) + scale(ScreeLing_1_sem), maxit=50)))) 
#check_collinearity(rlm(scale(ScreeLing_2_sem) ~ scale(FBC_acute_IFOF_R) + scale(ScreeLing_1_sem), data = PWAWM))

lagged_subacute = list(cl_beh_LAF_fon, cl_beh_LAF_sem, cl_beh_RAF_fon, cl_beh_RAF_sem, 
                       cl_beh_LIFOF_fon, cl_beh_LIFOF_sem, cl_beh_RIFOF_fon, cl_beh_RIFOF_sem)

save(cl_beh_LAF_fon, cl_beh_LAF_sem, cl_beh_RAF_fon, cl_beh_RAF_sem, cl_beh_LIFOF_fon, cl_beh_LIFOF_sem, cl_beh_RIFOF_fon, cl_beh_RIFOF_sem, file = here("data", "lagged_subacute.RData"))

## Brain to behavior time point 3

cl_long_beh_LAF_fon <- summary(pool(with(data=imputed_MICE, exp = rlm(scale(ScreeLing_3_fon) ~ scale(FBC_acute_AF_L) + scale(ScreeLing_1_fon), maxit=50))))
#check_collinearity(rlm(scale(ScreeLing_3_fon) ~ scale(FBC_acute_AF_L) + scale(ScreeLing_1_fon), data = PWAWM))
cl_long_beh_LAF_sem <- summary(pool(with(data=imputed_MICE, exp = rlm(scale(ScreeLing_3_sem) ~ scale(FBC_acute_AF_L) + scale(ScreeLing_1_sem), maxit=50))))
#check_collinearity(rlm(scale(ScreeLing_3_sem) ~ scale(FBC_acute_AF_L) + scale(ScreeLing_1_sem), data = PWAWM))
cl_long_beh_RAF_fon <- summary(pool(with(data=imputed_MICE, exp = rlm(scale(ScreeLing_3_fon) ~ scale(FBC_acute_AF_R) + scale(ScreeLing_1_fon), maxit=50)))) 
#check_collinearity(rlm(scale(ScreeLing_3_fon) ~ scale(FBC_acute_AF_R) + scale(ScreeLing_1_fon), data = PWAWM))
cl_long_beh_RAF_sem <- summary(pool(with(data=imputed_MICE, exp = rlm(scale(ScreeLing_3_sem) ~ scale(FBC_acute_AF_R) + scale(ScreeLing_1_sem), maxit=50)))) 
#check_collinearity(rlm(scale(ScreeLing_3_sem) ~ scale(FBC_acute_AF_R) + scale(ScreeLing_1_sem), data = PWAWM, maxit=100))
cl_long_beh_LIFOF_fon <- summary(pool(with(data=imputed_MICE, exp = rlm(scale(ScreeLing_3_fon) ~ scale(FBC_acute_IFOF_L) + scale(ScreeLing_1_fon), maxit=50))))
#check_collinearity(rlm(scale(ScreeLing_3_fon) ~ scale(FBC_acute_IFOF_L) + scale(ScreeLing_1_fon), data = PWAWM, maxit=100))
cl_long_beh_LIFOF_sem <- summary(pool(with(data=imputed_MICE, exp = rlm(scale(ScreeLing_3_sem) ~ scale(FBC_acute_IFOF_L) + scale(ScreeLing_1_sem), maxit=50))))
#check_collinearity(rlm(scale(ScreeLing_3_sem) ~ scale(FBC_acute_IFOF_L) + scale(ScreeLing_1_sem), data = PWAWM))
cl_long_beh_RIFOF_fon <- summary(pool(with(data=imputed_MICE, exp = rlm(scale(ScreeLing_3_fon) ~ scale(FBC_acute_IFOF_R) + scale(ScreeLing_1_fon), maxit=50)))) 
#check_collinearity(rlm(scale(ScreeLing_3_fon) ~ scale(FBC_acute_IFOF_R) + scale(ScreeLing_1_fon), data = PWAWM))
cl_long_beh_RIFOF_sem <- summary(pool(with(data=imputed_MICE, exp = rlm(scale(ScreeLing_3_sem) ~ scale(FBC_acute_IFOF_R) + scale(ScreeLing_1_sem), maxit=50))))
#check_collinearity(rlm(scale(ScreeLing_3_sem) ~ scale(FBC_acute_IFOF_R) + scale(ScreeLing_1_sem), data = PWAWM))

lagged_chronic = list(cl_long_beh_LAF_fon, cl_long_beh_LAF_sem, cl_long_beh_RAF_fon, cl_long_beh_RAF_sem, 
                      cl_long_beh_LIFOF_fon, cl_long_beh_LIFOF_sem, cl_long_beh_RIFOF_fon, cl_long_beh_RIFOF_sem)

save(cl_long_beh_LAF_fon, cl_long_beh_LAF_sem, cl_long_beh_RAF_fon, cl_long_beh_RAF_sem, cl_long_beh_LIFOF_fon, cl_long_beh_LIFOF_sem, cl_long_beh_RIFOF_fon, cl_long_beh_RIFOF_sem, file = here("data", "lagged_chronic.RData"))

# Predictive regressions

## Brain to behavior time point 2

pred_beh_LAF_fon <- summary(pool(with(data=imputed_MICE, exp = rlm(scale(ScreeLing_2_fon) ~ scale(FBC_acute_AF_L), maxit=50))))
pred_beh_LAF_sem <- summary(pool(with(data=imputed_MICE, exp = rlm(scale(ScreeLing_2_sem) ~ scale(FBC_acute_AF_L), maxit=50))))
pred_beh_RAF_fon <- summary(pool(with(data=imputed_MICE, exp = rlm(scale(ScreeLing_2_fon) ~ scale(FBC_acute_AF_R), maxit=50)))) 
pred_beh_RAF_sem <- summary(pool(with(data=imputed_MICE, exp = rlm(scale(ScreeLing_2_sem) ~ scale(FBC_acute_AF_R), maxit=50)))) 
pred_beh_LIFOF_fon <- summary(pool(with(data=imputed_MICE, exp = rlm(scale(ScreeLing_2_fon) ~ scale(FBC_acute_IFOF_L), maxit=50))))
pred_beh_LIFOF_sem <- summary(pool(with(data=imputed_MICE, exp = rlm(scale(ScreeLing_2_sem) ~ scale(FBC_acute_IFOF_L), maxit=50)))) 
pred_beh_RIFOF_fon <- summary(pool(with(data=imputed_MICE, exp = rlm(scale(ScreeLing_2_fon) ~ scale(FBC_acute_IFOF_R), maxit=50)))) 
pred_beh_RIFOF_sem <- summary(pool(with(data=imputed_MICE, exp = rlm(scale(ScreeLing_2_sem) ~ scale(FBC_acute_IFOF_R), maxit=50)))) 

pred_subacute = list(pred_beh_LAF_fon, pred_beh_LAF_sem, pred_beh_RAF_fon, pred_beh_RAF_sem, 
                     pred_beh_LIFOF_fon, pred_beh_LIFOF_sem, pred_beh_RIFOF_fon, pred_beh_RIFOF_sem)

save(pred_beh_LAF_fon, pred_beh_LAF_sem, pred_beh_RAF_fon, pred_beh_RAF_sem, pred_beh_LIFOF_fon, pred_beh_LIFOF_sem, pred_beh_RIFOF_fon, pred_beh_RIFOF_sem, file = here("data", "pred_subacute.RData"))

## Brain to behavior time point 3

pred_long_beh_LAF_fon <- summary(pool(with(data=imputed_MICE, exp = rlm(scale(ScreeLing_3_fon) ~ scale(FBC_acute_AF_L), maxit=50))))
pred_long_beh_LAF_sem <- summary(pool(with(data=imputed_MICE, exp = rlm(scale(ScreeLing_3_sem) ~ scale(FBC_acute_AF_L), maxit=50))))
pred_long_beh_RAF_fon <- summary(pool(with(data=imputed_MICE, exp = rlm(scale(ScreeLing_3_fon) ~ scale(FBC_acute_AF_R), maxit=50)))) 
pred_long_beh_RAF_sem <- summary(pool(with(data=imputed_MICE, exp = rlm(scale(ScreeLing_3_sem) ~ scale(FBC_acute_AF_R), maxit=50)))) 
pred_long_beh_LIFOF_fon <- summary(pool(with(data=imputed_MICE, exp = rlm(scale(ScreeLing_3_fon) ~ scale(FBC_acute_IFOF_L), maxit=50))))
pred_long_beh_LIFOF_sem <- summary(pool(with(data=imputed_MICE, exp = rlm(scale(ScreeLing_3_sem) ~ scale(FBC_acute_IFOF_L), maxit=50))))
pred_long_beh_RIFOF_fon <- summary(pool(with(data=imputed_MICE, exp = rlm(scale(ScreeLing_3_fon) ~ scale(FBC_acute_IFOF_R), maxit=50)))) 
pred_long_beh_RIFOF_sem <- summary(pool(with(data=imputed_MICE, exp = rlm(scale(ScreeLing_3_sem) ~ scale(FBC_acute_IFOF_R), maxit=50))))

pred_chronic = list(pred_long_beh_LAF_fon, pred_long_beh_LAF_sem, pred_long_beh_RAF_fon, pred_long_beh_RAF_sem, 
                    pred_long_beh_LIFOF_fon, pred_long_beh_LIFOF_sem, pred_long_beh_RIFOF_fon, pred_long_beh_RIFOF_sem)

save(pred_long_beh_LAF_fon, pred_long_beh_LAF_sem, pred_long_beh_RAF_fon, pred_long_beh_RAF_sem, pred_long_beh_LIFOF_fon, pred_long_beh_LIFOF_sem, pred_long_beh_RIFOF_fon, pred_long_beh_RIFOF_sem, file = here("data", "pred_chronic.RData"))

# Change analysis

## Assess change in FBC over time

PWAWM_long <- PWAWM %>% 
  select(c("patient", "age", "FBC_acute_AF_L", "FBC_acute_AF_R", "FBC_acute_IFOF_L", "FBC_acute_IFOF_R", "FBC_subacute_AF_L", "FBC_subacute_AF_R", "FBC_subacute_IFOF_L", "FBC_subacute_IFOF_R")) %>%
  pivot_longer(cols = starts_with("FBC"),
               names_to = c(".value", "timepoint", "tract", "laterality"),
               names_pattern = "(.*)_(.*)_(.*)_(.)")

change_LAF <- PWAWM_long %>% filter(tract=="AF" & laterality == "L") %>% wilcox_test(FBC ~ timepoint, paired=T)
eff_LAF <- PWAWM_long %>% filter(tract=="AF" & laterality == "L") %>% wilcox_effsize(FBC ~ timepoint, paired=T) 
change_RAF <- PWAWM_long %>% filter(tract=="AF" & laterality == "R") %>% wilcox_test(FBC ~ timepoint, paired=T)
eff_RAF <- PWAWM_long %>% filter(tract=="AF" & laterality == "R") %>% wilcox_effsize(FBC ~ timepoint, paired=T) 
change_LIFOF <- PWAWM_long %>% filter(tract=="IFOF" & laterality == "L") %>% wilcox_test(FBC ~ timepoint, paired=T)
eff_LIFOF <- PWAWM_long %>% filter(tract=="IFOF" & laterality == "L") %>% wilcox_effsize(FBC ~ timepoint, paired=T) 
change_RIFOF <- PWAWM_long %>% filter(tract=="IFOF" & laterality == "R") %>% wilcox_test(FBC ~ timepoint, paired=T)
eff_RIFOF <- PWAWM_long %>% filter(tract=="IFOF" & laterality == "R") %>% wilcox_effsize(FBC ~ timepoint, paired=T) 

save(change_LAF, eff_LAF, change_RAF, eff_RAF, change_LIFOF, eff_LIFOF, change_RIFOF, eff_RIFOF, file = here("data", "change_time.RData"))

## Change in brain to behavior time point 2

cl_beh_deltaLAF_fon <- summary(pool(with(data=imputed_MICE, exp = rlm(scale(ScreeLing_2_fon) ~ scale(delta_FBC_AF_L) + scale(ScreeLing_1_fon), maxit=50))))
#check_collinearity(rlm(scale(ScreeLing_2_fon) ~ scale(delta_FBC_AF_L) + scale(ScreeLing_1_fon), data = PWAWM))
cl_beh_deltaLAF_sem <- summary(pool(with(data=imputed_MICE, exp = rlm(scale(ScreeLing_2_sem) ~ scale(delta_FBC_AF_L) + scale(ScreeLing_1_sem), maxit=50))))
#check_collinearity(rlm(scale(ScreeLing_2_sem) ~ scale(delta_FBC_AF_L) + scale(ScreeLing_1_sem), data = PWAWM))
cl_beh_deltaRAF_fon <- summary(pool(with(data=imputed_MICE, exp = rlm(scale(ScreeLing_2_fon) ~ scale(delta_FBC_AF_R) + scale(ScreeLing_1_fon), maxit=50)))) 
#check_collinearity(rlm(scale(ScreeLing_2_fon) ~ scale(delta_FBC_AF_R) + scale(ScreeLing_1_fon), data = PWAWM))
cl_beh_deltaRAF_sem <- summary(pool(with(data=imputed_MICE, exp = rlm(scale(ScreeLing_2_sem) ~ scale(delta_FBC_AF_R) + scale(ScreeLing_1_sem), maxit=50)))) 
#check_collinearity(rlm(scale(ScreeLing_2_sem) ~ scale(delta_FBC_AF_R) + scale(ScreeLing_1_sem), data = PWAWM))

change_subacute = list(cl_beh_deltaLAF_fon, cl_beh_deltaLAF_sem, cl_beh_deltaRAF_fon, cl_beh_deltaRAF_sem)

save(cl_beh_deltaLAF_fon, cl_beh_deltaLAF_sem, cl_beh_deltaRAF_fon, cl_beh_deltaRAF_sem, file = here("data", "change_subacute.RData"))

## Change in brain to behavior time point 3

cl_long_beh_deltaLAF_fon <- summary(pool(with(data=imputed_MICE, exp = rlm(scale(ScreeLing_3_fon) ~ scale(delta_FBC_AF_L) + scale(ScreeLing_1_fon), maxit=50))))
#check_collinearity(rlm(scale(ScreeLing_3_fon) ~ scale(delta_FBC_AF_L) + scale(ScreeLing_1_fon), data = PWAWM))
cl_long_beh_deltaLAF_sem <- summary(pool(with(data=imputed_MICE, exp = rlm(scale(ScreeLing_3_sem) ~ scale(delta_FBC_AF_L) + scale(ScreeLing_1_sem), maxit=50))))
#check_collinearity(rlm(scale(ScreeLing_3_sem) ~ scale(delta_FBC_AF_L) + scale(ScreeLing_1_sem), data = PWAWM))
cl_long_beh_deltaRAF_fon <- summary(pool(with(data=imputed_MICE, exp = rlm(scale(ScreeLing_3_fon) ~ scale(delta_FBC_AF_R) + scale(ScreeLing_1_fon), maxit=50))))
#check_collinearity(rlm(scale(ScreeLing_3_fon) ~ scale(delta_FBC_AF_R) + scale(ScreeLing_1_fon), data = PWAWM))
cl_long_beh_deltaRAF_sem <- summary(pool(with(data=imputed_MICE, exp = rlm(scale(ScreeLing_3_sem) ~ scale(delta_FBC_AF_R) + scale(ScreeLing_1_sem), maxit=50))))
#check_collinearity(rlm(scale(ScreeLing_3_sem) ~ scale(delta_FBC_AF_R) + scale(ScreeLing_1_sem), data = PWAWM))

change_chronic = list(cl_long_beh_deltaLAF_fon, cl_long_beh_deltaLAF_sem, cl_long_beh_deltaRAF_fon, cl_long_beh_deltaRAF_sem)

save(cl_long_beh_deltaLAF_fon, cl_long_beh_deltaLAF_sem, cl_long_beh_deltaRAF_fon, cl_long_beh_deltaRAF_sem, file = here("data", "change_chronic.RData"))

# Check robustness of found effects

covariate_acute = c("scale(stroke_size)", "scale(age)", "scale(old_lesion_load)", "scale(ScreeLing_1_dpo)")
covariate_subacute = c("scale(stroke_size)", "scale(age)", "scale(old_lesion_load)", "scale(ScreeLing_2_dpo)")

concur_1_LAF_fon_robust <- lapply(covariate_acute, function(predictor){
  summary(pool(with(data=imputed_MICE, exp = rlm(as.formula(paste0("scale(ScreeLing_1_fon) ~ scale(FBC_acute_AF_L) + ", predictor)), maxit=50))))
  }) 
#check_collinearity(rlm(scale(ScreeLing_1_fon) ~ scale(FBC_acute_AF_L) + scale(stroke_size), data = PWAWM))

concur_1_LIFOF_sem_robust <- lapply(covariate_acute, function(predictor){
  summary(pool(with(data=imputed_MICE, exp = rlm(as.formula(paste0("scale(ScreeLing_1_sem) ~ scale(FBC_acute_IFOF_L) + ", predictor)), maxit=50)))) 
  }) 
#check_collinearity(rlm(scale(ScreeLing_1_sem) ~ scale(FBC_acute_IFOF_L) + scale(stroke_size), data = PWAWM))

concur_1_LIFOF_fon_robust <- lapply(covariate_acute, function(predictor){
  summary(pool(with(data=imputed_MICE, exp = rlm(as.formula(paste0("scale(ScreeLing_1_fon) ~ scale(FBC_acute_IFOF_L) + ", predictor)), maxit=50)))) 
  })
#check_collinearity(rlm(scale(ScreeLing_1_fon) ~ scale(FBC_acute_IFOF_L) + scale(stroke_size), data = PWAWM))

concur_1_RIFOF_sem_robust <- lapply(covariate_acute, function(predictor){
  summary(pool(with(data=imputed_MICE, exp = rlm(as.formula(paste0("scale(ScreeLing_1_sem) ~ scale(FBC_acute_IFOF_R) + ", predictor)), maxit=50)))) 
  })
#check_collinearity(rlm(scale(ScreeLing_1_sem) ~ scale(FBC_acute_IFOF_R) + scale(stroke_size), data = PWAWM))

concur_1_RIFOF_fon_robust <- lapply(covariate_acute, function(predictor){
  summary(pool(with(data=imputed_MICE, exp = rlm(as.formula(paste0("scale(ScreeLing_1_fon) ~ scale(FBC_acute_IFOF_R) + ", predictor)), maxit=50)))) 
  }) 
#check_collinearity(rlm(scale(ScreeLing_1_fon) ~ scale(FBC_acute_IFOF_R) + scale(stroke_size), data = PWAWM))

pred_beh_RAF_sem_robust <- lapply(covariate_subacute, function(predictor){
  summary(pool(with(data=imputed_MICE, exp = rlm(as.formula(paste0("scale(ScreeLing_2_sem) ~ scale(FBC_acute_AF_R) + ", predictor)), maxit=50)))) 
  }) 
#check_collinearity(rlm(scale(ScreeLing_2_sem) ~ scale(FBC_acute_AF_R) + scale(stroke_size), data = PWAWM))

pred_beh_LIFOF_sem_robust <- lapply(covariate_subacute, function(predictor){
  summary(pool(with(data=imputed_MICE, exp = rlm(as.formula(paste0("scale(ScreeLing_2_sem) ~ scale(FBC_acute_IFOF_L) + ", predictor)), maxit=50))))
  }) 

cl_beh_deltaRAF_fon_robust <- lapply(covariate_subacute, function(predictor){
  summary(pool(with(data=imputed_MICE, exp = rlm(as.formula(paste0("scale(ScreeLing_2_fon) ~ scale(ScreeLing_1_fon) + scale(delta_FBC_AF_R) + ", predictor)), maxit=50)))) 
  }) 

save(concur_1_LAF_fon_robust, concur_1_LIFOF_sem_robust, concur_1_LIFOF_fon_robust, concur_1_RIFOF_sem_robust, concur_1_RIFOF_fon_robust, cl_beh_deltaRAF_fon_robust, file = here("data", "robust.RData"))

# Save lists

save(concur_acute, concur_subacute, lagged_subacute, lagged_chronic, 
     pred_subacute, pred_chronic, change_subacute, change_chronic,
     file = here("data", "model.RData"))
