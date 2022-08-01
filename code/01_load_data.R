# Load libraries --------------------------------------------------------------

# Assuming these packages are already locally installed

library(here)
library(dplyr)
library(stringr)
library(tidyr)
library(labelled)
library(VIM)
library(mice)
library(ggstatsplot)
library(lavaan)
library(multicon)
library(lme4)
library(ppcor)
library(gtsummary)
library(DiagrammeR)
library(magick)
library(flextable)
library(emmeans)
library(ggplot2)
library(rstatix)
library(cowplot)
library(DiagrammeRsvg)
library(ggpubr)
library(performance)
library(papaja)

# Helper functions --------------------------------------------------------

f_pvalue = function(p.value, symbol = " = "){
  p.value <- round(p.value, digits = 3)
  if (p.value < 0.001) {
    return("< .001")
  } else {
    return(paste0(symbol, round(p.value, digits = 2)))
  }
}

numformat <- function(x, digits = 2) { 
  ncode <- paste0("%.", digits, "f")
  sub("^(-?)0.", "\\1.", sprintf(ncode, x))
}

cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

rms <- function(x1, x2, x3){
  sqrt(((x1^2)+(x2^2)+(x3^2))/3)
}

# Load data --------------------------------------------------------------

PWAWM <- read.csv(here("data", "pwawm_tidy.csv"), header = TRUE, sep = ",", dec = ".", check.names=T)

var_label(PWAWM) <- list(
  ScreeLing_1_tot = "Initial language score", ScreeLing_1_dpo = "Days post stroke (acute)", 
  ScreeLing_2_dpo = "Days post stroke (subacute)", ScreeLing_3_dpo = "Days post stroke (chronic)",
  stroke_size = "Acute lesion volume", old_lesion_load = "Old lesion load", 
  eTIV_acute = "Estimated total intracranial volume (acute)", eTIV_subacute = "Estimated total intracranial volume (subacute)",
  age = "Age (years)", sex = "Sex", handedness = "Handedness", education = "Education (years)", 
  NIHSS_total = "Acute NIHSS total score", NIHSS_language = "Acute NIHSS language score", 
  stroke_area = "Affected circulation area", stroke_type = "Stroke type", 
  stroke_laterality = "Stroke laterality", stroke_history = "History of stroke"
)

PWAWM <- PWAWM %>% mutate_if(is.character, ~factor(.)) %>% mutate(patient = as.character(patient))
