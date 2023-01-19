# This script produces publication-ready LaTeX tables 

library(knitr)
library(modelsummary)
options(modelsummary_format_numeric_latex = "plain") 

# Load data
full.data <- st_read(here("data", "full-data.shp"))

# Load models
mod.1 <- read_rds(here("models", "mod-1.rds"))
mod.2 <- read_rds(here("models", "mod-2.rds"))
mod.3 <- read_rds(here("models", "mod-3.rds"))
mod.4 <- read_rds(here("models", "mod-4.rds"))

# Table of mean values of income, race, Fr, and PM2.5
# by quintiles of tract-level asthma rate 
full.data %>% 
  mutate(rate = n / child.pop, 
         rate.q = ntile(rate, 5)) %>% 
  group_by(rate.q) %>% 
  summarise(pov = round(mean(pct.under.2.00), 2),
            Fr = round(mean(Fr), 2),
            pct_nonwht = round(mean(pct_nonwht), 2),
            pm25 = round(mean(pm25.mean), 2)) %>% 
  t() %>% 
  kable("latex")

# List of models
models <- list(
  "Model 1" = mod.1,
  "Model 2" = mod.2,
  "Model 3" = mod.3,
  "Model 4" = mod.4
)

# Model comparison table
modelsummary(models, 
             statistic = 'conf.int', 
             conf_level = 0.89,
             exponentiate = T, 
             fmt = 2,
             output = "latex",
             coef_map = c("b_Intercept" = "Intercept",
                          "b_pct.under.2.00" = "Prop. poverty ratio under 2.0",
                          "b_race.q2" = "Prop. non-white Q2",
                          "b_race.q3" = "Prop. non-white Q3",
                          "b_race.q4" = "Prop. non-white Q4",
                          "b_race.q5" = "Prop. non-white Q5",
                          "b_Fr" = "Fr",
                          "b_lst" = "LST",
                          "b_pm.q2" = "PM_2.5 Q2",
                          "b_pm.q3" = "PM_2.5 Q3",
                          "b_pm.q4" = "PM_2.5 Q4",
                          "b_pm.q5" = "PM_2.5 Q5",
                          "b_Fr:pm.q2" = "Fr:PM_2.5 Q2",
                          "b_Fr:pm.q3" = "Fr:PM_2.5 Q3",
                          "b_Fr:pm.q4" = "Fr:PM_2.5 Q4",
                          "b_Fr:pm.q5" = "Fr:PM_2.5 Q5"),
             metrics = c("R2", "WAIC"))
