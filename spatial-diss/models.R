# This script runs 5 different Bayesian models using brms
# The child population per tract is included as an offset
# to effectively model the asthma rate rather than the count.
# The negative binomial distribution accounts for overdispersion
# in a Poisson process. Priors were selected to be weakly informative
# and chosen based on prior predictive checks.

library(brms)
library(tidyverse)

# Model 1
# Count ~ Intercept + Income + Race
set.seed(5511)
mod.1 <- brm(n  ~ 1 + pct.under.2.00 + race.q + offset(log(child.pop)),
             data = full.data,
             family = negbinomial(),
             cores = 4, 
             iter = 3000,
             init = "0",
             prior = c(set_prior("normal(0, 5)", class = "b"),
                       set_prior("normal(0, 5)", class = "Intercept"),
                       set_prior("gamma(2, 1)", class = "shape")))

# Write model results
write_rds(mod.1, here("models", "mod-1.rds"))

# Model 2
# Count ~ Intercept + Income + Race + Fr
set.seed(5511)
mod.2 <- brm(n  ~ 1 + pct.under.2.00 + race.q + Fr + offset(log(child.pop)),
             data = full.data,
             family = negbinomial(),
             cores = 4, 
             iter = 3000,
             init = "0",
             prior = c(set_prior("normal(0, 5)", class = "b"),
                       set_prior("normal(0, 5)", class = "Intercept"),
                       set_prior("gamma(2, 1)", class = "shape")))

# Write model results
write_rds(mod.2, here("models", "mod-2.rds"))

# Model 3
# Count ~ Intercept + Income + Race + PM2.5
set.seed(5511)
mod.3 <- brm(n  ~ 1 + pct.under.2.00 + race.q + pm.q + offset(log(child.pop)),
             data = full.data,
             family = negbinomial(),
             cores = 4, 
             iter = 3000,
             init = "0",
             prior = c(set_prior("normal(0, 5)", class = "b"),
                       set_prior("normal(0, 5)", class = "Intercept"),
                       set_prior("gamma(2, 1)", class = "shape")))

# Write model results
write_rds(mod.3, here("models", "mod-3.rds"))

# Model 4
# Count ~ Intercept + Income + Race + Fr + PM2.5
set.seed(5511)
mod.4 <- brm(n  ~ 1 + pct.under.2.00 + race.q + Fr + pm.q + offset(log(child.pop)),
             data = full.data,
             family = negbinomial(),
             cores = 4, 
             iter = 3000,
             init = "0",
             prior = c(set_prior("normal(0, 5)", class = "b"),
                       set_prior("normal(0, 5)", class = "Intercept"),
                       set_prior("gamma(2, 1)", class = "shape")))

# Write model results
write_rds(mod.4, here("models", "mod-4.rds"))

#### Interaction

# Model 5
# Count ~ Intercept + Income + Race + Fr + PM2.5 + Fr * PM2.5
set.seed(5511)
mod.5 <- brm(n  ~ 1 + pct.under.2.00 + race.q + Fr + pm.q +
                pm.q:Fr + 
                offset(log(child.pop)),
              data = full.data,
              family = negbinomial(),
              cores = 4, 
              iter = 3000,
              #control = list(adapt_delta = 0.99),
              init = "0",
              prior = c(set_prior("normal(0, 5)", class = "b"),
                        set_prior("normal(0, 5)", class = "Intercept"),
                        set_prior("gamma(2, 1)", class = "shape")))

# Write model results
write_rds(mod.5, here("models", "mod-5.rds"))

