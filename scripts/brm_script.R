## ------------------------------------------------------------------------------------------------------------------------
library(tidyverse)
library(modelr)
library(magrittr)
library(brms)
theme_set(theme_classic())


# ------- Data setup --------
set.seed(10101) # Omit or change this if you like

N <- 25

x_1 <- rnorm(N)
x_2 <- rnorm(N)

beta_0 <- 1.25
beta_1 <- 1.75
beta_2 <- 2.25

mu <- beta_0 + beta_1 * x_1 + beta_2 * x_2

y <- mu + rnorm(N, mean=0, sd=1.75)

Df <- tibble(x_1, x_2, y)

# ------ Standard, non-Bayesian, linear regression -------
M_lm <- lm(y ~ x_1 + x_2, data=Df)

# ------ Bayesian linear regression ----------

# Set-up and sample from Bayesian linear model
# using defaults for more or less everything
M_bayes <- brm(y ~ x_1 + x_2, data = Df)

# Overriding defaults
M_bayes <- brm(y ~ x_1 + x_2, 
               data = Df,
               cores = 4, # I have a dual-core
               chains = 4, # 4 chains is typical
               iter = 2500,
               warmup = 1000, # these are initilization etc iterations
               prior = set_prior('normal(0, 100)'), # flat prior on coefs
               seed = 101011, # for reproducibility
               save_all_pars = T # needed for bayes_factor
)


# Get the model summary
summary(M_bayes)

# Plot the posteriors etc
plot(M_bayes)

# See plots of e.g. y ~ x_1, y ~ x_2 
marginal_effects(M_bayes)

# View the posterior samples 
posterior_samples(M_bayes)


# Get predictions of the model, using original predictor values
predict(M_bayes)

# predictions with new data, with new predictors
predict(M_bayes, newdata = data.frame(x_1 = c(0, 1, 2), 
                                      x_2 = c(-1, 1, 2))
)

# We can view the stan code of this model like so:
stancode(M_bayes)

# We can view the priors of this model like this:
prior_summary(M_bayes)


# ------ Model comparison ----------
M_bayes <- brm(y ~ x_1 + x_2, 
               data = Df,
               cores = 4, # I have a dual-core
               chains = 4, # 4 chains is typical
               iter = 2500,
               warmup = 1000, # these are initilization etc iterations
               prior = set_prior('normal(0, 100)'), # flat prior on coefs
               seed = 101011, # for reproducibility
               save_all_pars = T # needed for bayes_factor
)

# Set up a null model
M_bayes_null <- brm(y ~ x_1, 
                    data=Df,
                    cores = 4, # I have a dual-core
                    chains = 4, # 4 chains is typical
                    iter = 2500,
                    warmup = 1000, # these are initilization etc iterations
                    prior = set_prior('normal(0, 100)'), # flat prior on coefs
                    seed = 101013, # for reproducibility
                    save_all_pars = T
)

# Calculate and compare WAIC scores
waic(M_bayes_null, M_bayes)

# Calculate and compare LOO-CV IC scores
loo(M_bayes_null, M_bayes)

# ------ Bayes factors ---------

BF <- bayes_factor(M_bayes_null, M_bayes, log = T)


# interactions ------------------------------------------------------------

Df <- read_csv("data/beautyeval.csv")

# Visualize it
ggplot(Df,
       mapping=aes(x=beauty, y=eval, col=sex)) + 
  geom_point() +
  geom_smooth(method='lm') +
  xlab('Lecturer attractiveness') +
  ylab('Teaching evaluation score') +
  ggtitle('Do good looking lecturers get better teaching evaluation score?') +
  theme_classic()


# Classical lm
M_lm <- lm(eval ~ beauty*sex, data=Df)

M_bayes <- brm(eval ~ beauty*sex, 
               data = Df,
               cores = 2, 
               prior = set_prior('normal(0, 100)'), 
               save_all_pars = T 
)

# We'll do a model comparison comparing the above 
# model to an additive, i.e. non-interaction, model

M_lm_additive <- lm(eval ~ beauty + sex, data = Df)
M_bayes_additive <- brm(eval ~ beauty + sex, 
                        data = Df,
                        cores = 2, 
                        prior = set_prior('normal(0, 100)'), 
                        save_all_pars = T 
)

waic(M_bayes_additive, M_bayes)
loo(M_bayes_additive, M_bayes)
bayes_factor(M_bayes_additive, M_bayes)


# Bayesian lmer -----------------------------------------------------------

library(lme4)

Df <- sleepstudy # rename the data frame

# Visualize it
ggplot(Df,
       aes(x=Days, y=Reaction, col=Subject)
) + geom_point() +
  stat_smooth(method='lm', se=F, size=0.5) +
  facet_wrap(~Subject) +
  theme_classic()



# Random intercepts model

M_lmer_ri <- lmer(Reaction ~ Days + (1|Subject),
                  data = sleepstudy)

M_ri <- brm(Reaction ~ Days + (1|Subject),
            cores = 4,               
            prior = set_prior('normal(0, 100)'), # flat prior on coef
            save_all_pars = T,
            data = sleepstudy)

# Random intercepts and random slopes model
M_lmer <- lmer(Reaction ~ Days + (Days|Subject),
               data = sleepstudy)

M <- brm(Reaction ~ Days + (Days|Subject),
         cores = 2,               
         prior = set_prior('normal(0, 100)'), # flat prior on coefs
         save_all_pars = T,
         data = sleepstudy)


# Model comparison
waic(M_ri, M)
loo(M_ri, M)
#bayes_factor(M_ri, M)

# Nested models
Df <- read_csv('data/science.csv')

M_2 <- brm(like ~ sex + PrivPub + (1|school) + (1|Class), 
           cores = 2,               
           prior = set_prior('normal(0, 100)'), # flat prior on coefs
           save_all_pars = T,
           data = Df)


# Ordinal logistic 
M_3 <- brm(like ~ sex + PrivPub + (1|school) + (1|Class), 
           cores = 2,               
           prior = set_prior('normal(0, 100)'), # flat prior on coefs
           save_all_pars = T,
           family=cumulative("logit"), 
           data = Df)


# consider using control = list(adapt_delta = 0.95)

# Multilevel logistic regression
Df %<>% mutate(fast_rt = Reaction < median(Reaction))

M <- brm(fast_rt ~ Days + (Days|Subject),
         family = bernoulli(),
         cores = 2,               
         prior = set_prior('normal(0, 100)'), # flat prior on coefs
         save_all_pars = T,
         data = Df)

