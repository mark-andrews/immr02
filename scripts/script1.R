M <- glm(cbind(m, n-m) ~ 1,
         family = binomial(link = 'logit'),
         data = rats_df_42)

ilogit <- plogis
ilogit <- function(x) {1/(1 + exp(-x))}

# 
rats_df <- rats_df %>% mutate(batch = as.character(batch))
rats_df <- rats_df %>% mutate(batch = factor(batch))

# The *non* multilevel model
M <- glm(cbind(m, n-m) ~ 0 + batch,
         family = binomial(link = 'logit'),
         data = rats_df)

# The multilevel model
library(lme4)
M_ml <- glmer(cbind(m, n-m) ~ 1 + (1|batch),
              data = rats_df,
              family = binomial(link = 'logit')
)

# To see the value of "beta" (as in formula/diagram)
# Use ranef(M_ml) + fixef(M_ml) or coef(M_ml)



# Visualize shrinkage

# Non multilevel model

library(broom)
M <- glm(cbind(m, n-m) ~ 0 + batch,
         data = rats_df,
         family = binomial(link = 'logit')
)

M_estimates <- tidy(M) %>%
  select(term, beta = estimate) %>%
  mutate(term = str_remove(term, 'batch'),
         theta = ilogit(beta)) %>%
  rename(batch = term)

# Multilevel model
M_ml_estimates <- coef(M_ml)$batch

M_ml_estimates %>%
  head()


## ---- shrinkage, echo=F, fig.cap='Estimates for $\\theta_1, \\theta_2 \\dots \\theta_j \\ldots \\theta_J$ from the flat or non-multilevel model (left) and the multilevel model (right).', fig.align='center', out.width='0.75\\textwidth'----
library(latex2exp)
M_ml_estimates %>%
  rownames_to_column('batch') %>%
  inner_join(M_estimates, by='batch') %>%
  select(batch, multilevel = '(Intercept)', flat = beta) %>%
  gather(type, estimate, -batch) %>%
  mutate(estimate = ilogit(estimate)) %>%
  ggplot(aes(x = type, y = estimate, group = batch)) +
  geom_point() +
  geom_line(size = 0.5, alpha = 0.25) 



# Normal random effects models
alcohol_df <- read_csv("data/alcohol.csv")

# Non multilevel model for Russia
M <- lm(alcohol ~ 1, data = filter(alcohol_df, country == 'Russia'))

# Non multilevel model for all countries
M <- lm(alcohol ~ 0 + country, data = alcohol_df)

# Multilevel model for all countries
M_ml <- lmer(alcohol ~ 1 + (1|country),
             data = alcohol_df)
            

# Get sleepstudy

ggplot(sleepstudy,
       aes(x = Days, y = Reaction, colour = Subject)
) + geom_point() + 
  facet_wrap(~Subject) + 
  theme_classic() +
  stat_smooth(method = 'lm', se = F)

# A linear model for subject 350
M_350 <- lm(Reaction ~ Days, data = filter(sleepstudy, Subject == 350))

# A non multilevel model for all subjects
# this is a varying slope and varying intercept model
# but with no model of the variability
M_flat <- lm(Reaction ~ 0 + Subject + Subject:Days, data = sleepstudy)


# Multilevel linear model
M_ml <- lmer(Reaction ~ Days + (Days|Subject),
             data = sleepstudy)



library(ggrepel)
library(cowplot)
library(magrittr)

b <- fixef(M_ml)
s <- VarCorr(M_ml)$Subject %>% attr('stddev')
s <- diag(s)
P <- VarCorr(M_ml)$Subject %>% attr('correlation')
Sigma <- s %*% P %*% s

contour_df <- map(c(0.25, 0.5, 0.75, 0.9, 0.95),
                  ~ellipse::ellipse(x = Sigma, centre = b, level = .) %>%
                    as_tibble()
) %>% bind_rows(.id = 'level')

M_ml_beta <- M_ml %>%
  coef() %>%
  extract2('Subject') %>%
  rename(intercept = `(Intercept)`, slope = Days) %>%
  rownames_to_column('subject')

M_beta <- M_flat %>%
  coef() %>%
  enframe() %>%
  separate(name, into=c('subject', 'coef'), sep = ':', fill = 'right') %>%
  spread(coef, value) %>%
  mutate(subject = str_remove(subject, 'Subject')) %>%
  select(subject, intercept = `<NA>`, slope=Days)

beta_df2 <- inner_join(M_beta, M_ml_beta, by='subject', suffix = c('_flat', '_multilevel'))

beta_df <- beta_df2 %>%
  pivot_longer(-subject,
               names_to = c('.value','model_type'),
               names_pattern = "(intercept|slope)_(flat|multilevel)")

beta_df_flat <- beta_df %>% filter(model_type == 'flat')
beta_df_ml <- beta_df %>% filter(model_type == 'multilevel')

# Shrinkage of coefficients viewed by the lines of best fit
p1 <- beta_df %>%
  ggplot() +
  geom_abline(aes(intercept = intercept, slope = slope, colour = model_type)) +
  facet_wrap(~subject) +
  scale_color_manual(values=c( "#E69F00", "#56B4E9")) +
  lims(x = c(0, 10),
       y = c(200, 500)) +
  theme_minimal()

# Shrinkage of coefficients superimposed on multivariate normal
p2 <- ggplot() +
  geom_path(data = contour_df,
            mapping = aes(x = x, y = y, group = level), size = 0.5, alpha = 0.5) +
  geom_segment(data = beta_df2,
               mapping = aes(x = intercept_flat, xend = intercept_multilevel, y = slope_flat, yend = slope_multilevel),
               arrow = arrow(length = unit(0.01, "npc"))
  ) +
  geom_text_repel(data = beta_df_flat,
                  mapping = aes(x = intercept, y = slope, label = subject), size = 3) +
  theme(legend.position = "bottom") + scale_shape_manual(values=c(1, 16)) +
  labs(x = TeX('$\\beta_0$'),
       y = TeX('$\\beta_1$'))

p2

M_ml <- lmer(Reaction ~ 1 + Days + (1 + Days|Subject),
             data = sleepstudy)


# random intercept only multilevel model
M_ml_vi <- lmer(Reaction ~ Days + (1|Subject),
             data = sleepstudy)

# random slope only multilevel model
M_ml_vs <- lmer(Reaction ~ Days + (0 + Days|Subject),
                data = sleepstudy)

# random slope and random intercept, but zero correlation,  multilevel model
M_ml_vsvi <- lmer(Reaction ~ Days + (0 + Days|Subject) + (1|Subject),
                    data = sleepstudy)

# random slope and random intercept, but zero correlation,  multilevel model
M_ml_vsvi <- lmer(Reaction ~ Days + (Days || Subject),
                  data = sleepstudy)




