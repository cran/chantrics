## ----knitr_init, include=FALSE------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.retina = 2
)

## ----setup--------------------------------------------------------------------
library(chantrics)

## ----datagen------------------------------------------------------------------
set.seed(123)
x <- rnorm(250)
y <- rnbinom(250, mu = exp(1 + x), size = 1)

## Fit the Poisson glm model, which is not correctly specified
fm_pois <- glm(y ~ x + I(x^2), family = poisson)
lmtest::coeftest(fm_pois)
## The I(x^2) term is spuriously significant.

## ----adjust_fmpois------------------------------------------------------------
fm_pois_adj <- adj_loglik(fm_pois)
coef(fm_pois_adj) # class "numeric"
summary(fm_pois_adj)
lmtest::coeftest(fm_pois_adj)

## ----ci-----------------------------------------------------------------------
chandwich::conf_intervals(fm_pois_adj)
chandwich::conf_intervals(fm_pois_adj, type = "spectral", conf = 99)
confint(fm_pois_adj)

## ----conf_region--------------------------------------------------------------
fm_pois_adj_vert <-
  chandwich::conf_region(fm_pois_adj, which_pars = c("x", "I(x^2)"))
fm_pois_adj_none <-
  chandwich::conf_region(fm_pois_adj,
    which_pars = c("x", "I(x^2)"),
    type = "none"
  )

## ----conf_region_plot, fig.align='center', fig.width=7, fig.height=7, out.width = "110%"----
par(mar = c(5.1, 5.1, 2.1, 2.1))
plot(
  fm_pois_adj_vert,
  fm_pois_adj_none,
  conf = c(60, 80, 90, 95),
  col = c("brown", "darkgreen"),
  lty = c(1, 2),
  lwd = 2.5
)
par(mar = c(5.1, 4.1, 4.1, 2.1))

## ----plot_chan, fig.align='center', fig.width=7, fig.height=7-----------------
fm_pois_smallest_adj <- update(fm_pois_adj, . ~ 1)
plot(fm_pois_smallest_adj, type = 1:4, col = 1:4, legend_pos = "bottom", lwd = 2.5)

## ----swisslaborimport---------------------------------------------------------
data("SwissLabor", package = "AER")
swiss_probit <-
  glm(participation ~ . + I(age^2),
    data = SwissLabor,
    family = binomial(link = "probit")
  )
swiss_probit_adj <- adj_loglik(swiss_probit)
lmtest::coeftest(swiss_probit_adj)

## ----update-------------------------------------------------------------------
swiss_probit_small_adj <-
  update(swiss_probit_adj, . ~ . - I(age^2) - education)
swiss_probit_smaller_adj <-
  update(swiss_probit_adj, . ~ . - I(age^2) - education - youngkids - oldkids)

## ----anova--------------------------------------------------------------------
anova(swiss_probit_adj, swiss_probit_small_adj, swiss_probit_smaller_adj)

## ----sequential---------------------------------------------------------------
anova(swiss_probit_adj)

## ----alrtest------------------------------------------------------------------
alrtest(swiss_probit_adj, 3, "oldkids")
alrtest(swiss_probit_adj, . ~ . - youngkids - foreign, . ~ . - education)
alrtest(swiss_probit_adj, swiss_probit_small_adj)

## ----poisson------------------------------------------------------------------
summary(fm_pois) # fm_pois is the fitted poisson model from above.
fm_pois_adj <- adj_loglik(fm_pois)
summary(fm_pois_adj)

## ----binomial-----------------------------------------------------------------
data("SwissLabor", package = "AER")
## Fitting a Probit model
swiss_probit <-
  glm(participation ~ . + I(age^2),
    data = SwissLabor,
    family = binomial(link = "probit")
  )
swiss_probit_adj <- adj_loglik(swiss_probit)
summary(swiss_probit_adj)

## Fitting a Logit model
swiss_logit <-
  glm(formula(swiss_probit),
    data = SwissLabor,
    family = binomial(link = "logit")
  )
swiss_logit_adj <- adj_loglik(swiss_logit)
summary(swiss_logit_adj)

## -----------------------------------------------------------------------------
data("CPS1988", package = "AER")
cps_glm <-
  glm(
    log(wage) ~ experience + I(experience^2) + education + ethnicity,
    data = CPS1988,
    family = gaussian()
  )
summary(cps_glm)
cps_glm_adj <- adj_loglik(cps_glm)
summary(cps_glm_adj)

## ----negbin-------------------------------------------------------------------
fm_negbin_theta <- MASS::glm.nb(y ~ x)
summary(fm_negbin_theta)
fm_negbin_theta_adj <- adj_loglik(fm_negbin_theta)
summary(fm_negbin_theta_adj)

