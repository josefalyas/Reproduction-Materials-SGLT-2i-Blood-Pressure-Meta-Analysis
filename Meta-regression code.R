########################################
# READ ME:                             #
# yi = md                              #
# vi = se (standard error)             #
########################################

# Pre-requisites:
library(metafor)
library(meta)
library(brms)

########################################
########################################
# Frequentist models                   #
########################################
########################################

dat <- read.csv2("sbpfulltest.csv")
dat$diabetes <- factor(dat$diabetes, levels = c("n","y"))
dat$eod <- factor(dat$eod, levels = c("n","y"))
dat$bpmeasurement <- factor(dat$bpmeasurement, levels = c("Office","24H"))

full_formula <- 
  yi ~ eod + diabetes + bpmeasurement + base_sbp

full_model <- rma.uni(
  full_formula, 
  sei = vi, 
  data = dat,
  method = "REML"       
)
summary(full_model)

#########################

dat1 <- read.csv2("dbpfulltest.csv")
dat1$diabetes <- factor(dat1$diabetes, levels = c("n","y"))
dat1$eod <- factor(dat1$eod, levels = c("n","y"))
dat1$bpmeasurement <- factor(dat1$bpmeasurement,levels = c("Office","24H"))

fulldbp_formula <- 
  yi ~  eod + diabetes + bpmeasurement + base_dbp

fulldbp_model <- rma.uni(
  fulldbp_formula, 
  sei = vi, 
  data = dat1,
  method = "REML"       
)
summary(fulldbp_model)

########################################
########################################
# Scales for priors in Bayesian        #
########################################
########################################

permissive     (5/qt(0.975,df=3)) # Sigma = 1.571118
moderate       (4/qt(0.975,df=3)) # Sigma = 1.256895
strict        (3/qt(0.975,df=3))  # Sigma = 0.942671

                                

# We don't actually set run these.
# Layman terms:"What is the scale /sigma, if we
# want 95% of our data to fall within +- n 
# (2/3/4) from the null-effect (mu)." 

########################################
########################################
# BAYESIAN   (Mild bias adjustment)    #
########################################
########################################


sbp_bayes_permissive <- brm(
  yi | se(vi) ~ eod + diabetes + bpmeasurement + base_sbp + (1 | trial),
  data   = dat,
  prior  = c(
    prior(student_t(3, 0, 1),       class = "sd"),
    prior(student_t(3 ,0 ,10),      class = "Intercept"),
    prior(student_t(3 ,0 ,1.571118),    class = "b", coef = "eody"),
    prior(student_t(3 ,0 ,1.571118),    class = "b", coef = "diabetesy"),
    prior(student_t(3 ,0 ,1.571118),    class = "b", coef = "bpmeasurement24H"),
    prior(student_t(3 ,0 ,1.571118),    class = "b", coef = "base_sbp")
  ),
  iter = 20000,
  warmup = 10000,
  control = list(adapt_delta = 0.999),
  cores = 4,
  seed = 123
)
summary(sbp_bayes_permissive)  # BAYESIAN FOR SBP
summary(full_model) # FREQUENTIST FOR SBP



dbp_bayes_permissive <- brm(
  yi | se(vi) ~ eod + diabetes + bpmeasurement + base_dbp + (1 | trial),
  data   = dat1,
  prior  = c(
    prior(student_t(3, 0, 1),       class = "sd"),
    prior(student_t(3 ,0, 10),              class = "Intercept"),
    prior(student_t(3 ,0 ,1.571118),    class = "b", coef = "eody"),
    prior(student_t(3 ,0, 1.571118),    class = "b", coef = "diabetesy"),
    prior(student_t(3 ,0 ,1.571118),    class = "b", coef = "bpmeasurement24H"),
    prior(student_t(3 ,0 ,1.571118),    class = "b", coef = "base_dbp")
  ),
  iter = 20000,
  warmup = 10000,
  control = list(adapt_delta = 0.999),
  cores = 4,
  seed = 123
)
summary(dbp_bayes_permissive)     # BAYESIAN FOR DBP
summary(fulldbp_model) # FREQUENTIST FOR DBP

##########################################################
##########################################################
# BAYESIAN MODELS                                        #
##########################################################
##########################################################

sbp_bayes_moderate <- brm(
  yi | se(vi) ~ eod + diabetes + bpmeasurement + base_sbp + (1 | trial),
  data   = dat,
  prior  = c(
    prior(student_t(3, 0, 1),       class = "sd"),
    prior(student_t(3 ,0, 10),      class = "Intercept"),
    prior(student_t(3 ,0, 1.256895),    class = "b", coef = "eody"),
    prior(student_t(3 ,0, 1.256895),    class = "b", coef = "diabetesy"),
    prior(student_t(3 ,0, 1.256895),    class = "b", coef = "bpmeasurement24H"),
    prior(student_t(3 ,0, 1.256895),    class = "b", coef = "base_sbp")
  ),
  iter = 20000,
  warmup = 10000,
  control = list(adapt_delta = 0.999),
  cores = 4,
  seed = 123
)
summary(sbp_bayes_moderate)  # BAYESIAN FOR SBP
summary(full_model) # FREQUENTIST FOR SBP


dbp_bayes_moderate <- brm(
  yi | se(vi) ~ eod + diabetes + bpmeasurement + base_dbp + (1 | trial),
  data   = dat1,
  prior  = c(
    prior(student_t(3 ,0, 1),       class = "sd"),
    prior(student_t(3 ,0, 10),              class = "Intercept"),
    prior(student_t(3 ,0, 1.256895),    class = "b", coef = "eody"),
    prior(student_t(3 ,0, 1.256895),    class = "b", coef = "diabetesy"),
    prior(student_t(3 ,0, 1.256895),    class = "b", coef = "bpmeasurement24H"),
    prior(student_t(3 ,0, 1.256895),    class = "b", coef = "base_dbp")
  ),
  iter = 20000,
  warmup = 10000,
  control = list(adapt_delta = 0.999),
  cores = 4,
  seed = 123
)
summary(dbp_bayes_moderate)     # BAYESIAN FOR DBP
summary(fulldbp_model) # FREQUENTIST FOR DBP

##########################################################
##########################################################
# BAYESIAN MODELS (Effect modifier discounted 100%)      #
# Layman terms: "The effect modifier is not real. It is  #
#             solely due to bias. We believe the effect  #
#             is 0. If there is an effect, the data must #
#             convince us."                             #
##########################################################
##########################################################

sbp_bayes_strict <- brm(
  yi | se(vi) ~ eod + diabetes + bpmeasurement + base_sbp + (1 | trial),
  data   = dat,
  prior  = c(
    prior(student_t(3, 0, 1),       class = "sd"),
    prior(student_t(3 ,0, 10),      class = "Intercept"),
    prior(student_t(3 ,0, 0.942671),    class = "b", coef = "eody"),
    prior(student_t(3 ,0, 0.942671),    class = "b", coef = "diabetesy"),
    prior(student_t(3 ,0, 0.942671),    class = "b", coef = "bpmeasurement24H"),
    prior(student_t(3 ,0, 0.942671),    class = "b", coef = "base_sbp")
  ),
  iter = 20000,
  warmup = 10000,
  control = list(adapt_delta = 0.999),
  cores = 4,
  seed = 123
)
summary(sbp_bayes_strict)  # BAYESIAN FOR SBP
summary(full_model) # FREQUENTIST FOR SBP


dbp_bayes_strict <- brm(
  yi | se(vi) ~ eod + diabetes + bpmeasurement + base_dbp + (1 | trial),
  data   = dat1,
  prior  = c(
    prior(student_t(3, 0, 1),       class = "sd"),
    prior(student_t(3 ,0, 10),      class = "Intercept"),
    prior(student_t(3 ,0, 0.942671),    class = "b", coef = "eody"),
    prior(student_t(3 ,0, 0.942671),    class = "b", coef = "diabetesy"),
    prior(student_t(3 ,0, 0.942671),    class = "b", coef = "bpmeasurement24H"),
    prior(student_t(3 ,0, 0.942671),    class = "b", coef = "base_dbp")
  ),
  iter = 20000,
  warmup = 10000,
  control = list(adapt_delta = 0.999),
  cores = 4,
  seed = 123
)
summary(dbp_bayes_strict)     # BAYESIAN FOR DBP
summary(fulldbp_model) # FREQUENTIST FOR DBP

########################################
########################################
# Comparing models                     #
########################################
########################################


summary(full_model)                      # FREQUENTIST FOR SBP 
summary(sbp_bayes_permissive)            # BAYESIAN for SBP (no adjust.)
summary(sbp_bayes_moderate)              # BAYESIAN for SBP (minor adjust.)
summary(sbp_bayes_strict)                # BAYESIAN for SBP (major adjust.)

summary(fulldbp_model)                   # FREQUENTIST FOR DBP
summary(dbp_bayes_permissive)            # BAYESIAN for DBP (no adjust.)
summary(dbp_bayes_moderate)              # BAYESIAN for DBP (minor adjust.)
summary(dbp_bayes_strict)                # BAYESIAN for DBP (major adjust.)

#####################################################
#####################################################
# Calculating R^2  pre-requisites                  #
#####################################################
#####################################################

unconditional_sbp_bayes <- brm(
  yi | se(vi) ~ 1 + (1 | trial),
  data   = dat,
  prior  = c(
    prior(student_t(3, 0, 1),       class = "sd")
  ),
  iter = 20000,
  warmup = 10000,
  control = list(adapt_delta = 0.999),
  cores = 4,
  seed = 123
)
summary(unconditional_sbp_bayes)  # BAYESIAN FOR SBP


unconditional_dbp_bayes <- brm(
  yi | se(vi) ~ 1 + (1 | trial),
  data   = dat1,
  prior  = c(
    prior(student_t(3, 0, 1),       class = "sd")
  ),
  iter = 20000,
  warmup = 10000,
  control = list(adapt_delta = 0.999),
  cores = 4,
  seed = 123
)
summary(unconditional_dbp_bayes)     # BAYESIAN FOR DBP

#####################################################
#####################################################
# The actual R^2 results                            #
#####################################################
#####################################################


tau2_unconditional_sbp <- VarCorr(unconditional_sbp_bayes)$trial$sd[1]^2
tau2_conditional_sbp <- VarCorr(sbp_bayes)$trial$sd[1]^2
R2_sbp <- ((tau2_unconditional_sbp - tau2_conditional_sbp) 
/ tau2_unconditional_sbp)

print(R2_sbp*100) # RESULT FOR SBP IN %

tau2_unconditional_dbp <- VarCorr(unconditional_dbp_bayes)$trial$sd[1]^2
tau2_conditional_dbp <- VarCorr(dbp_bayes)$trial$sd[1]^2
R2_dbp <- ((tau2_unconditional_dbp - tau2_conditional_dbp) / 
  tau2_unconditional_dbp)

print(R2_dbp*100)  # RESULT FOR DBP IN %

########################################
########################################
# Diagnostics   (Bayesian)             #
########################################
########################################

plot(sbp_bayes_mild)                      # Change to any model
mcmc_plot(sbp_bayes_mild, type="hist")
mcmc_plot(sbp_bayes_mild, type="trace")
mcmc_plot(sbp_bayes_mild, type="acf")



##### EXPLANATION:#####################
#sbp_bayes <- brm(
#  yi | se(vi) ~ eod + diabetes + bpmeasurement + base_sbp + (1 | trial),
#     #This part is called the "formula", telling the model which moderators
#     # we want to test. The last part explains it is a random-effects model.
#
#  data   = dat,
#  prior  = c(
#    prior(student_t(3, 0, 1),       class = "sd"), #The SD may vary.
#    prior(student_t(3 ,-4.07 ,2),      class = "Intercept"), 
#    prior(student_t(3 ,0.396 ,0.7),    class = "b", coef = "eody"),
#    prior(student_t(3 ,-1.76 ,0.7),    class = "b", coef = "diabetesy"),
#    prior(student_t(3 ,-1.70 ,0.7),    class = "b", coef = "bpmeasurement24H"),
#    prior(student_t(3 ,0.02 ,0.7),    class = "b", coef = "base_sbp")
#  ),
#   ### Here we use the frequentist model mu (the middle number) to set a base
#   ### In the adjusted models, we are "sceptical", by discounting the 
#   ### observed effect by 50% and 100%. Saying "bias doubles the effect"
#   ### or "bias is the only reason for an effect" (null-effect). 
#
#  iter = 20000, #how many times to measure
#  warmup = 10000, #how many times the initial measurements should be removed
#                 # imagine it as a "tutorial" on how to measure. Initial
#                 #measurements are prone to "mistakes", therefore we discard.
#  control = list(adapt_delta = 0.95),
#                 # "how precise should our measurements vary by"
#                 # e.g. should we take a LARGE step, or a SMALLER?
#                 # keep in mind, the step-size is inverse of adapt_delta
#  cores = 4,     # How many CPU cores should do this job?
#  seed = 123     # For reproducibility. Can be any number, as long as 
#                 # the future analyst uses the same seed.
###########################################################################
