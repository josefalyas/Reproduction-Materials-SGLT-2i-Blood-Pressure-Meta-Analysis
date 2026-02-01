library("meta")
library("metafor")
library("RoBMA")


all_data <-read.csv2("Rmetaanalysis.csv")

diabetes_y <- all_data[all_data$diabetes == "y", ]
diabetes_n <- all_data[all_data$diabetes == "n", ]
eod_y <- all_data[all_data$eod == "y", ]
eod_n <- all_data[all_data$eod == "n", ]

##################################################
# CHECK AGAINST RevMan via DerSimonian and Laird #
##################################################

### DIABETES

DL_sbp_diabetes_y <- rma.uni(
  yi = mdsbp,
  sei = sesbp,
  data = diabetes_y,
  method = "DL",
  test = "z",
  weighted = TRUE
)
summary(DL_sbp_diabetes_y)

DL_dbp_diabetes_y <- rma.uni(
  yi = mddbp,
  sei = sedbp,
  data = diabetes_y,
  method = "DL",
  test = "z",
  weighted = TRUE
)
summary(DL_dbp_diabetes_y)

DL_sbp_diabetes_n <- rma.uni(
  yi = mdsbp,
  sei = sesbp,
  data = diabetes_n,
  method = "DL",
  test = "z",
  weighted = TRUE
)
summary(DL_sbp_diabetes_n)

DL_dbp_diabetes_n <- rma.uni(
  yi = mddbp,
  sei = sedbp,
  data = diabetes_n,
  method = "DL",
  test = "z",
  weighted = TRUE
)
summary(DL_dbp_diabetes_n)



### End-Organ Damage


DL_sbp_eod_y <- rma.uni(
  yi = mdsbp,
  sei = sesbp,
  data = eod_y,
  method = "DL",
  test = "z",
  weighted = TRUE
)
summary(DL_sbp_eod_y)

DL_dbp_eod_y <- rma.uni(
  yi = mddbp,
  sei = sedbp,
  data = eod_y,
  method = "DL",
  test = "z",
  weighted = TRUE
)
summary(DL_dbp_eod_y)

DL_sbp_eod_n <- rma.uni(
  yi = mdsbp,
  sei = sesbp,
  data = eod_n,
  method = "DL",
  test = "z",
  weighted = TRUE
)
summary(DL_sbp_eod_n)

DL_dbp_eod_n <- rma.uni(
  yi = mddbp,
  sei = sedbp,
  data = eod_n,
  method = "DL",
  test = "z",
  weighted = TRUE
)
summary(DL_dbp_eod_n)

######################################################
######################################################
#                                                    #
# REML MODEL                                         #
#                                                    #
######################################################
######################################################
######################################################

### DIABETES

REML_sbp_diabetes_y <- rma.uni(
  yi = mdsbp,
  sei = sesbp,
  data = diabetes_y,
  method = "REML",
  test = "knha",
  weighted = TRUE
)
summary(REML_sbp_diabetes_y)

REML_dbp_diabetes_y <- rma.uni(
  yi = mddbp,
  sei = sedbp,
  data = diabetes_y,
  method = "REML",
  test = "knha",
  weighted = TRUE
)
summary(REML_dbp_diabetes_y)

REML_sbp_diabetes_n <- rma.uni(
  yi = mdsbp,
  sei = sesbp,
  data = diabetes_n,
  method = "REML",
  test = "knha",
  weighted = TRUE
)
summary(REML_sbp_diabetes_n)

REML_dbp_diabetes_n <- rma.uni(
  yi = mddbp,
  sei = sedbp,
  data = diabetes_n,
  method = "REML",
  test = "knha",
  weighted = TRUE
)
summary(REML_dbp_diabetes_n)



### End-Organ Damage


REML_sbp_eod_y <- rma.uni(
  yi = mdsbp,
  sei = sesbp,
  data = eod_y,
  method = "REML",
  test = "knha",
  weighted = TRUE
)
summary(REML_sbp_eod_y)

REML_dbp_eod_y <- rma.uni(
  yi = mddbp,
  sei = sedbp,
  data = eod_y,
  method = "REML",
  test = "knha",
  weighted = TRUE
)
summary(REML_dbp_eod_y)

REML_sbp_eod_n <- rma.uni(
  yi = mdsbp,
  sei = sesbp,
  data = eod_n,
  method = "REML",
  test = "knha",
  weighted = TRUE
)
summary(REML_sbp_eod_n)

REML_dbp_eod_n <- rma.uni(
  yi = mddbp,
  sei = sedbp,
  data = eod_n,
  method = "REML",
  test = "knha",
  weighted = TRUE
)
summary(REML_dbp_eod_n)

################################################
################################################
################################################
#                                              #
# PREDICTION INTERVALS REML                    #
#                                              #
################################################
################################################
################################################

predict(REML_sbp_diabetes_y)
predict(REML_dbp_diabetes_y)
predict(REML_sbp_diabetes_n)
predict(REML_dbp_diabetes_n)

predict(REML_sbp_eod_y)
predict(REML_dbp_eod_y)
predict(REML_sbp_eod_n)
predict(REML_dbp_eod_n)

################################################
################################################
################################################
#                                              #
# EGGERS REGRESSION TEST                       #
#                                              #
################################################
################################################
################################################

regtest(DL_sbp_diabetes_y)
regtest(DL_dbp_diabetes_y)
regtest(DL_sbp_diabetes_n)
regtest(DL_dbp_diabetes_n)

regtest(DL_sbp_eod_y)
regtest(DL_dbp_eod_y)
regtest(DL_sbp_eod_n)
regtest(DL_dbp_eod_n)

################################################
################################################
################################################
#                                              #
# FUNNEL PLOTS COMBINED                        #
#                                              #
################################################
################################################
################################################

sbp_diabetes_combined <- rma.uni(
  yi = mdsbp,
  sei = sesbp,
  mods = ~ factor(diabetes),
  data = all_data,
  method = "DL",
  test = "z"
)
summary(sbp_diabetes_combined)

dbp_diabetes_combined <- rma.uni(
  yi = mddbp,
  sei = sedbp,
  mods = ~ factor(diabetes),
  data = all_data,
  method = "DL",
  test = "z"
)
summary(dbp_diabetes_combined)

sbp_eod_combined <- rma.uni(
  yi = mdsbp,
  sei = sesbp,
  mods = ~ factor(eod),
  data = all_data,
  method = "DL",
  test = "z"
)
summary(sbp_eod_combined)

dbp_eod_combined <- rma.uni(
  yi = mddbp,
  sei = sedbp,
  mods = ~ factor(eod),
  data = all_data,
  method = "DL",
  test = "z"
)
summary(dbp_eod_combined)

############################################################
############################################################
###### Normal Funnel Plots (imputed)                       #
############################################################
############################################################
metafor::funnel(
  trimfill_sbp_eod_y, 
  digits=c(0,1),
  xlab="Observed modification of SBP (mmHg)",
  ylab="Standard error",
  col=("#000000"),
  back=("#B4B4B4"),
  level=c(95)
)

metafor::funnel(
  trimfill_dbp_eod_y, 
  digits=c(0,1),
  xlab="Observed modification of SBP (mmHg)",
  ylab="Standard error",
  col=("#000000"),
  back=("#B4B4B4"),
  level=c(95)
)

metafor::funnel(
  DL_sbp_eod_n, 
  digits=c(0,1),
  xlab="Observed modification of SBP (mmHg)",
  ylab="Standard error",
  col=("#000000"),
  back=("#B4B4B4"),
  level=c(95)
)

metafor::funnel(
  trimfill_dbp_eod_n, 
  digits=c(0,1),
  xlab="Observed modification of SBP (mmHg)",
  ylab="Standard error",
  col=("#000000"),
  back=("#B4B4B4"),
  level=c(95)
)

metafor::funnel(
  trimfill_sbp_diabetes_y, 
  digits=c(0,1),
  xlab="Observed modification of SBP (mmHg)",
  ylab="Standard error",
  col=("#000000"),
  back=("#B4B4B4"),
  level=c(95)
)

metafor::funnel(
  trimfill_dbp_diabetes_y, 
  digits=c(0,1),
  xlab="Observed modification of SBP (mmHg)",
  ylab="Standard error",
  col=("#000000"),
  back=("#B4B4B4"),
  level=c(95)
)

metafor::funnel(
  trimfill_sbp_diabetes_n, 
  digits=c(0,1),
  xlab="Observed modification of SBP (mmHg)",
  ylab="Standard error",
  col=("#000000"),
  back=("#B4B4B4"),
  level=c(95)
)

metafor::funnel(
  trimfill_sbp_diabetes_n, 
  digits=c(0,1),
  xlab="Observed modification of SBP (mmHg)",
  ylab="Standard error",
  col=("#000000"),
  back=("#B4B4B4"),
  level=c(95)
)

############################################################
############################################################
###### Trim-and-fill models                                #
############################################################
############################################################

trimfill_sbp_diabetes_y <-trimfill(DL_sbp_diabetes_y)
trimfill_dbp_diabetes_y <-trimfill(DL_dbp_diabetes_y)
trimfill_sbp_diabetes_n <-trimfill(DL_sbp_diabetes_n)
trimfill_dbp_diabetes_n <-trimfill(DL_dbp_diabetes_n)

trimfill_sbp_eod_y <-trimfill(DL_sbp_eod_y)
trimfill_dbp_eod_y <-trimfill(DL_dbp_eod_y)
trimfill_sbp_eod_n <-trimfill(DL_sbp_eod_n) # cannot converge 
trimfill_dbp_eod_n <-trimfill(DL_dbp_eod_n)

############################################################
############################################################
###### Trim-and-fill vs. observed effect                   #
############################################################
############################################################


##### Diabetes
print(DL_sbp_diabetes_y) # Missing n=11
print(trimfill_sbp_diabetes_y)

print(DL_dbp_diabetes_y) # Missing n=5
print(trimfill_dbp_diabetes_y)

print(DL_sbp_diabetes_n) # Missing n=3
print(trimfill_sbp_diabetes_n)

print(DL_dbp_diabetes_n)
print(trimfill_dbp_diabetes_n) # Missing n=1

###### End-organ damage

print(DL_sbp_eod_y) # Missing n=2
print(trimfill_sbp_eod_y) 

print(DL_dbp_eod_y)
print(trimfill_dbp_eod_y) # Missing n=0

print(DL_sbp_eod_n) # CANNOT CONVERGE

print(DL_dbp_eod_n) # Missing n=3
print(trimfill_dbp_eod_n)


############################################################
############################################################
###### Robust Bayesian Model (RoBMA)                       #
############################################################
############################################################

############################################################
# DIABETES
############################################################


RoBMA_sbp_diabetes_y <- RoBMA(
  y = diabetes_y$mdsbp,
  se = diabetes_y$sesbp,
  effect_direction = "negative",
  parallel = TRUE
)

RoBMA_dbp_diabetes_y <- RoBMA(
  y = diabetes_y$mddbp[!is.na(diabetes_y$mddbp) & !is.na(diabetes_y$sedbp)],
  se = diabetes_y$sedbp[!is.na(diabetes_y$mddbp) & !is.na(diabetes_y$sedbp)],
  effect_direction = "negative",
  parallel = TRUE
)

RoBMA_sbp_diabetes_n <- RoBMA(
  y = diabetes_n$mdsbp,
  se = diabetes_n$sesbp,
  effect_direction = "negative",
  parallel = TRUE
)

RoBMA_dbp_diabetes_n <- RoBMA(
  y = diabetes_n$mddbp[!is.na(diabetes_n$mddbp) & !is.na(diabetes_n$sedbp)],
  se = diabetes_n$sedbp[!is.na(diabetes_n$mddbp) & !is.na(diabetes_n$sedbp)],
  effect_direction = "negative",
  parallel = TRUE
)

############################################################
# End-organ damage
############################################################

RoBMA_sbp_eod_y <- RoBMA(
  y = eod_y$mdsbp,
  se = eod_y$sesbp,
  effect_direction = "negative",
  parallel = TRUE
)

RoBMA_dbp_eod_y <- RoBMA(
  y = eod_y$mddbp[!is.na(eod_y$mddbp) & !is.na(eod_y$sedbp)],
  se = eod_y$sedbp[!is.na(eod_y$mddbp) & !is.na(eod_y$sedbp)],
  effect_direction = "negative",
  parallel = TRUE,
)

RoBMA_sbp_eod_n <- RoBMA(
  y = eod_n$mdsbp,
  se = eod_n$sesbp,
  effect_direction = "negative",
  parallel = TRUE
)

RoBMA_dbp_eod_n <- RoBMA(
  y = eod_n$mddbp[!is.na(eod_n$mddbp) & !is.na(eod_n$sedbp)],
  se = eod_n$sedbp[!is.na(eod_n$mddbp) & !is.na(eod_n$sedbp)],
  effect_direction = "negative",
  parallel = TRUE
)

############################################################
# Summary RoBMA vs. Unadjusted frequentist DL
############################################################
summary(RoBMA_sbp_diabetes_y) # MD = -2,54
summary(DL_sbp_diabetes_y) # MD = -3,66

summary(RoBMA_dbp_diabetes_y) # MD = -0,73
summary(DL_dbp_diabetes_y) # MD = -1,33

summary(RoBMA_sbp_diabetes_n) # MD = -0,45 NS!
summary(DL_sbp_diabetes_n) # MD = -1,79

summary(RoBMA_dbp_diabetes_n) # MD = -0.41 NS!
summary(DL_dbp_diabetes_n) # MD = -1,46 NS!

##### End-organ damage

summary(RoBMA_sbp_eod_y) # MD = -1,23 NS!
summary(DL_sbp_eod_y) # MD = -2,50

summary(RoBMA_dbp_eod_y) # MD = -0,941 
summary(DL_dbp_eod_y) # MD = -1,03

summary(RoBMA_sbp_eod_n) # MD = -2,24
summary(DL_sbp_eod_n) # MD = -4,19

summary(RoBMA_dbp_eod_n) # MD = -0,79
summary(DL_dbp_eod_n) # MD = 1,64

############################################################
# END OF DOCUMENT                                          #
############################################################
