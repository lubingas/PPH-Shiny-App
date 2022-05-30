# place all files in the same working directory and change to this directory 
setwd("../PPH-Shiny-App/pph_model")

# load functions we need
source("sub_routines.R")

# some preliminary stuff
# these are not varied in sensitivity analyses
interventions <- c("Misoprostol", "No Misoprostol")

source("data_input.R")

# run basecase model
model <- RunModel(baseparms = baseinputs, 
                  basetransitions = basetransitions)

# # iterate over variables and perform OWSA, function returns the table and plot with max_vars variable
dsa_all <- lapply(outcomes, FUN = owsa,
                  model = model,
                  low_base = usa.low, low_transitions = dir_lowinputs,
                  high_base = usa.high, high_transitions = dir_highinputs,
                  max_vars = 20)
# 
# dsa_all

## PSA
# how many simulations? In furure, either wrap into function or pregenerate the data in the data_input file
nsims <- 5000

# first identify fixed parameters
fixedparms <- data %>%
  filter(dist == "fixed") %>%
  pull(variable)

betaparms <- data %>%
  filter(dist == "beta") %>%
  pull(variable)

tnormparms <- data %>%
  filter(dist == "truncnormal") %>%
  pull(variable)

unifparms <- data %>%
  filter(dist == "uniform") %>%
  pull(variable)

lognormparms <- data %>%
  filter(dist == "lognormal") %>%
  pull(variable)

alphas <- data %>%
  pull(alpha, variable)

betas <- data %>%
  pull(beta, variable)

sems <- data %>%
  pull(sem, variable)

# draw values
#betas
randbeta <- mapply(rbeta, MoreArgs=list(n=nsims), alphas[betaparms], betas[betaparms])

# normals
randtnormal <- mapply(rtruncnorm, MoreArgs=list(n=nsims), 
                      mean = baseinputs[tnormparms], sd = sems[tnormparms], 
                      a = usa.low[tnormparms], b = usa.high[tnormparms])

# durations (uniformly distributed)
randunif <- mapply(runif, MoreArgs=list(n=nsims), usa.low[unifparms], usa.high[unifparms])

# log-normals
randlognormal <- mapply(rlnorm, MoreArgs=list(n=nsims), 
                        meanlog = log(baseinputs[lognormparms]), 
                        sdlog = sems[lognormparms])

# random dirichlet transitions
randdirchinputs <- data %>%
  filter(dist == "dirichlet") %>%   # remove NA from data
  pull(alpha, variable) %>%
  split(., rep(1:2, c(5, 3)))
names(randdirchinputs) <- c("Quintiles", "AgeDistribution")


# generate variates and put in one database
randdirichlets <- lapply(randdirchinputs, vdirichlet, n=nsims) %>%
  do.call("cbind", .)

fixedinputs <- matrix(rep(baseinputs[fixedparms],each=nsims),nrow=nsims) %>%
  magrittr::set_colnames(., fixedparms)

# pool together into one dataframe
psa_input <- data.frame(cbind(randbeta, randtnormal, randunif, randlognormal, randdirichlets, fixedinputs))

# run on one outcome
# psaDALYs <- RunPSA(model = model, psalist = psa_input, outcome = "DALYs", wtp = 2000, by = 100)

# run PSA on all outcomes
psa_all <- lapply(outcomes, FUN = RunPSA, model = model, psalist = psa_input, wtp = 2000, by = 100)


