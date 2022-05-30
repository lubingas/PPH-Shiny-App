# load functions we need
source("sub_routines.R")

pboptions(
  type = "shiny",
  title = "Performing Sensitivity",
  label = "Be patient ...")

# some preliminary stuff
# these are not varied in sensitivity analyses
interventions <- c("Misoprostol", "No Misoprostol")

# get the data we need
source("data_input.R")

TxTrjList <- list("Hospital", "HC", "Friend", "TBA", "Unassisted")
names(TxTrjList) <- c("Hospital", "HC", "Friend", "TBA", "Unassisted")

QuntileList <- list("Lowest", "Second", "Middle", "Fourth", "Highest")
names(QuntileList) <- c("Lowest", "Second", "Middle", "Fourth", "Highest")

dir_inputs <-c("Quintiles", "AgeDistribution")

# put outcomes in a list and run DSA and PSA on all, remember to add names so they are identifiable in the returned list
outcomes = list("Incidence", "Mortality", "DALYs")
names(outcomes) = c("Incidence", "Mortality", "DALYs")


# pull out inputs for the main model but separate out the transition probabilities
baseinputs <- data %>%
  filter(!is.na(variable) & dist != "dirichlet") %>%   # remove NA from data
  pull(basecase, variable)

# process dirichlet inputs so I can input set of high and low values at the same time (also add label, so it is easy to reference in functions)
basetransitions <- data %>%
  filter(!is.na(variable) & dist == "dirichlet") %>%   # remove NA from data
  pull(basecase, variable)


## ONE WAY SENSITIVITY ANALYSES
# parameters
usa.low <- data %>%
  filter(!is.na(low) & dist != "dirichlet") %>%   # remove NA from data
  pull(low, variable)

usa.high <- data %>%
  filter(!is.na(low) & dist != "dirichlet") %>%   # remove NA from data
  pull(high, variable)

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






