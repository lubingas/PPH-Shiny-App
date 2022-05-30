
# load data
data = read_excel("pph_data_input.xlsx", sheet = 1)

# colors
col <- brewer.pal(6,"Dark2")

## general parameters
# discounts
discFact <- 0.03
procDaysYr <- 264
procHrsDay <- 8
age <- c(18, 27, 42)
LifeExp <- 63

# parameters for misoprostol program costs
nWomen <- 10000
nHW <- 200
daysTraining <- 5
cTrain <- 10 #US$/day
nurseFTE <- 20
nNurse <- 50
cPackage <- 0.5 # 50% of cost of misoprostol


## age distribution of women
# proportions = c(0.167306502,0.6974613,0.135232198)








