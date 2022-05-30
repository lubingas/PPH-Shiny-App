# switch off file writing if in use
if(sink.number()>0) sink()

rm(list=ls())

# define function for detaching variables
detachAllData <- function()  {
  pos.to.detach <- (1:length(search()))[substring(search(), 
                                                  first = 1, last = 8) != "package:" & search() != ".GlobalEnv" & 
                                          search() != "Autoloads" & search() != "CheckExEnv" & search() != "tools:rstudio" & search() != "TempEnv"]
  for (i in 1:length(pos.to.detach)) {
    if (length(pos.to.detach) > 0) {
      detach(pos = pos.to.detach[1])
      pos.to.detach <- (1:length(search()))[substring(search(), 
                                                      first = 1, last = 8) != "package:" & search() != 
                                              ".GlobalEnv" & search() != "Autoloads" & search() != 
                                              "CheckExEnv" & search() != "tools:rstudio" & 
                                              search() != "TempEnv"]
    }
  }
}

# run detach function
detachAllData()

library(tidyverse)
library(readxl)
library(plotly)
library(RColorBrewer)
library(truncnorm)
library(shiny)
library(shinyjs)
library(shinythemes)
library(pbapply)
#library(data.table)

# function calculates care path depending on wealth quintile
# Intervention = TRUE will apply the treatment effects (in this function on odds of HF delivery)
getCarePath <- function(Quintile, Intervention = TRUE) {
  
  # beta1 <- as.list(data)
  # attach(beta1)
  
  orHFDelivery*pHfDelLowQ/(1-pHfDelLowQ+(orHFDelivery*pHfDelLowQ))
  
  if (Quintile == "Lowest") {
    
    pHfDel <- pHfDelLowQ
    pHospDel <- pHospDelLowQ
    pUnassDel <- pUnassDelLowQ
    pFriendRelDel <- pFriendRelDelLowQ
    
  } else if (Quintile == "Second") {
    
    pHfDel <- pHfDelSecQ
    pHospDel <- pHospDelSecQ
    pUnassDel <- pUnassDelSecQ
    pFriendRelDel <- pFriendRelDelSecQ
    
  } else if (Quintile == "Middle") {
    
    pHfDel <- pHfDelMidQ
    pHospDel <- pHospDelMidQ
    pUnassDel <- pUnassDelMidQ
    pFriendRelDel <- pFriendRelDelMidQ
    
  } else if (Quintile == "Fourth") {
    
    pHfDel <- pHfDelFourQ
    pHospDel <- pHospDelFourQ
    pUnassDel <- pUnassDelFourQ
    pFriendRelDel <- pFriendRelDelFourQ
    
  } else if (Quintile == "Highest") {
    
    pHfDel <- pHfDelHighQ
    pHospDel <- pHospDelHighQ
    pUnassDel <- pUnassDelHighQ
    pFriendRelDel <- pFriendRelDelHighQ
    
  }
  
  # computations to make the probabilities work
  pUnassDel <- pUnassDel/(1-pHfDel)
  pFriendRelDel <- pFriendRelDel/(1-pUnassDel)
  
  # apply treatment effect: intervention increases the odds of HF delivery
  pHfDel <- pHfDel * (Intervention==FALSE) + orHFDelivery*pHfDel/(1-pHfDel+(orHFDelivery*pHfDel)) * (Intervention==TRUE)
  
  # calculate product of path probabilities
  hosp   <- prod(pHfDel, pHospDel)
  hc     <- prod(pHfDel, (1-pHospDel))
  friend <- prod((1-pHfDel), (1-pUnassDel), pFriendRelDel)
  TBA    <- prod((1-pHfDel), (1-pUnassDel), (1-pFriendRelDel))
  unass  <- prod((1-pHfDel), pUnassDel)
  
  
  # # don't forget to detach
  # detach(beta1)
  
  return(c(hospital = hosp, hc = hc, friend = friend, TBA = TBA, unass = unass))
  
}


# function calculates care path: 32 different possibilities per care pathway (5 care pathways)
# Intervention = TRUE will apply the treatment effects, higher access to miso with intervention
getTxPath <- function(Where, Intervention = TRUE) {
  
  # beta1 <- as.list(data)
  # attach(beta1)
  
  if (Where == "Hospital") {
    
    pOxy <- pOxyHosp
    pAccessEmOC <- pAccessEmOCHosp
    pMisoAccess <- pMisoAccessHospMisoPolicy * (Intervention==TRUE) + pMisoAccessHospOxyPolicy * (Intervention==FALSE)

    # hospital deliveries, apply pHospPPH
    pPPHOxy  <- pHospPPH*rPPHOxyPlacebo
    pPPHMiso <- pHospPPH*rPPHMisoHF
    pPPH     <- pHospPPH
    
  } else if (Where == "HC") {
    
    pOxy <- pOxyHC
    pAccessEmOC <- pAccessEmOCHC
    pMisoAccess <- pMisoAccessHCMisoPolicy * (Intervention==TRUE) + pMisoAccessHCOxyPolicy * (Intervention==FALSE)

    # hospital deliveries, apply pHospPPH
    pPPHOxy  <- pHospPPH*rPPHOxyPlacebo
    pPPHMiso <- pHospPPH*rPPHMisoHF
    pPPH     <- pHospPPH
    
  } else if (Where == "Friend") {
    
    pOxy <- pOxyHomeFriend
    pAccessEmOC <- pAccessEmOCFriend
    pMisoAccess <- pMisoAccessHomeFriendMisoPolicy * (Intervention==TRUE) + pMisoAccessHomeOxyPolicy * (Intervention==FALSE)

    # home deliveries, apply pHomePPH
    pPPHOxy  <- pHomePPH*rPPHOxyPlacebo
    pPPHMiso <- pHomePPH*rPPHMisoOxy
    pPPH     <- pHomePPH
    
  } else if (Where == "TBA") {
    
    pOxy <- pOxyHomeTBA
    pAccessEmOC <- pAccessEmOCTBA
    pMisoAccess <- pMisoAccessHomeTBAMisoPolicy * (Intervention==TRUE) + pMisoAccessHomeOxyPolicy * (Intervention==FALSE)
    
    # home deliveries, apply pHomePPH
    pPPHOxy  <- pHomePPH*rPPHOxyPlacebo
    pPPHMiso <- pHomePPH*rPPHMisoOxy
    pPPH     <- pHomePPH
    
  } else if (Where == "Unassisted") {
    
    pOxy <- pOxyHomeUnass
    pAccessEmOC <- pAccessEmOCUnass
    pMisoAccess <- pMisoAccessHomeUnassMisoPolicy * (Intervention==TRUE) + pMisoAccessHomeOxyPolicy * (Intervention==FALSE)
    
    # home deliveries, apply pHomePPH
    pPPHOxy  <- pHomePPH*rPPHOxyPlacebo
    pPPHMiso <- pHomePPH*rPPHMisoOxy
    pPPH     <- pHomePPH
    
  }
  
  # calculate individual paths
  pathlist <- c(
    path01 <- prod(pOxy, pPPHOxy, pAccessEmOC, pDieEmOC),
    path02 <- prod(pOxy, pPPHOxy, pAccessEmOC, (1-pDieEmOC)),
    path03 <- prod(pOxy, pPPHOxy, (1-pAccessEmOC), pDieEmOC),
    path04 <- prod(pOxy, pPPHOxy, (1-pAccessEmOC), (1-pDieEmOC)),
    path05 <- prod(pOxy, (1-pPPHOxy), pDieNoPPH),
    path06 <- prod(pOxy, (1-pPPHOxy), (1-pDieNoPPH)),
    path07 <- prod((1-pOxy), pMisoAccess, (1-pMisuseMiso), pPPHMiso, pAccessEmOC, pDieEmOC),
    path08 <- prod((1-pOxy), pMisoAccess, (1-pMisuseMiso), pPPHMiso, pAccessEmOC, (1-pDieEmOC)),
    path09 <- prod((1-pOxy), pMisoAccess, (1-pMisuseMiso), pPPHMiso, (1-pAccessEmOC), pDieEmOC),
    path10 <- prod((1-pOxy), pMisoAccess, (1-pMisuseMiso), pPPHMiso, (1-pAccessEmOC), (1-pDieEmOC)),
    path11 <- prod((1-pOxy), pMisoAccess, (1-pMisuseMiso), (1-pPPHMiso), pDieNoPPH),
    path12 <- prod((1-pOxy), pMisoAccess, (1-pMisuseMiso), (1-pPPHMiso), (1-pDieNoPPH)),
    path15 <- prod((1-pOxy), pMisoAccess, pMisuseMiso, (1-pNoMisoAE), pUterineRuptureMiso, pDieUterineRupture),
    path16 <- prod((1-pOxy), pMisoAccess, pMisuseMiso, (1-pNoMisoAE), pUterineRuptureMiso, (1-pDieUterineRupture)),
    path17 <- prod((1-pOxy), pMisoAccess, pMisuseMiso, (1-pNoMisoAE), (1-pUterineRuptureMiso), pDieStillBirth),
    path18 <- prod((1-pOxy), pMisoAccess, pMisuseMiso, (1-pNoMisoAE), (1-pUterineRuptureMiso), (1-pDieStillBirth)),
    path19 <- prod((1-pOxy), pMisoAccess, pMisuseMiso, pNoMisoAE, pPPH, pAccessEmOC, pDieEmOC),
    path20 <- prod((1-pOxy), pMisoAccess, pMisuseMiso, pNoMisoAE, pPPH, pAccessEmOC, (1-pDieEmOC)),
    path21 <- prod((1-pOxy), pMisoAccess, pMisuseMiso, pNoMisoAE, pPPH, (1-pAccessEmOC), pDieEmOC),
    path22 <- prod((1-pOxy), pMisoAccess, pMisuseMiso, pNoMisoAE, pPPH, (1-pAccessEmOC), (1-pDieEmOC)),
    path23 <- prod((1-pOxy), pMisoAccess, pMisuseMiso, pNoMisoAE, (1-pPPH), pDieNoPPH),
    path24 <- prod((1-pOxy), pMisoAccess, pMisuseMiso, pNoMisoAE, (1-pPPH), (1-pDieNoPPH)),
    path27 <- prod((1-pOxy), (1-pMisoAccess), pPPH, pAccessEmOC, pDieEmOC),
    path28 <- prod((1-pOxy), (1-pMisoAccess), pPPH, pAccessEmOC, (1-pDieEmOC)),
    path29 <- prod((1-pOxy), (1-pMisoAccess), pPPH, (1-pAccessEmOC), pDieEmOC),
    path30 <- prod((1-pOxy), (1-pMisoAccess), pPPH, (1-pAccessEmOC), (1-pDieEmOC)),
    path31 <- prod((1-pOxy), (1-pMisoAccess), (1-pPPH), pDieNoPPH),
    path32 <- prod((1-pOxy), (1-pMisoAccess), (1-pPPH), (1-pDieNoPPH))
  )
  
  # don't forget to detach
  #detach(beta1)
  
  return(pathlist)
  
}


getYLL <- function(disc, time, wt) {
  
  disc * time
  
}

getYLD <- function(wtD, time) {
  
  wtD * time
  
}


getDALYS <- function(age, LE, disabilitywt, duration, discFact) {
  
  ages = seq(age, LE, by = 1)
  index = (0:(length(ages)-1))
  discFact = (1 / (1 + discFact)^index)
  YLL = getYLD(disabilitywt, duration)
  YLD = sum(getYLL(disc = discFact, time = c(1-duration, rep(1, (length(ages)-2)),0.5)))
  return(list(YLL, YLD))
  
}

getCareCost <- function(Societal = TRUE, Intervention = TRUE) {
  
  # calculate individual cost paths
  # to preserve structure with the paths module, set places where there is no cost to zero
  cOxy <- ifelse(Intervention, (cOxytocin + cDestruction), cOxytocin)
  cMisoAccess <- ifelse(Intervention, 0, cMisoprostol) #  patient can still access Miso in HF if in Intervention; for intervention I add cMisoprostol to everyone
  
  cAccessEmOC <- ifelse(Societal, cPPHManagementHospSocietal, cPPHManagementHosp)
  cUterineRupture <- ifelse(Societal, cUterineRuptureSocietal, cUterineRupture)
  cVaginalStillBirth <- ifelse(Societal, cVaginalStillBirthSocietal, cVaginalStillBirth)
  
  costlist <- c(
    cost01 <- sum(cOxy, cPPH = 0, cAccessEmOC, cDieEmOC = cMortality),
    cost02 <- sum(cOxy, cPPH = 0, cAccessEmOC, cAliveEmOC = 0),
    cost03 <- sum(cOxy, cPPH = 0, cNoAccessEmOC = 0, cDieEmOC = cMortality),
    cost04 <- sum(cOxy, cPPH = 0, cNoAccessEmOC = 0, cDieEmOC = 0),
    cost05 <- sum(cOxy, cNoPPH = 0, cDieNoPPH = cMortality),
    cost06 <- sum(cOxy, cNoPPH = 0, cAliveNoPPH = 0),
    cost07 <- sum(cOxy = 0, cMisoAccess, cNoMisuseMiso = 0, cPPH = 0, cAccessEmOC, cDieEmOC = cMortality),
    cost08 <- sum(cOxy = 0, cMisoAccess, cNoMisuseMiso = 0, cPPH = 0, cAccessEmOC, cAliveEmOC = 0),
    cost09 <- sum(cOxy = 0, cMisoAccess, cNoMisuseMiso = 0, cPPH = 0, cNoAccessEmOC = 0, cDieEmOC = cMortality),
    cost10 <- sum(cOxy = 0, cMisoAccess, cNoMisuseMiso = 0, cPPH = 0, cNoAccessEmOC = 0, cAliveEmOC = 0),
    cost11 <- sum(cOxy = 0, cMisoAccess, cNoMisuseMiso = 0, cNoPPH = 0, cDieNoPPH = cMortality),
    cost12 <- sum(cOxy = 0, cMisoAccess, cNoMisuseMiso = 0, cNoPPH = 0, cAliveNoPPH = 0),
    cost15 <- sum(cOxy = 0, cMisoAccess, cMisuseMiso = 0, cMisoAE = 0, cUterineRupture, cDieUterineRupture = cMortality),
    cost16 <- sum(cOxy = 0, cMisoAccess, cMisuseMiso = 0, cMisoAE = 0, cUterineRupture, cAliveUterineRupture = 0),
    cost17 <- sum(cOxy = 0, cMisoAccess, cMisuseMiso = 0, cMisoAE = 0, cVaginalStillBirth, cDieStillBirth = cMortality),
    cost18 <- sum(cOxy = 0, cMisoAccess, cMisuseMiso = 0, cMisoAE = 0, cVaginalStillBirth, cAliveStillBirth = 0),
    cost19 <- sum(cOxy = 0, cMisoAccess, cMisuseMiso = 0, cNoMisoAE = 0, cPPH = 0, cAccessEmOC, cDieEmOC = cMortality),
    cost20 <- sum(cOxy = 0, cMisoAccess, cMisuseMiso = 0, cNoMisoAE = 0, cPPH = 0, cAccessEmOC, cAliveEmOC = 0),
    cost21 <- sum(cOxy = 0, cMisoAccess, cMisuseMiso = 0, cNoMisoAE = 0, cPPH = 0, cNoAccessEmOC = 0, cDieEmOC = cMortality),
    cost22 <- sum(cOxy = 0, cMisoAccess, cMisuseMiso = 0, cNoMisoAE = 0, cPPH = 0, cNoAccessEmOC = 0, cAliveEmOC = 0),
    cost23 <- sum(cOxy = 0, cMisoAccess, cMisuseMiso = 0, cNoMisoAE = 0, cNoPPH = 0, cDieNoPPH = cMortality),
    cost24 <- sum(cOxy = 0, cMisoAccess, cMisuseMiso = 0, cNoMisoAE = 0, cNoPPH = 0, cAliveNoPPH = 0),
    cost27 <- sum(cOxy = 0, cNoMisoAccess = 0, cPPH = 0, cAccessEmOC, cDieEmOC = cMortality),
    cost28 <- sum(cOxy = 0, cNoMisoAccess = 0, cPPH = 0, cAccessEmOC, cAliveEmOC = 0),
    cost29 <- sum(cOxy = 0, cNoMisoAccess = 0, cPPH = 0, cNoAccessEmOC = 0, cDieEmOC = cMortality),
    cost30 <- sum(cOxy = 0, cNoMisoAccess = 0, cPPH = 0, cNoAccessEmOC = 0, cAliveEmOC = 0),
    cost31 <- sum(cOxy = 0, cNoMisoAccess = 0, cNoPPH = 0, cDieNoPPH = cMortality),
    cost32 <- sum(cOxy = 0, cNoMisoAccess = 0, cNoPPH = 0, cAliveEmOC = 0)
  )
  
  return(costlist)
  
}


getIncidencePath <- function(Where, Intervention = TRUE) {
  
  if (Where == "Hospital") {
    
    pOxy <- pOxyHosp
    pMisoAccess <- pMisoAccessHospMisoPolicy * (Intervention==TRUE) + pMisoAccessHospOxyPolicy * (Intervention==FALSE)
    
    # hospital deliveries, apply pHospPPH
    pPPHOxy  <- pHospPPH*rPPHOxyPlacebo
    pPPHMiso <- pHospPPH*rPPHMisoHF
    pPPH     <- pHospPPH
    
  } else if (Where == "HC") {
    
    pOxy <- pOxyHC
    pMisoAccess <- pMisoAccessHCMisoPolicy * (Intervention==TRUE) + pMisoAccessHCOxyPolicy * (Intervention==FALSE)
    
    # hospital deliveries, apply pHospPPH
    pPPHOxy  <- pHospPPH*rPPHOxyPlacebo
    pPPHMiso <- pHospPPH*rPPHMisoHF
    pPPH     <- pHospPPH
    
  } else if (Where == "Friend") {
    
    pOxy <- pOxyHomeFriend
    pMisoAccess <- pMisoAccessHomeFriendMisoPolicy * (Intervention==TRUE) + pMisoAccessHomeOxyPolicy * (Intervention==FALSE)
    
    # home deliveries, apply pHomePPH
    pPPHOxy  <- pHomePPH*rPPHOxyPlacebo
    pPPHMiso <- pHomePPH*rPPHMisoOxy
    pPPH     <- pHomePPH
    
  } else if (Where == "TBA") {
    
    pOxy <- pOxyHomeTBA
    pMisoAccess <- pMisoAccessHomeTBAMisoPolicy * (Intervention==TRUE) + pMisoAccessHomeOxyPolicy * (Intervention==FALSE)
    
    # home deliveries, apply pHomePPH
    pPPHOxy  <- pHomePPH*rPPHOxyPlacebo
    pPPHMiso <- pHomePPH*rPPHMisoOxy
    pPPH     <- pHomePPH
    
  } else if (Where == "Unassisted") {
    
    pOxy <- pOxyHomeUnass
    pMisoAccess <- pMisoAccessHomeUnassMisoPolicy * (Intervention==TRUE) + pMisoAccessHomeOxyPolicy * (Intervention==FALSE)
    
    # home deliveries, apply pHomePPH
    pPPHOxy  <- pHomePPH*rPPHOxyPlacebo
    pPPHMiso <- pHomePPH*rPPHMisoOxy
    pPPH     <- pHomePPH
    
  }
  
  # calculate individual paths
  pathlist <- c(
    path01 <- prod(pOxy, pPPHOxy),
    path06 <- prod(pOxy, (1-pPPHOxy)),
    path07 <- prod((1-pOxy), pMisoAccess, (1-pMisuseMiso), pPPHMiso),
    path12 <- prod((1-pOxy), pMisoAccess, (1-pMisuseMiso), (1-pPPHMiso)),
    path15 <- prod((1-pOxy), pMisoAccess, pMisuseMiso, pPPH = 0), # no one gets PPH
    path16 <- prod((1-pOxy), pMisoAccess, pMisuseMiso, noPPH = 1), # no one gets PPH because either a uterine rupture or still birth
    path27 <- prod((1-pOxy), (1-pMisoAccess), pPPH),
    path32 <- prod((1-pOxy), (1-pMisoAccess), (1-pPPH))
  )
  

  return(pathlist)
  
}



getIncidenceCost <- function(Societal = TRUE, Intervention = TRUE) {
  
  # calculate individual cost paths
  # to preserve structure with the paths module, set places where there is no cost to zero
  cOxy <- ifelse(Intervention, (cOxytocin + cDestruction), cOxytocin)
  cMisoAccess <- ifelse(Intervention, 0, cMisoprostol) #  patient can still access Miso in HF if in Intervention; for intervention I add cMisoprostol to everyone

  IncidCostlist <- c(
    cost01 <- sum(cOxy, cPPH = 0),
    cost06 <- sum(cOxy, cNoPPH = 0),
    cost07 <- sum(cOxy = 0, cMisoAccess, cNoMisuseMiso = 0, cPPH = 0),
    cost12 <- sum(cOxy = 0, cMisoAccess, cNoMisuseMiso = 0, cNoPPH = 0),
    cost15 <- sum(cOxy = 0, cMisoAccess, cMisuseMiso = 0, cPPH = 0),
    cost16 <- sum(cOxy = 0, cMisoAccess, cMisuseMiso = 0, cNoPPH = 0),
    cost27 <- sum(cOxy = 0, cNoMisoAccess = 0, cPPH = 0),
    cost32 <- sum(cOxy = 0, cNoMisoAccess = 0, cNoPPH = 0)
  )
  
  return(IncidCostlist)
  
}


# function calculates care path depending on wealth quintile
# Intervention = TRUE will apply the treatment effects (in this function on odds of HF delivery)
getAccessCost <- function(Societal = TRUE, Intervention = TRUE) {
  
  cHospDel <- ifelse(Societal, cNormalDeliveryHospSocietal, cNormalDeliveryHosp)
  cHCDel <- ifelse(Societal, cNormalDeliveryHCSocietal, cNormalDeliveryHC)
  cFriendRelDel <- ifelse(Societal, cNormalDeliveryHomeFriend, 0)
  cDelTBA <- ifelse(Societal, cNormalDeliveryHomeTBA, 0)
  cDelUnass <- ifelse(Societal, cNormalDeliveryHomeUnass, 0)
  
  ProgramCost <- ifelse(Intervention, cMisoprostolProgram, 0)

  # calculate product of path probabilities
  hosp   <- cHospDel + ProgramCost
  hc     <- cHCDel + ProgramCost
  friend <- cFriendRelDel + ProgramCost
  TBA    <- cDelTBA + ProgramCost
  unass  <- cDelUnass + ProgramCost
  
  return(c(hospital = hosp, hc = hc, friend = friend, TBA = TBA, unass = unass))
  
}


getMisoProgCosts <- function(nWomen, nHW, cTrain, daysTraining, cNurse, nNurse, nurseFTE, cMisoprostol, cPackage) {
  
  cHWTraining <- nHW * cTrain * daysTraining ## 200 HWs @ $10/day for 5 days
  cMotherTraining <- cNurse * 8 * 5 * nNurse * nurseFTE # 20 MidWife FTEs
  cPackaging <- cMisoprostol * cPackage * nWomen
  
  # put it together
  cMisoprostolProgram <- (cMisoprostol*nWomen + cHWTraining + cMotherTraining + cPackaging)/nWomen
  
  return(cMisoprostolProgram)
  
}


RunModel <- function(baseparms, 
                     basetransitions) {
  
  beta1 <- as.list(c(baseparms, basetransitions))
  attach(beta1)

  environment(getAccessCost) <- environment() # makes temp copy of getAccessCost function so variables created here are accessible to it without needing to plance entire mtrace function in the RunModel function
  environment(getCareCost) <- environment() # makes temp copy of getCareCost function so variables created here are accessible to it without needing to plance entire mtrace function in the RunModel function
  
  daly_vec_PPH <- sapply(age, FUN = getDALYS, LE = LifeExp, disabilitywt = disabilitywtPPH, duration = durationPPH, discFact = discFact)
  daly_vec_URup <- sapply(age, FUN = getDALYS, LE = LifeExp, disabilitywt = disabilitywtRupture, duration = durationUterineRupture, discFact = discFact)
  daly_vec_StillB <- sapply(age, FUN = getDALYS, LE = LifeExp, disabilitywt = disabilitywtStillBirth, duration = durationStillBirth, discFact = discFact)
  
  proportions <- c(isless20, is20to34, is35to49)
  
  DALYs_PPH <- proportions %*% t(matrix(unlist(daly_vec_PPH), nrow = 2, byrow = FALSE))
  DALYs_URup <- proportions %*% t(matrix(unlist(daly_vec_URup), nrow = 2, byrow = FALSE))
  DALYs_StillB <- proportions %*% t(matrix(unlist(daly_vec_StillB), nrow = 2, byrow = FALSE))
  
  pphYLD <- DALYs_PPH[1, 1]
  pphYLL <- DALYs_PPH[1, 2]
  
  ruptureYLL <- DALYs_URup[1, 1]
  ruptureYLD <- DALYs_URup[1, 2]
  
  stillbirthYLL <- DALYs_StillB[1, 1]
  stillbirthYLD <- DALYs_StillB[1, 2]
  
  ## opportunity cost calculations
  cNormDelHomeFriend <- (cProductivity/procDaysYr)*avgFriendTime
  cOppCostPPHPatientTime <- ((averageLOSPPH*cProductivity)/procDaysYr)+((averageTravelTime*cProductivity)/(procDaysYr*procHrsDay))
  OppCostPatientTimeHome <- ((averageLOSHome*cProductivity)/procDaysYr)
  cOppCostRupturePatientTime <- ((averageLOSUterineRupture*cProductivity)/procDaysYr)+((averageTravelTime*cProductivity)/(procDaysYr*procHrsDay))
  cOppCostNormDelPatient <- ((averageLOSNormDel*cProductivity)/procDaysYr)+((averageTravelTime*cProductivity)/(procDaysYr*procHrsDay))
  
  dirNonMedHC <- cTravelHC + cUpkeepHC
  dirNonMedHosp <- cTravelHospital + cUpkeepHospital
  
  # costs of PPH management
  cPPHManagementHCSocietal <- cPPHManagementHC + dirNonMedHC + cOppCostPPHPatientTime
  cPPHManagementHospSocietal <- cPPHManagementHosp + dirNonMedHosp + cOppCostPPHPatientTime
  
  # costs of Normal Delivery
  cNormalDeliveryHCSocietal <- cNormalDeliveryHC + dirNonMedHC + cOppCostNormDelPatient
  cNormalDeliveryHospSocietal <- cNormalDeliveryHosp + dirNonMedHC + cOppCostNormDelPatient
  
  # costs of treating a uterine rupture (only in hospital)
  cUterineRuptureSocietal <- cUterineRupture +  dirNonMedHosp + cOppCostRupturePatientTime
  
  # cost of a vaginal still birth equal to costs of a normal delivery
  cVaginalStillBirthSocietal <- cVaginalStillBirth + dirNonMedHC + cOppCostNormDelPatient

  # normal delivery by TBA
  cNormalDeliveryHomeTBA <- cNormDelHomeTBA + cTravelTBA + cNormDelHomeFriend
  
  # normal delivery by friend
  cNormalDeliveryHomeFriend <- cNormDelHomeFriend
  
  # normal delivery home unassisted
  cNormalDeliveryHomeUnass <- cOppCostNormDelPatient
  
  # calculate misoprostol program costs
  cMisoprostolProgram <- getMisoProgCosts(nWomen, nHW, cTrain, daysTraining, cNurse, nNurse, nurseFTE, cMisoprostol, cPackage)
  
  ### PAYOFF VECTORS, convert everything to matrix then tibble to ease calculations
  # calculate incidence
  # 1 = incident case, 0 = not an incident case
  # avoid counting incidence multiple times, assume model was built to estimate the incidence of PPH only
  incidence <- c(1, 0, 1, 0, 1, 0, 1, 0) %>% 
    matrix(., ncol = 1) %>%
    as_tibble()

  # calculate mortality
  mortality <- c(1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0) %>% 
    matrix(., ncol = 1) %>%
    as_tibble()
  
  # calculate DALYs
  DALYlist <- c(pphYLL+pphYLD*pSevereAnemiaEmOC, pphYLD*pSevereAnemiaEmOC,
                pphYLL+pphYLD*pSevereAnemiaEmOC, pphYLD*pSevereAnemiaEmOC,
                0, 0, 
                pphYLL+pphYLD*pSevereAnemiaEmOC, pphYLD*pSevereAnemiaEmOC,
                pphYLL+pphYLD*pSevereAnemiaEmOC, pphYLD*pSevereAnemiaEmOC,
                0, 0, 
                ruptureYLL+ruptureYLD, ruptureYLL, 
                stillbirthYLL+stillbirthYLD, stillbirthYLL,
                pphYLL+pphYLD*pSevereAnemiaEmOC, pphYLD*pSevereAnemiaEmOC, 
                pphYLL+pphYLD*pSevereAnemiaEmOC, pphYLD*pSevereAnemiaEmOC, 
                0, 0,
                pphYLL+pphYLD*pSevereAnemiaEmOC, pphYLD*pSevereAnemiaEmOC, 
                pphYLL+pphYLD*pSevereAnemiaEmOC, pphYLD*pSevereAnemiaEmOC,
                0, 0) %>% 
    matrix(., ncol = 1) %>%
    as_tibble()
  

  # TREATMENT TRAJECTORIES
  TxTrajectory_NoMiso <- lapply(TxTrjList, FUN = getTxPath, Intervention = FALSE)
  TxTrajectory_Miso <- lapply(TxTrjList, FUN = getTxPath, Intervention = TRUE)
  
  # INCIDENCE ONLY TRAJECTORY
  IncidTrajectory_NoMiso <- lapply(TxTrjList, FUN = getIncidencePath, Intervention = FALSE)
  IncidTrajectory_Miso <- lapply(TxTrjList, FUN = getIncidencePath, Intervention = TRUE)
  
  
  # CAREPATHWAY TRAJECTORIES
  CareTrajectory_NoMiso <- lapply(QuntileList, FUN = getCarePath, Intervention = FALSE)
  CareTrajectory_Miso <- lapply(QuntileList, FUN = getCarePath, Intervention = TRUE)
  
  # combine CAREPATHWAY & TREATMENT TRAJECTORIES for each quintile
  # outer product to multiply each element of carepathway trajectory list with its counterpart in the treatment trajectory pathway
  # data is pooled by quintiles, each element of the returned list corresponds to a wealth quintile
  # overall pathway probabilities for each quintile (they should sum to 1)
  noMisoAll <- lapply(CareTrajectory_NoMiso, map2_dfc, .x = TxTrajectory_NoMiso, .f = outer)
  MisoAll <- lapply(CareTrajectory_Miso, map2_dfc, .x = TxTrajectory_Miso, .f = outer)
  
  # incidence pathway probabilities for each quintile
  # should sum to 1
  IncidnoMisoAll <- lapply(CareTrajectory_NoMiso, map2_dfc, .x = IncidTrajectory_NoMiso, .f = outer)
  IncidMisoAll <- lapply(CareTrajectory_Miso, map2_dfc, .x = IncidTrajectory_Miso, .f = outer)
  
  
  # COST for TREATMENT TRAJECTORIES
  SocCareCostNoMiso <- getCareCost(Societal = TRUE, Intervention = FALSE)
  GovCareCostNoMiso <- getCareCost(Societal = FALSE, Intervention = FALSE)
  SocCareCostMiso <- getCareCost(Societal = TRUE, Intervention = TRUE)
  GovCareCostMiso <- getCareCost(Societal = FALSE, Intervention = TRUE)
  
  # incidence only cost
  SocIncidCostNoMiso <- getIncidenceCost(Societal = TRUE, Intervention = FALSE)
  GovIncidCostNoMiso <- getIncidenceCost(Societal = FALSE, Intervention = FALSE)
  SocIncidCostMiso <- getIncidenceCost(Societal = TRUE, Intervention = TRUE)
  GovIncidCostMiso <- getIncidenceCost(Societal = FALSE, Intervention = TRUE)

  # COSTS OF CARE
  SocAccessCostNoMiso <- getAccessCost(Societal = TRUE, Intervention = FALSE)
  GovAccessCostNoMiso <- getAccessCost(Societal = FALSE, Intervention = FALSE)
  SocAccessCostMiso <- getAccessCost(Societal = TRUE, Intervention = TRUE)
  GovAccessCostMiso <- getAccessCost(Societal = FALSE, Intervention = TRUE)
  
  # put it together - TOTAL CARE COSTS
  totalSocCostNoMiso <- t(outer(SocAccessCostNoMiso, SocCareCostNoMiso, FUN = "+")) %>% as_tibble()
  totalGovCostNoMiso <- t(outer(GovAccessCostNoMiso, GovCareCostNoMiso, FUN = "+")) %>% as_tibble()
  totalSocCostMiso <- t(outer(SocAccessCostMiso, SocCareCostMiso, FUN = "+")) %>% as_tibble()
  totalGovCostMiso <- t(outer(GovAccessCostMiso, GovCareCostMiso, FUN = "+")) %>% as_tibble()
  
  # put it together - Incidence COSTS
  IncidSocCostNoMiso <- t(outer(SocAccessCostNoMiso, SocIncidCostNoMiso, FUN = "+")) %>% as_tibble()
  IncidGovCostNoMiso <- t(outer(GovAccessCostNoMiso, GovIncidCostNoMiso, FUN = "+")) %>% as_tibble()
  IncidSocCostMiso <- t(outer(SocAccessCostMiso, SocIncidCostMiso, FUN = "+")) %>% as_tibble()
  IncidGovCostMiso <- t(outer(GovAccessCostMiso, GovIncidCostMiso, FUN = "+")) %>% as_tibble()
  
  # Do the rest of the final math here in the function
  # INCIDENCE
  IncidenceNoMiso <- sapply(lapply(IncidnoMisoAll, FUN = map_dfc, .x = incidence, .f = ~.x * .y), sum)
  IncidenceMiso <- sapply(lapply(IncidMisoAll, FUN = map_dfc, .x = incidence, .f = ~.x * .y), sum)
  
  # MORTALITY
  mortalityNoMiso <- sapply(lapply(noMisoAll, FUN = map_dfc, .x = mortality, .f = ~.x * .y), sum)
  mortalityMiso <- sapply(lapply(MisoAll, FUN = map_dfc, .x = mortality, .f = ~.x * .y), sum)
  
  # DALYs
  DALYsNoMiso <- sapply(lapply(noMisoAll, FUN = map_dfc, .x = DALYlist, .f = ~.x * .y), sum)
  DALYsMiso <- sapply(lapply(MisoAll, FUN = map_dfc, .x = DALYlist, .f = ~.x * .y), sum)
  
  # COSTS
  SocCostsNoMiso <- sapply(lapply(noMisoAll, map2_dfr, .y = totalSocCostNoMiso, .f = ~.x * .y), sum)
  SocCostsMiso <- sapply(lapply(MisoAll, map2_dfr, .y = totalSocCostNoMiso, .f = ~.x * .y), sum)
  GovCostsNoMiso <- sapply(lapply(noMisoAll, map2_dfr, .y = totalGovCostNoMiso, .f = ~.x * .y), sum)
  GovCostsMiso <- sapply(lapply(MisoAll, map2_dfr, .y = totalGovCostNoMiso, .f = ~.x * .y), sum)
  
  # Incidence Costs
  SocCostsIncidNoMiso <- sapply(lapply(IncidnoMisoAll, map2_dfr, .y = IncidSocCostNoMiso, .f = ~.x * .y), sum)
  SocCostsIncidMiso <- sapply(lapply(IncidMisoAll, map2_dfr, .y = IncidSocCostMiso, .f = ~.x * .y), sum)
  GovCostsIncidNoMiso <- sapply(lapply(IncidnoMisoAll, map2_dfr, .y = IncidGovCostNoMiso, .f = ~.x * .y), sum)
  GovCostsIncidMiso <- sapply(lapply(IncidMisoAll, map2_dfr, .y = IncidGovCostMiso, .f = ~.x * .y), sum)
  
  # sum everything, then weight by proportions in each quantile
  # done above with lapply and sum
  
  # now weight outcomes and total
  wts <- c(pLowQuintile, pSecQuintile, pMidQuintile, pFourthQuintile, pHighQuintile)
  
  # collect indicence outcomes
  IncidOutcomes <- list(IncidenceNoMiso = IncidenceNoMiso, IncidenceMiso = IncidenceMiso, deltaIncidence = (IncidenceNoMiso - IncidenceMiso), 
                        SocCostsIncidNoMiso = SocCostsIncidNoMiso, SocCostsIncidMiso = SocCostsIncidMiso, deltaSocCostsIncid = (SocCostsIncidMiso - SocCostsIncidNoMiso), 
                        GovCostsIncidNoMiso = GovCostsIncidNoMiso, GovCostsIncidMiso = GovCostsIncidMiso, deltaGovCostsIncid = (GovCostsIncidMiso - GovCostsIncidNoMiso), 
                        iCERSocIncid = (SocCostsIncidMiso - SocCostsIncidNoMiso)/(IncidenceNoMiso - IncidenceMiso),
                        iCERGovIncid = (GovCostsIncidMiso - GovCostsIncidNoMiso)/(IncidenceNoMiso - IncidenceMiso))
  
  wtIncidOutcomes <- lapply(IncidOutcomes, "*", wts)  %>%
    bind_cols()
  
  avgIncidOutcomes <- bind_rows(IncidOutcomes, total = apply(wtIncidOutcomes, MARGIN = 2, sum))
  
  # collect mortality outcomes
  MortOutcomes <- list(mortalityNoMiso = mortalityNoMiso, mortalityMiso = mortalityMiso, deltaMortality = (mortalityNoMiso - mortalityMiso),
                       SocCostsNoMiso = SocCostsNoMiso, SocCostsMiso = SocCostsMiso, deltaSocCosts = (SocCostsMiso - SocCostsNoMiso), 
                       GovCostsMiso = GovCostsMiso, GovCostsNoMiso = GovCostsNoMiso, deltaGovCosts = (GovCostsMiso - GovCostsNoMiso),
                       iCERSocMortality = (SocCostsMiso - SocCostsNoMiso)/(mortalityNoMiso - mortalityMiso), 
                       iCERGovMortality = (GovCostsMiso - GovCostsNoMiso)/(mortalityNoMiso - mortalityMiso))
  
  wtMortOutcomes <- lapply(MortOutcomes, "*", wts) %>%
    bind_cols()
  
  avgMortOutcomes <- bind_rows(MortOutcomes, total = apply(wtMortOutcomes, MARGIN = 2, sum))
  
  # collect DALY outcomes
  DALYOutcomes <- list(DALYsNoMiso = DALYsNoMiso, DALYsMiso = DALYsMiso, deltaDALYs = (DALYsNoMiso - DALYsMiso),
                       SocCostsMiso = SocCostsMiso, SocCostsNoMiso = SocCostsNoMiso, deltaSocCosts = (SocCostsMiso - SocCostsNoMiso), 
                       GovCostsMiso = GovCostsMiso, GovCostsNoMiso = GovCostsNoMiso, deltaGovCosts = (GovCostsMiso - GovCostsNoMiso), 
                       iCERSocDALYs = (SocCostsMiso - SocCostsNoMiso)/(DALYsNoMiso - DALYsMiso), 
                       iCERGovDALYs = (GovCostsMiso - GovCostsNoMiso)/(DALYsNoMiso - DALYsMiso))
  
  wtDALYOutcomes <- lapply(DALYOutcomes, "*", wts) %>%
    bind_cols()
  
  avgDALYOutcomes <- bind_rows(DALYOutcomes, total = apply(wtDALYOutcomes, MARGIN = 2, sum))

  # don't forget to detach
  detach(beta1)
  
  return(list(Incidence = avgIncidOutcomes, Mortality = avgMortOutcomes, DALYS = avgDALYOutcomes, 
              baseparms = baseparms, basetransitions = basetransitions))
  
}

## loop through base-case, high and low values and calculate model
# function requires user to have run the incremental analysis, if not, run incremental within the function and extrace the things we need
owsa <- function(model, low_base, low_transitions, high_base, high_transitions, 
                 outcome, max_vars) {
  
  # get the baseinputs and transitions from model object
  baseinputs <- model$baseparms
  basetransitions <- model$basetransitions
  
  # identify basecase values
  basecase <- switch(outcome,
                     Incidence = c(model$Incidence[6, 3], model$Incidence[6, 6], model$Incidence[6, 9:11]),
                     Mortality = c(model$Mortality[6, 3], model$Mortality[6, 6], model$Mortality[6, 9:11]),
                     DALYs = c(model$DALYS[6, 3], model$DALYS[6, 6], model$DALYS[6, 9:11]))
  

  # set up matrix to collect the results
  owsamat <- matrix(NA,
                    nrow = length(low_base) + length(low_transitions),
                    ncol=10,
                    dimnames=list(c(names(low_base), names(low_transitions)),
                                  c(paste0("low ", names(basecase)),
                                    paste0("high ", names(basecase)))))
  
  lowinputs <- low_base
  highinputs <- high_base
  owsavars <- c(paste0(names(low_base), " (", sprintf(lowinputs, fmt = "%#.2f"),", ", sprintf(highinputs, fmt = "%#.2f"),")"),
                paste0("Distribution of - ", names(low_transitions)))
  
  i = 0
  # set all live values to the base case
  live <- baseinputs
  for (n in names(lowinputs)) {
    i = i + 1
    # counter
    message("DSA on ", outcome, ": analyzing impact of ", n, " (variable ", i, ") of ", length(low_base) + length(low_transitions))
    # replace the current parameter with its low value and run the model
    low.input <- lowinputs[n]
    live[n] <- low.input
    
    model <- RunModel(baseparms = live, 
                      basetransitions = basetransitions)
    
    dsa.low <- switch(outcome,
                      Incidence = c(model$Incidence[6, 3], model$Incidence[6, 6], model$Incidence[6, 9:11]),
                      Mortality = c(model$Mortality[6, 3], model$Mortality[6, 6], model$Mortality[6, 9:11]),
                      DALYs = c(model$DALYS[6, 3], model$DALYS[6, 6], model$DALYS[6, 9:11]))

    # replace the current parameter with its high value and run the model
    high.input <- highinputs[n]
    live[n] <- high.input
    
    model <- RunModel(baseparms = live, 
                      basetransitions = basetransitions)
    
    dsa.high <- switch(outcome,
                      Incidence = c(model$Incidence[6, 3], model$Incidence[6, 6], model$Incidence[6, 9:11]),
                      Mortality = c(model$Mortality[6, 3], model$Mortality[6, 6], model$Mortality[6, 9:11]),
                      DALYs = c(model$DALYS[6, 3], model$DALYS[6, 6], model$DALYS[6, 9:11]))
    
    owsamat[i, ] <- c(unlist(dsa.low), unlist(dsa.high))
    

    # reset to the original value
    live[n] <- baseinputs[n]
  }
  
  # do the same for the transition probabilities, but put all in at once
  # set all live values to the base case, the 4th element of list returned by the RunModel function
  live <- basetransitions
  
  for (i in 1:length(low_transitions)) {
    
    message("DSA on ", outcome, ": analyzing impact of dirichlet input set of ", names(low_transitions)[i], " (variable ", i + length(low_base), ") of ", length(low_base) + length(low_transitions))
    #message("DSA on ", outcome, ": analyzing impact of ", n, " (variable ", i, ") of ", length(low_base) + length(low_transitions))
    
    # replace the current parameter-set with its low value and run the model
    low.input <- low_transitions[[i]]
    live[names(low_transitions[[i]])] <- low.input
    
    model <- RunModel(baseparms = baseinputs, 
                      basetransitions = live)
    
    dsa.low <- switch(outcome,
                      Incidence = c(model$Incidence[6, 3], model$Incidence[6, 6], model$Incidence[6, 9:11]),
                      Mortality = c(model$Mortality[6, 3], model$Mortality[6, 6], model$Mortality[6, 9:11]),
                      DALYs = c(model$DALYS[6, 3], model$DALYS[6, 6], model$DALYS[6, 9:11]))
    
    
    # replace the current parameter-set with its high value and run the model
    high.input <- high_transitions[[i]]
    live[names(high_transitions[[i]])] <- high.input
    
    model <- RunModel(baseparms = baseinputs, 
                      basetransitions = live)
    
    dsa.high <- switch(outcome,
                       Incidence = c(model$Incidence[6, 3], model$Incidence[6, 6], model$Incidence[6, 9:11]),
                       Mortality = c(model$Mortality[6, 3], model$Mortality[6, 6], model$Mortality[6, 9:11]),
                       DALYs = c(model$DALYS[6, 3], model$DALYS[6, 6], model$DALYS[6, 9:11]))
    
    owsamat[i+length(low_base), ] <- c(unlist(dsa.low), unlist(dsa.high))
    
    # reset to the original value in basetransitions
    live[names(high_transitions[[i]])] <- basetransitions[names(high_transitions[[i]])]
    
  }
  
  owsavars <- c(paste0(names(usa.low), " (", sprintf(usa.low, fmt = "%#.2f"),", ", sprintf(usa.high, fmt = "%#.2f"),")"),
                paste0("Distribution of - ", names(low_transitions)))
  
  
  # post processing of data to return full results
  # split the table into respective components
  
  # loop through table and create list of tables for each outcome 
  
  owsadata <- list()
  p <- list()
  
  for (i in 1:5) {
    owsa_tab <- as_tibble(owsamat) %>%
      select(c(0 + i, 5 + i)) %>%
      rename("low" = 1, "high" = 2) %>%
      mutate(range = abs(high - low), highval = high - basecase[[i]], lowval = low - basecase[[i]]) %>%
      add_column(variable = owsavars, .before = 1) %>%
      arrange(range)
    
    # generate plot object (can deal with axis labels later)
    # owsaplot[[i]] <- TornadoPlot(owsatab = owsa_tab, basecase = basecase[[i]], max_vars = max_vars)
    
    # filter for those with perc impact on outcome
    owsa_torn <- owsa_tab %>%
      slice_max(range, n = max_vars) %>%
      arrange(range)
    
    # axislabel <- switch(outcome,
    #                     ICER=paste0("Cost per ", on_outcome),
    #                     cost="Incremental Cost ($)",
    #                     outcome=on_outcome)
    
    p[[i]] <- plot_ly(data = owsa_torn) %>%
      add_trace(x = ~lowval, y = ~variable, 
                base = basecase, 
                type = 'bar', orientation = 'h', name = "lower input",
                marker = list(color = "rgba(166,206,227,0.7)",
                              line = list(color = "rgb(166,206,227)",
                                          width = 2))) %>%
      add_trace(x = ~highval, y = ~variable, 
                base = basecase, 
                type = 'bar', orientation = 'h', name = "higher input",
                marker = list(color = "rgba(31,120,180,0.7)",
                              line = list(color = "rgb(31,120,180)",
                                          width = 2))) %>%
      layout(barmode='overlay') %>% 
      layout(yaxis = list(categoryorder = "array",
                          categoryarray = ~variable,
                          title = ""),
             xaxis = list(zeroline = FALSE,
                          showline = FALSE,
                          showgrid = FALSE
                          #title = axislabel
             ),
             font = list(size=12, 
                         family='Fira Sans'))
    
    # also store owsadata in case I need to inspect it 
    #owsadata[[i]] <- owsa_tab
  }

  #return(subplot(p[[1]], p[[2]], p[[3]], p[[4]], p[[5]]))
  
  return(subplot(p))
  
}

# function takes output from DSA as a list, and plots the results (user inputs max vars)
TornadoPlot <- function(owsatab, basecase, max_vars) {
  
  # set color
  col <- brewer.pal(3,"Dark2")
  
  # filter for those with perc impact on outcome
  owsa_torn <- owsatab %>%
    slice_max(range, n = max_vars) %>%
    arrange(range)
  
  # axislabel <- switch(outcome,
  #                     ICER=paste0("Cost per ", on_outcome),
  #                     cost="Incremental Cost ($)",
  #                     outcome=on_outcome)
  
  p <- plot_ly(data = owsa_torn) %>%
    add_trace(x = ~lowval, y = ~variable, 
              base = basecase, 
              type = 'bar', orientation = 'h', name = "lower input",
              marker = list(color = "rgba(166,206,227,0.7)",
                            line = list(color = "rgb(166,206,227)",
                                        width = 2))) %>%
    add_trace(x = ~highval, y = ~variable, 
              base = basecase, 
              type = 'bar', orientation = 'h', name = "higher input",
              marker = list(color = "rgba(31,120,180,0.7)",
                            line = list(color = "rgb(31,120,180)",
                                        width = 2))) %>%
    layout(barmode='overlay') %>% 
    layout(yaxis = list(categoryorder = "array",
                        categoryarray = ~variable,
                        title = ""),
           xaxis = list(zeroline = FALSE,
                        showline = FALSE,
                        showgrid = FALSE
                        #title = axislabel
                        ),
           font = list(size=12, 
                       family='Fira Sans'))
  
  return(p)
  
}


# functions to create intervals for the dirichlet distributions
dir_interval <- function(prior) {
  prior <- unlist(prior)
  posterior <- prior + rep(1, length=length(prior))
  lower <- qgamma(.025,shape=posterior,rate=1)/sum(qgamma(.025,shape=posterior,rate=1))
  upper <- qgamma(.975,shape=posterior,rate=1)/sum(qgamma(.975,shape=posterior,rate=1))
  return(list(lower,upper))
}

# function to generate and name random dirichlet variates
vdirichlet <- function(n, alpha) {
  variates <- MCMCpack::rdirichlet(n, alpha)
  colnames(variates) <- names(alpha)
  return(variates)
}


# this function calls the RunModel function on psalist
# use it to extract the PSA trace of costs and outcomes
genPSA <- function(model, psalist, outcome) {
  
  # get things we need
  basenames <- names(model$baseparms)
  transitnames <- names(model$basetransitions)
  
  psamod <- RunModel(baseparms = psalist[basenames],
                     basetransitions = psalist[transitnames])
  
  psares <- switch(outcome,
                    Incidence = unlist(c(psamod$Incidence[6, 3], psamod$Incidence[6, 6], psamod$Incidence[6, 9:11])),
                    Mortality = unlist(c(psamod$Mortality[6, 3], psamod$Mortality[6, 6], psamod$Mortality[6, 9:11])),
                    DALYs = unlist(c(psamod$DALYS[6, 3], psamod$DALYS[6, 6], psamod$DALYS[6, 9:11])))
  
  return(psares)
  
}


RunPSA <- function(model, psalist, outcome, wtp, by) {
  
  # run PSA: split PSA inputs into lists by row, calculate PSA (map function), and pull costs and effects together into separate tibbles
  psaTrace <- psalist %>%
    purrr::transpose() %>% # convert each row of data into a separate list
    purrr::map(.f = genPSA, model = model, outcome = outcome) %>% # apply function to each dataframe in the list to get CE pairs
    do.call("rbind", .) %>%
    data.frame()
    # pmap(., bind_rows) # pull together costs and outcomes into separate tables for WTP function
  
  # generate scatter plot
  scatterPlot <- ScatterPSA(psaTrace = psaTrace)
  
  # run CEAC analysis
  ceacTrace <- RunCEAC(psaTrace = psaTrace, wtp = wtp, by = by)
  
  # generate CEAC
  ceacPlot <- drawCEAC(ceacTrace = ceacTrace)
  
  return(list(psaTrace = psaTrace, plots = subplot(scatterPlot, ceacPlot),  ceacTrace = ceacTrace))
  
}

# function takes the maxwtp, a vector or dataframe of costs and outcomes for all interventions
# and returns probability that a given intervention is cost effective 
genProbCE <- function(wtp, psaTrace) {
  NMBSoc <- psaTrace[, 1]*wtp - psaTrace[, 2]                   # NMB = DALYs * wtp - COSTS
  NMBGov <- psaTrace[, 1]*wtp - psaTrace[, 3]                   # NMB = DALYs * wtp - COSTS
  pCESoc <- mean(NMBSoc > 0)        # find maximum for each row (nax nmb)
  pCEGov <- mean(NMBGov > 0)         # find maximum for each row (nax nmb)
  return(c(pCESoc = pCESoc, pCEGov = pCEGov))           # calculate probability of cost-effectiveness
}


RunCEAC <- function(psaTrace, wtp, by) {
  
  # probability of cost effectiveness calculations
  probCE <- map_dfr(seq(0, wtp, by = by), .f = genProbCE, psaTrace) %>%
    do.call("cbind", .) %>%
    bind_cols(WTP = seq(0, wtp, by = by), .)
  
  return(probCE)
  
}


ScatterPSA <- function(psaTrace) {
  
  plot_ly() %>%
    add_trace(x = ~psaTrace[, 1], y = ~psaTrace[, 2], 
              type = 'scatter', mode = 'markers', name = "Societal",
              marker = list(symbol = "circle-open", color = col[1], size = 7)) %>%
    
    add_trace(x = ~psaTrace[, 1], y = ~psaTrace[, 3],
              type = 'scatter', mode = 'markers', name = "Government",
              marker = list(symbol = "circle-open", color = col[2], size = 8)) %>% 
    
    layout(xaxis = list(title = 'Incremental Outcome'), yaxis = list(title = 'Incremental Costs'))
}


drawCEAC <- function(ceacTrace) {
  
    plot_ly(ceacTrace, x = ~WTP) %>%
    add_trace(y = ~pCESoc, type = 'scatter', mode = 'lines+markers', name = "Societal",
              line = list(shape = 'spline', color = col[1], width= 1.5),
              marker = list(symbol = "diamond", color = col[1], size = 9)) %>%
    
    add_trace(y = ~pCEGov, type = 'scatter', mode = 'lines+markers', name = "Government",
              line = list(shape = 'spline', color = col[2], width= 1.5),
              marker = list(symbol = "square", color = col[2], size = 8)) %>%
    
    layout(xaxis = list(title = 'Willingness-to-pay'), 
           yaxis = list(title = 'Probability Cost-Effective', range = c(0, 1)),
           legend = list(orientation = 'v'))
}


outcomesPlot <- function(model, outcome) {
  
  # identify which table we need
  whichTab <- switch(outcome,
                     Incidence = model$Incidence,
                     Mortality = model$Mortality,
                     DALYs = model$DALYS)
  
  whichTab <- data.frame(Quintile = factor(c(unlist(QuntileList), "Average"), 
                                           levels = c(unlist(QuntileList), "Average")), data.frame(whichTab))
  
  p1 <- plot_ly(whichTab, x = ~whichTab[, 1], y = ~whichTab[, 2], type = 'bar',
                marker = list(color = col[3],
                              line = list(color = col[3],
                                          width = 2))) %>% 
    add_trace(y = ~whichTab[, 4],
              marker = list(color = col[2],
                            line = list(color = col[2],
                                        width = 2))) %>% 
    layout(yaxis = list(title = 'Probabiity'), 
           barmode = 'stack',
           legend = list(orientation = 'h'))
  
  p2 <- plot_ly(whichTab, x = ~whichTab[, 1], y = ~whichTab[, 5], type = 'bar',
                marker = list(color = col[3],
                              line = list(color = col[3],
                                          width = 2))) %>% 
    add_trace(y = ~whichTab[, 7],
              marker = list(color = col[2],
                            line = list(color = col[2],
                                        width = 2))) %>% 
    layout(yaxis = list(title = 'Societal Costs', range = c(0, 30)), 
           barmode = 'stack',
           legend = list(orientation = 'h'))
  
  p3<- plot_ly(whichTab, x = ~whichTab[, 1], y = ~whichTab[, 8], type = 'bar',
               marker = list(color = col[3],
                             line = list(color = col[3],
                                         width = 2))) %>% 
    add_trace(y = ~whichTab[, 10],
              marker = list(color = col[2],
                            line = list(color = col[2],
                                        width = 2))) %>% 
    layout(yaxis = list(title = 'Governemt Costs', range = c(0, 30)), 
           barmode = 'stack',
           legend = list(orientation = 'h'))
  
  return(subplot(p1, p2, p3))
  
  
}

makeSims <- function(nsims) {
  
  # draw values (everything is in the global environment so should work)
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
  
  # # random dirichlet transitions
  # randdirchinputs <- data %>%
  #   filter(dist == "dirichlet") %>%   # remove NA from data
  #   pull(alpha, variable) %>%
  #   split(., rep(1:2, c(5, 3)))
  # names(randdirchinputs) <- c("Quintiles", "AgeDistribution")
  # 
  # 
  # # generate variates and put in one database
  # randdirichlets <- lapply(randdirchinputs, vdirichlet, n=nsims) %>%
  #   do.call("cbind", .)
  
  fixedinputs <- matrix(rep(baseinputs[fixedparms],each=nsims),nrow=nsims) %>%
    magrittr::set_colnames(., fixedparms)
  
  psa_input <- data.frame(cbind(randbeta, randtnormal, randunif, randlognormal, fixedinputs))
  
  return(psa_input)
  
  
}




# estimate parameters of beta distribution given mean and variance
estBetaParams <- function(mu, var) {
  alpha <- ((1 - mu) / var - 1 / mu) * mu ^ 2
  beta <- alpha * (1 / mu - 1)
  return(list(alpha = alpha, beta = beta))
}








