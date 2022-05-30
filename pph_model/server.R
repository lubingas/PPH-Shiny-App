#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

#library(shiny)

# place all files in the same working directory and change to this directory 
# setwd("../PPH-Shiny-App/pph_model")


# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  output$contents <- renderPrint(
    isolate("Server is ready"))
  
  ModelRun <- reactive({

    # replace the values from the spreadsheet with these before running model
    
    quintiles <- c(input$pLowQuintile, input$pSecQuintile, input$pMidQuintile, input$pFourthQuintile, input$pHighQuintile)
    quintiles <- quintiles/sum(quintiles)
    names(quintiles) <- c("pLowQuintile", "pSecQuintile", "pMidQuintile", "pFourthQuintile", "pHighQuintile")
    agedist <- c(input$isless20, input$is20to34, input$is35to49)
    agedist <- agedist/sum(agedist)
    names(agedist) <- c("isless20", "is20to34", "is35to49")
    otherparms <- c(input$pMisoAccessHomeUnassMisoPolicy, 
                    input$pMisoAccessHomeFriendMisoPolicy, 
                    input$pMisoAccessHomeTBAMisoPolicy,
                    input$pMisoAccessHCMisoPolicy,
                    input$pMisoAccessHospMisoPolicy)
    names(otherparms) <- c("pMisoAccessHomeUnassMisoPolicy", 
                           "pMisoAccessHomeFriendMisoPolicy", 
                           "pMisoAccessHomeTBAMisoPolicy",
                           "pMisoAccessHCMisoPolicy",
                           "pMisoAccessHospMisoPolicy")
    
    baseinputs <- c(baseinputs, otherparms)
    basetransitions <- c(basetransitions, quintiles, agedist)
    
    
    # run basecase model with user input values of parameters
    model <- RunModel(baseparms = baseinputs,
                      basetransitions = basetransitions)
    
  }) %>%
    bindEvent(input$runmodel)
  
  observe({
    reset("inputparms")
    output$contents <- renderPrint("Server is ready")
    }) %>%
    bindEvent(input$resetinputs)
  
  observe({
    output$contents <- renderPrint("Model Complete")
  }) %>%
    bindEvent(input$runmodel)
  

  TabIncidence <- function() {
    Qtiles <- c("Lowest", "Second", "Middle", "Fourth", "Highest")
    data.frame(Quintile = factor(c(Qtiles, "Average"),
                                 levels = c(Qtiles, "Average")), 
               data.frame(round(ModelRun()$Incidence, 4))) %>%
    as_tibble()
  }
  
  TabMortality <- function() {
    Qtiles <- c("Lowest", "Second", "Middle", "Fourth", "Highest")
    data.frame(Quintile = factor(c(Qtiles, "Average"),
                                 levels = c(Qtiles, "Average")), 
               data.frame(round(ModelRun()$Mortality, 4))) %>%
      as_tibble()
  }
  
  TabDALYs <- function() {
    Qtiles <- c("Lowest", "Second", "Middle", "Fourth", "Highest")
    data.frame(Quintile = factor(c(Qtiles, "Average"),
                                 levels = c(Qtiles, "Average")), 
               data.frame(round(ModelRun()$DALYS, 4))) %>%
      as_tibble()
  }
  
  
  output$IncidenceTable <- DT::renderDT({
    TabIncidence()[, c(1, 4, 7, 10:12)]
    
  })
  
  output$IncidencePlots <- renderPlotly({
    
    p1 <- plot_ly(TabIncidence(), x = ~Quintile, y = ~IncidenceMiso, type = 'bar', name = 'Incident cases (Miso)',
                  marker = list(color = col[1],
                                line = list(color = col[1],
                                            width = 2))) %>% 
      add_trace(y = ~deltaIncidence, name = 'Cases Averted',
                marker = list(color = col[2],
                              line = list(color = col[2],
                                          width = 2))) %>% 
      layout(yaxis = list(title = 'Probabiity'), 
             barmode = 'stack',
             legend = list(orientation = 'h'))
    
    p2 <- plot_ly(TabIncidence(), x = ~Quintile, y = ~SocCostsIncidNoMiso, type = 'bar', name = 'Costs (No Miso)',
                  marker = list(color = col[1],
                                line = list(color = col[1],
                                            width = 2))) %>% 
      add_trace(y = ~deltaSocCostsIncid, name = 'Incremental Costs (Miso)',
                marker = list(color = col[2],
                              line = list(color = col[2],
                                          width = 2))) %>% 
      layout(yaxis = list(title = 'Societal Costs', range = c(0, 30)), 
             barmode = 'stack',
             legend = list(orientation = 'h'))
    
    p3 <- plot_ly(TabIncidence(), x = ~Quintile, y = ~GovCostsIncidNoMiso, type = 'bar', name = 'Costs (No Miso)',
                  marker = list(color = col[1],
                                line = list(color = col[1],
                                            width = 2))) %>% 
      add_trace(y = ~deltaGovCostsIncid, name = 'Incremental Costs (Miso)',
                marker = list(color = col[2],
                              line = list(color = col[2],
                                          width = 2))) %>% 
      layout(yaxis = list(title = 'Public Sector Costs', range = c(0, 30)), 
             barmode = 'stack',
             legend = list(orientation = 'h'))
    
    return(subplot(p1, p2, p3))
    
  })
  
  output$MortalityTable <- DT::renderDT({
    TabMortality()[, c(1, 4, 7, 10:12)]
  })
  
  output$MortalityPlots <- renderPlotly({
    
    p1 <- plot_ly(TabMortality(), x = ~Quintile, y = ~mortalityMiso, type = 'bar', name = 'Deaths (Miso)',
                  marker = list(color = col[1],
                                line = list(color = col[1],
                                            width = 2))) %>% 
      add_trace(y = ~deltaMortality, name = 'Cases Averted',
                marker = list(color = col[2],
                              line = list(color = col[2],
                                          width = 2))) %>% 
      layout(yaxis = list(title = 'Probabiity'), 
             barmode = 'stack',
             legend = list(orientation = 'h'))
    
    p2 <- plot_ly(TabMortality(), x = ~Quintile, y = ~SocCostsMiso, type = 'bar', name = 'Costs (Miso)',
                  marker = list(color = col[1],
                                line = list(color = col[1],
                                            width = 2))) %>% 
      add_trace(y = ~deltaSocCosts, name = 'Incremental Costs (No Miso)',
                marker = list(color = col[2],
                              line = list(color = col[2],
                                          width = 2))) %>% 
      layout(yaxis = list(title = 'Societal Costs', range = c(0, 30)), 
             barmode = 'stack',
             legend = list(orientation = 'h'))
    
    p3 <- plot_ly(TabMortality(), x = ~Quintile, y = ~GovCostsMiso, type = 'bar', name = 'Costs (Miso)',
                  marker = list(color = col[1],
                                line = list(color = col[1],
                                            width = 2))) %>% 
      add_trace(y = ~deltaGovCosts, name = 'Incremental Costs (No Miso)',
                marker = list(color = col[2],
                              line = list(color = col[2],
                                          width = 2))) %>% 
      layout(yaxis = list(title = 'Public Sector Costs', range = c(0, 30)), 
             barmode = 'stack',
             legend = list(orientation = 'h'))
    
    
    return(subplot(p1, p2, p3))
    
  })
  

  output$DALYsTable <- DT::renderDT({
    TabDALYs()[, c(1, 4, 7, 10:12)]
  })
  
  output$DALYsPlots <- renderPlotly({
    
    p1 <- plot_ly(TabDALYs(), x = ~Quintile, y = ~DALYsMiso, type = 'bar', name = 'Deaths (Miso)',
                  marker = list(color = col[1],
                                line = list(color = col[1],
                                            width = 2))) %>% 
      add_trace(y = ~deltaDALYs, name = 'Cases Averted',
                marker = list(color = col[2],
                              line = list(color = col[2],
                                          width = 2))) %>% 
      layout(yaxis = list(title = 'Probabiity'), 
             barmode = 'stack',
             legend = list(orientation = 'h'))
    
    p2 <- plot_ly(TabDALYs(), x = ~Quintile, y = ~SocCostsMiso, type = 'bar', name = 'Costs (Miso)',
                  marker = list(color = col[1],
                                line = list(color = col[1],
                                            width = 2))) %>% 
      add_trace(y = ~deltaSocCosts, name = 'Incremental Costs (No Miso)',
                marker = list(color = col[2],
                              line = list(color = col[2],
                                          width = 2))) %>% 
      layout(yaxis = list(title = 'Societal Costs', range = c(0, 30)), 
             barmode = 'stack',
             legend = list(orientation = 'h'))
    
    p3 <- plot_ly(TabDALYs(), x = ~Quintile, y = ~GovCostsMiso, type = 'bar', name = 'Costs (Miso)',
                  marker = list(color = col[1],
                                line = list(color = col[1],
                                            width = 2))) %>% 
      add_trace(y = ~deltaGovCosts, name = 'Incremental Costs (No Miso)',
                marker = list(color = col[2],
                              line = list(color = col[2],
                                          width = 2))) %>% 
      layout(yaxis = list(title = 'Public Sector Costs', range = c(0, 30)), 
             barmode = 'stack',
             legend = list(orientation = 'h'))
    
    
    return(subplot(p1, p2, p3))
    
  })
  
  
  DSARun <- reactive({
    
    quintiles <- c(input$pLowQuintile, input$pSecQuintile, input$pMidQuintile, input$pFourthQuintile, input$pHighQuintile)
    # names(quintiles) <- c("pLowQuintile", "pSecQuintile", "pMidQuintile", "pFourthQuintile", "pHighQuintile")
    dsa_quintiles <- dir_interval(quintiles)
    
    low_quintiles <- dsa_quintiles[[1]]/sum(dsa_quintiles[[1]])
    high_quintiles <- dsa_quintiles[[2]]/sum(dsa_quintiles[[2]])
    names(low_quintiles) <- names(high_quintiles) <- c("pLowQuintile", "pSecQuintile", 
                                                       "pMidQuintile", "pFourthQuintile", 
                                                       "pHighQuintile")
    
    agedist <- c(input$isless20, input$is20to34, input$is35to49)
    names(agedist) <- c("isless20", "is20to34", "is35to49")
    dsa_agedist <- dir_interval(agedist)
    
    low_agedist <- dsa_agedist[[1]]/sum(dsa_agedist[[1]])
    high_agedist <- dsa_agedist[[2]]/sum(dsa_agedist[[2]])
    names(low_agedist) <- names(high_agedist) <- c("isless20", "is20to34", "is35to49")
    
    otherparms <- c(input$pMisoAccessHomeUnassMisoPolicy, 
                    input$pMisoAccessHomeFriendMisoPolicy, 
                    input$pMisoAccessHomeTBAMisoPolicy,
                    input$pMisoAccessHCMisoPolicy,
                    input$pMisoAccessHospMisoPolicy)
    names(otherparms) <- c("pMisoAccessHomeUnassMisoPolicy", 
                           "pMisoAccessHomeFriendMisoPolicy", 
                           "pMisoAccessHomeTBAMisoPolicy",
                           "pMisoAccessHCMisoPolicy",
                           "pMisoAccessHospMisoPolicy")
    
    serr <- sqrt((otherparms * (1-otherparms))/508)
    serr[4:5] <- sqrt((otherparms[4:5] * (1-otherparms[4:5]))/317)
    
    low_otherparms <- otherparms - qnorm(0.975) * serr
    high_otherparms <- otherparms - qnorm(0.975) * serr
    
    low_trans <- list(low_quintiles, low_agedist)
    high_trans <- list(high_quintiles, high_agedist)
    names(low_trans) <- names (high_trans) <- dir_inputs
    
    # run DSA on Incidence
    dsa <- pblapply(outcomes, FUN = owsa, model = ModelRun(),
                    low_base = usa.low, low_transitions = low_trans,
                    high_base = usa.high, high_transitions = high_trans,
                    max_vars = input$max_vars)
    
  }) %>%
    bindEvent(input$rundsa)
  
  output$dsaIncidencePlots <- renderPlotly({
    DSARun()$Incidence
  })
  
  output$dsaMortalityPlots <- renderPlotly({
    DSARun()$Mortality
  })

  output$dsaDALYsPlots <- renderPlotly({
    DSARun()$DALYs
  })
  
  ### now PSA
  PSARun <- reactive({
    
    quintiles <- c(input$pLowQuintile, input$pSecQuintile, input$pMidQuintile, input$pFourthQuintile, input$pHighQuintile)
    names(quintiles) <- c("pLowQuintile", "pSecQuintile", "pMidQuintile", "pFourthQuintile", "pHighQuintile")
    
    agedist <- c(input$isless20, input$is20to34, input$is35to49)
    names(agedist) <- c("isless20", "is20to34", "is35to49")
    
    randinternaldirchinputs <- list(quintiles, agedist)
    names(randinternaldirchinputs) <- dir_inputs
    
    # generate dirichlet variates and put in one database
    randinternaldirichlets <- lapply(randinternaldirchinputs, vdirichlet, n=input$nsims) %>%
      do.call("cbind", .)
    
    otherparms <- c(input$pMisoAccessHomeUnassMisoPolicy, 
                    input$pMisoAccessHomeFriendMisoPolicy, 
                    input$pMisoAccessHomeTBAMisoPolicy,
                    input$pMisoAccessHCMisoPolicy,
                    input$pMisoAccessHospMisoPolicy)
    names(otherparms) <- c("pMisoAccessHomeUnassMisoPolicy", 
                           "pMisoAccessHomeFriendMisoPolicy", 
                           "pMisoAccessHomeTBAMisoPolicy",
                           "pMisoAccessHCMisoPolicy",
                           "pMisoAccessHospMisoPolicy")
    
    serr <- sqrt((otherparms * (1-otherparms))/508)
    serr[4:5] <- sqrt((otherparms[4:5] * (1-otherparms[4:5]))/317)
    
    betaparams <- estBetaParams(mu = otherparms, var = serr)
    
    # internal betas
    randinternalbeta <- mapply(rbeta, MoreArgs=list(n=input$nsims), betaparams$alpha, betaparams$beta)
    
    
    # pool together into one dataframe ready for PSA functions
    psa_input <- data.frame(makeSims(input$nsims), 
                            cbind(randinternalbeta,
                                  randinternaldirichlets))

    # run PSA on everything
    ## PSA
    # run PSA on all outcomes
    psa_all <- pblapply(outcomes, FUN = RunPSA, model = ModelRun(), 
                        psalist = psa_input, wtp = input$maxwtp, by = 100)
    
    
  }) %>%
    bindEvent(input$runpsa)
  
  observe({
    output$psaProgress <- renderPrint("Running PSA")
  }) %>%
    bindEvent(input$runpsa)
  
  
  output$psaIncidencePlots <- renderPlotly({
    PSARun()$Incidence$plots
  })
  
  output$psaMortalityPlots <- renderPlotly({
    PSARun()$Mortality$plots
  })
  
  output$psaDALYsPlots <- renderPlotly({
    PSARun()$DALYs$plots
  })
  

  
})
  

