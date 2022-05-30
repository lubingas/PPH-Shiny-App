#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

#library(shiny)
#library(shinythemes)
#library(plotly)

# # Define UI 
shinyUI(tagList(useShinyjs(),  # Include shinyjs
                navbarPage("PPH Health Economic Model",
             tabPanel("About",
                      tabsetPanel(
                        tabPanel("Description", br(), br(),
                                 "Potential Cost-Effectiveness of Prenatal Distribution of Misoprostol for Prevention of Postpartum Hemorrhage in Uganda",
                                 br(), br(),
                                 "In settings where home birth rates are high, prenatal distribution of misoprostol could 
                                 increase access to uterotonics during the third stage of labor to prevent postpartum hemorrhage (PPH). 
                                 This decision analytic model is designed to utilize data from the demographic health surveys to track the likely delivery pathway of 
                                 a woman. Depending on her social economic status, a women may deliver either in a health facility or at home, 
                                 with or without assistance of a friend or a traditional birth attendant. She may or may not experience post-partum hemorrhage.
                                 The model applied differential uterotonic coverage probabilities depending on the setting of delivery that impact the likelihood of PPH.
                                 The model then tracks outcomes of potential misuse of misoprostol, PPH, access to emergency obstetric care (EmOC) and death.",
                                 br(), br(),
                                 "The model then estimates costs and outcomes under a prenatal distribution policy versus a no misoprostol policy, assuming intervention 
                                 would increase the coverage rates of uterotonics."),
                        tabPanel("Inputs")
                      )
                      ),
             tabPanel("Basecase",
                      sidebarLayout(
                        sidebarPanel("User defined parameters",
                                     div(id = "inputparms",
                                         tabsetPanel(
                                           tabPanel("Wealth quintiles",
                                                    helpText(h6("Enter number of women in each wealth quintile from DHS survey")),
                                                    numericInput("pLowQuintile", label = "Lowest", value = 1813),
                                                    numericInput("pSecQuintile", label = "Second", value = 1728),
                                                    numericInput("pMidQuintile", label = "Middle", value = 1617),
                                                    numericInput("pFourthQuintile", label = "Fourth", value = 1426),
                                                    numericInput("pHighQuintile", label = "Highest", value = 1497)
                                           ),
                                           tabPanel("Age distribution",
                                                    helpText(h6("Enter age distribution of women")),
                                                    numericInput("isless20", label = "< 20", value = 1352),
                                                    numericInput("is20to34", label = "20 - 34", value = 5633),
                                                    numericInput("is35to49", label = "35 - 49", value = 1093)
                                           ),
                                           tabPanel("Misoprostol access",
                                                    helpText("Misoprostol coverage probabilities"),
                                                    sliderInput("pMisoAccessHomeUnassMisoPolicy", label = "unassisted", min = 0, max = 1, value = 0.911),
                                                    sliderInput("pMisoAccessHomeFriendMisoPolicy", label = "if assisted by friend/relative", min = 0, max = 1, value = 0.911),
                                                    sliderInput("pMisoAccessHomeTBAMisoPolicy", label = "assisted by TBA", min = 0, max = 1, value = 0.911),
                                                    sliderInput("pMisoAccessHCMisoPolicy", label = "at HC", min = 0, max = 1, value = 0.744),
                                                    sliderInput("pMisoAccessHospMisoPolicy", label = "at hospital", min = 0, max = 1, value = 0.744)
                                           )
                                         )
                                     ),
                                     actionButton("runmodel", label = "Run Model"),
                                     actionButton("resetinputs", label = "Reset Inputs"),
                                     hr()
                        ),
                        mainPanel("Results/Output",
                                  verbatimTextOutput("contents"),
                                  tabsetPanel(
                                    tabPanel("Incidence",
                                             DT::dataTableOutput(outputId = "IncidenceTable"),
                                             plotlyOutput(outputId = "IncidencePlots")),
                                    tabPanel("Mortality",
                                             DT::dataTableOutput(outputId = "MortalityTable"),
                                             plotlyOutput(outputId = "MortalityPlots")),
                                    tabPanel("DALYs",
                                             DT::dataTableOutput(outputId = "DALYsTable"),
                                             plotlyOutput(outputId = "DALYsPlots"))
                                  )
                                  ) 
                      )
             ),
             tabPanel("Sensitivity analyses",
                      tabsetPanel(
                        tabPanel("Deterministic",
                                 sidebarLayout(
                                 sidebarPanel("Settings",
                                              sliderInput("max_vars", label = "How may variables in Tornado Plots?", min = 10, max = 25, value = 15),
                                              actionButton("rundsa", label = "Run DSA")
                                              ),
                                 mainPanel("Results",
                                           verbatimTextOutput("dsaProgress"),
                                           tabsetPanel(
                                             tabPanel("Incidence",
                                                      plotlyOutput(outputId = "dsaIncidencePlots")
                                                      ),
                                             tabPanel("Mortality",
                                                      plotlyOutput(outputId = "dsaMortalityPlots")
                                                      ),
                                             tabPanel("DALYs",
                                                      plotlyOutput(outputId = "dsaDALYsPlots")
                                                      )
                                           )
                                           )
                                 )
                                 ),
                        tabPanel("Probabilitic",
                                 sidebarLayout(
                                 sidebarPanel("Settings",
                                              sliderInput("nsims", label = "How many simulations", min = 50, max = 5000, value = 1000),
                                              sliderInput("maxwtp", label = "Maximum willingness-to-pay", min = 1000, max = 5000, value = 2000),
                                              actionButton("runpsa", label = "Run PSA")
                                              ),
                                 mainPanel("Results",
                                           verbatimTextOutput("psaProgress"),
                                           tabsetPanel(
                                             tabPanel("Incidence",
                                                      plotlyOutput(outputId = "psaIncidencePlots")),
                                             tabPanel("Mortality",
                                                      plotlyOutput(outputId = "psaMortalityPlots")),
                                             tabPanel("DALYs",
                                                      plotlyOutput(outputId = "psaDALYsPlots"))
                                           ))
                                 )
                                 )
                      )
                      )
  )
  
)
)
#   theme = shinytheme("default"),



# # Define UI 
# shinyUI(fluidPage(
#   #theme = shinytheme("default"),
#   navbarPage("PPH Economic Model",
#              tabPanel("About"),
#              tabPanel("Model",
#                       sidebarPanel(
#                         HTML("Model Parameters"),
#                         tabPanel(id = "tabset",
#                                     tabPanel("Wealth quintiles",
#                                              helpText(h6("Enter number of women in each wealth quintile from DHS")),
#                                              numericInput("pLowQuintile", label = h6("Lowest"), value = 1813),
#                                              numericInput("pSecQuintile", label = h6("Second"), value = 1728),
#                                              numericInput("pMidQuintile", label = h6("Middle"), value = 1617),
#                                              numericInput("pFourthQuintile", label = h6("Fourth"), value = 1426),
#                                              numericInput("pHighQuintile", label = h6("Highest"), value = 1497)
#                                     ),tabPanel("Age Distribution",
#                                                helpText(h6("Enter age distribution of women")),
#                                                numericInput("isless20", label = h6("Young"), value = 1352),
#                                                numericInput("is20to34", label = h6("Middle"), value = 5633),
#                                                numericInput("is35to49", label = h6("Old"), value = 1093)
#                                     ),
#                                     tabPanel("Misoprostol access",
#                                              helpText(h6("Misoprostol coverage probabilities")),
#                                              numericInput("pMisoAccessHomeUnassMisoPolicy", label = h6("if unassisted"), value = 0.911),
#                                              numericInput("pMisoAccessHomeFriendMisoPolicy", label = h6("if assisted by friend/relative"), value = 0.911),
#                                              numericInput("pMisoAccessHomeTBAMisoPolicy", label = h6("assisted by TBA"), value = 0.911),
#                                              numericInput("pMisoAccessHCMisoPolicy", label = h6("at HC"), value = 0.744),
#                                              numericInput("pMisoAccessHospMisoPolicy", label = h6("at hospital"), value = 0.744)
#                                     ),
#                                     
#                         )#,
#                         actionButton("runmodel", label = "Run Model", icon("sync")) 
#                       ),
#                       mainPanel(
#                         tags$label("Results"),
#                         #verbatimTextOutput("contents"),
#                         tabsetPanel(
#                           tabPanel("Other model inputs",
#                                    
#                           ),
#                           tabPanel("Incidence",
#                                    DT::dataTableOutput(outputId = "IncidenceTable"),
#                                    plotlyOutput(outputId = "IncidencePlots")
#                                    
#                           ),
#                           tabPanel("Mortality",
#                                    DT::dataTableOutput(outputId = "MortalityTable"),
#                                    plotlyOutput(outputId = "MortalityPlots")
#                           ),
#                           tabPanel("DALYs"),
#                           DT::dataTableOutput(outputId = "DALYsTable")
#                         )
#                       )
#                       
#              )
#   )
# )
# )