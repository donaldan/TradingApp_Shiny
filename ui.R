###################################
# UI functions
# 
# Andrew Donaldson 2016
###################################

theDates <- seq(as.Date("2007-01-01"), Sys.Date(), by="months")

shinyUI(
  fluidPage(
    headerPanel(""),
    
    sidebarPanel(
      selectInput("strategy", 
                  "Strategy:", 
                  choices = c("Commodities", "US 10Y Bond", 
                              "Precious_Metals", "S&P_500", "Portfolio")),
      
      selectInput("startDate", 
                  "Start:", 
                  choices = theDates),
      
      selectInput("endDate", 
                  "End:", 
                  choices = theDates,
                  selected = theDates[length(theDates)]), 
      
      div(HTML("<br>This app was created by <a href='waverley.inc@gmail.com ,Mob +61 (0)488222795' target='_blank'>Andrew Donaldson</a>.
               <br>Powered by <a href='http://shiny.rstudio.com/' target='_blank'>Shiny</a>"))
      ),
    
    mainPanel(  
      tabsetPanel(
        ## OVERVIEW
        tabPanel("Overview", plotOutput("plot1",height = 550, width = 550)),
        
        ## TRADING STATISTICS
        tabPanel("Trading Statistics",
                 tableOutput("tablePerformance")
                 # fixedRow(
                 #   column(8,
                 #          fixedRow(column(1,tableOutput("tablePerformance"))
                 #          # ,         column(4,tableOutput("tableRisk"))),
                          # fixedRow(column(4,tableOutput("tableDaily")),
                          #          column(4,tableOutput("tableMonthly"))))
                 # )))
        ),
        ## RISK
        tabPanel("Risk",plotOutput("plot2",height = 550, width = 550)),
        
        ## HOW TO
        tabPanel("Info",
                 HTML("<p> This Shiny application example is designed to help analyse trading strategies and is by no means exhaustive</p>
<b>A simple data visualisation tool to look at returns by Asset Class over time </b>

                    "))
      )  
    )
    ))

