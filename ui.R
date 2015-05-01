library(shiny)
library(ggvis)
library(rCharts)
library(lattice)


shinyUI(navbarPage("NEG/ECP Scenario Analysis",
     tabPanel("New England CO2e Emissions",
      fluidRow(
       column(3,
        wellPanel(
         uiOutput("c1"),
         br()
     )
    ),
       column(9,
        wellPanel(h4("Total New England CO2e Emissions",align="center"),style="width: 75%;",
         ggvisOutput("plot1")
     )
    )
   )
  ),
    tabPanel("Wedges Analysis",
      fluidRow(
       column(3,
        wellPanel(
         uiOutput("c2")
     ),
        wellPanel(
         uiOutput("c3")
     ),
        wellPanel(
         uiOutput("c4")
     ),
        wellPanel(
        uiOutput("c5")
     ),
        wellPanel(
        uiOutput("c6")
     )
   ),
       column(9,
        wellPanel(h4("Wedges",align="center"),style="width: 75%; height: 125%;",
        #includeHTML("svgFiller.js"),
        #reactiveSvg(outputId = "gridPlot") 
        plotOutput("gridPlot")
     )
    )
   )
  )
 ) 
) 
 
 












# style="width: 100%; height: 100%; position: relative; padding: 1000px;",