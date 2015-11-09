library(shiny)
library(reshape)

shinyUI(navbarPage(
  title = 'Plot Indicators of Portland Watershed Health',
  tabPanel("Compare Watersheds", 
           # Define UI for application that draws a histogram
           # shinyUI(fluidPage(
           
           # Application title
           titlePanel("Compare Watershed Health"),
           
           # Lay out UI
           sidebarLayout(
             sidebarPanel(
               selectInput("select1", label = h4("What type of indicator would you like to plot?"), 
                           choices = list("Habitat", "Water Quality", "Biological"), 
                           selected = "Habitat"),
               hr(),
               
               uiOutput("watVars")
             ),
             
             
             # Show a plot of the generated distribution
             mainPanel(
               plotOutput("watPlot", height = "500px")
             )
           )
  ),
  tabPanel("Compare Stations within a Watershed",
           
           # Application title
           titlePanel("Compare indicators within a watershed"),
           
           # Lay out UI
           sidebarLayout(
             sidebarPanel(
               selectInput("select2", label = h4("Which watershed would you like to plot?"), 
                           choices = sort(unique(stat.info3$watershed))),
#                hr(),
               selectInput("select3", label = h4("What type of data would you like to plot?"), 
                           choices = list("Habitat", "Water Quality", "Biological"), 
                           selected = "Habitat"),
#                hr(),
               
               uiOutput("inwatVars")
             ),
             
             # Show a plot of the generated distribution
             mainPanel(
               plotOutput("inwatPlot", height = "500px")
             )
           )
  )
)
)

