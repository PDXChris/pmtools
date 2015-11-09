library(ggplot2)
library(grid)
library(plyr)

shinyServer(function(input, output) {
  
  output$watVars <- renderUI({
    if (input$select1 == 'Habitat') vars <- list("Wood Volume" = 'v1tm100', 
                                                 "Large Riparian Canopy" = 'xcl', 
                                                 "Embeddedness" = 'xcembed', 
                                                 "Riparian Disturbance"  = 'xcb_hall', 
                                                 "Bank Hardening" = 'bankhard')
    if (input$select1 == 'Water Quality') vars <- levels(wq14$metric_name)
    if (input$select1 == 'Biological') vars <- c('Fish Index of Biotic Integrity',
                                                 'Macroinvertebrate Observed/Expected',
                                                 'Bird Integrity Index')
    
    selectInput("watVars", label = h4("Choose Indicator"), vars)
  })

  output$inwatVars <- renderUI({
    if (input$select3 == 'Habitat') vars <- list("Wood Volume" = 'v1tm100', 
                                                 "Large Riparian Canopy" = 'xcl', 
                                                 "Embeddedness" = 'xcembed', 
                                                 "Riparian Disturbance"  = 'xcb_hall', 
                                                 "Bank Hardening" = 'bankhard')
    if (input$select3 == 'Water Quality') vars <- levels(wq14$metric_name)
    if (input$select3 == 'Biological') vars <- c('Fish Index of Biotic Integrity',
                                                 'Macroinvertebrate Observed/Expected',
                                                 'Bird Integrity Index')
    
    html <- selectInput("inwatVars", label = h4("Choose Indicator"), choices = vars)
    if (input$select3 == 'Water Quality') {
      html <- gsub('select id="inwatVars"', 'select id="inwatVars", size="3"', html)
    }
    html
    
  })

# Fill in the spot we created for a plot
output$watPlot <- renderPlot({
  
  if(input$select1 == 'Water Quality') plotWQbyWshd(input$watVars)  
  if(input$select1 == 'Habitat') plotHabByWshd(input$watVars)
  if(input$watVars == 'Macroinvertebrate Observed/Expected') plotOep5_wat() 
  if(input$watVars == 'Fish Index of Biotic Integrity') plotIBI_wat()
  if(input$watVars == 'Bird Integrity Index') plotBII_wat()     
  
  })
  
  output$inwatPlot <- renderPlot({
    
    if (input$select3 == 'Water Quality') plotWQ_inwat(input$inwatVars, input$select2)
    if(input$select3 == 'Habitat') plotHab_inwat(input$inwatVars, input$select2)
    if(input$inwatVars == 'Macroinvertebrate Observed/Expected') plotOep5_inwat(input$select2) 
    if(input$inwatVars == 'Fish Index of Biotic Integrity') plotIBI_inwat(input$select2)
    if(input$inwatVars == 'Bird Integrity Index') plotBII_inwat(input$select2)
  })
})