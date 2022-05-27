#this is where the code will go
# exercise1 week 5/25/22

library(shiny)
library(shinyWidgets)
library(ggplot2)
library(ggrepel)

ui = fluidPage(
  
  plotOutput('plot_out')
)


server = function(input, output){
  
  df <- read.csv("PopulationLocations.csv")
  caveset <- table(df$Morph == "Cave")
  surfaceset <- table(df$Morph == "Surface")
  
  
  
  bar_plot <- ggplot(data = df, aes(x=Morph)) +
  geom_bar(aes(fill = Morph),
           width = 0.5
  )
  
  output$plot_out <- renderPlot(bar_plot)
  
  
  
}


shinyApp(ui = ui, server = server)