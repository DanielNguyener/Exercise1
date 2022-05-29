#this is where the code will go
# exercise1 week 5/25/22

library(shiny)
library(shinyWidgets)
library(ggplot2)
library(ggrepel)

ui = fluidPage(
  # accessing css file
  theme = 'styling.css',
  # title
  h1("Exercise 1 - Week 5/25/22"),
  
  #body text description
  br(),
  div('The side bar on the left allows you to search for a specific popuation to display values in a table'),
  div('the main pane on the right allows you to select morphs to display in a plot, the coordinate locations relative to one another'),
  div('download the data for all populations using the download button at the bottom.'),
  br(),
  img(src = 'tetra_image.jpg'),
  br(),
  
  
  sidebarLayout(
    sidebarPanel(id = 'sidebar1',
                 #header
                 h2('Population Search'),
                 searchInput(
                   inputId = "search_id", label = "Enter a population",
                   placeholder = "Population",
                   btnSearch = icon("search"),
                   width = "450px"),
                 
                 #out put reactive population table
                 tableOutput('react_table'),
                 
    ),
    mainPanel(
              #header
              h2('Morph Selection'),
              radioButtons(
                inputId = "selection_id",
                label = "Select a morph",
                choices = NULL,
                selected = NULL,
                choiceNames = c("Cave-Dwelling", "Surface-Dwelling"),
                choiceValues = c("Cave","Surface")
              ),
              #output radiobutton table
              tableOutput("Table_out"),
              
              #output reactive plot
              plotOutput('plot_out'),
              
              #output reactive table for plot dataframe
              verbatimTextOutput('test_text'),
              
              
    )
  ),
  
  
  #busy message
  conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                   tags$div("Crawling...",id="loadmessage")
  ),
  
  #download button
  downloadButton(outputId = "download_this",
                 label = "Click to Download",
                 class = "download")
  
)


#server
server = function(input, output){
  
  #read file to store as data
  df <- read.csv("PopulationLocations.csv")

  output$table_out <- renderTable(
    if (input$radio_button == "Cave") {
      df[df$Morph == "Cave",]
    } else {
      df[df$Morph == "Surface",]
    }
  )
  
  #new data for reactive plot, based on radiobutton input
  data_filtered <- reactive({
    if(input$selection_id == 'Cave'){
      new_df <- df[df$Morph == 'Cave',]
    } else if(input$ selection_id == 'Surface'){
      new_df <- df[df$Morph == 'Surface',]
    }
    return(new_df)
  })
  
  
  #debugging to check if new_df contains correct data
  output$test_text <- renderPrint(req(data_filtered()))

  
  # Generates a plot graph of longitude/loattitude of selected morph
  output$plot_out <- renderPlot({
    req(data_filtered())
  
    #plot using new_df as data.
    ggplot(data_filtered()) +
    geom_point(
      aes(y = Latitude,
          x = Longitude,
          color = Morph)
    ) +
    xlab("Longitude") +
    ylab("Latitude") +
    geom_label_repel(
        aes(y = Latitude,
             x = Longitude,
            label = Population),
        color = "black", fill = "white",
       box.padding = 2,
        max.overlaps = 50
       ) +
    ggtitle("Latitudes and Longitudes of Astynax mexicanus population") +
    theme(panel.grid = element_blank(), plot.title = element_text(hjust = 0.5))

  })
  
  
  #reactive table generation
  reactive_table <- eventReactive(input$search_id, valueExpr = {
    if(input$search_id == ""){
      data.frame()
    }else{
      df[grepl(input$search_id, df$Population),]
    }
  })
  
  output$react_table <- renderTable(reactive_table())
  
  #download button
  output$download_this <- downloadHandler(
    filename = function(){
      paste("Table_Download_",Sys.Date(),".csv", sep = '')
    },
    content = function(file){
      write.csv(df, file, row.names = FALSE)
    }
  )
}



shinyApp(ui = ui, server = server)
