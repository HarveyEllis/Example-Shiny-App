#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

#### to do ####
# Table in tabs
# Add option for showing all types of flowers
# Add theme for whole app
# Brushing table under the graph
# Change defaults to something that is different
# Table underneath for selections
# Button for download
# Button for sample
# Scale input widget - zoom and not zoom
# Editing data in a table
# Use the data table library and replace the data table here
# Make selections dependent on what the other selection is - i.e. the x and y axis can't be the same!
#### Advanced features
# Modules
# Add back and forward buttons functionality - see Dean Attalis website
# Break up the model into parts
# Use render cached graph
# use async plots
# Popups as found here: 

library(shiny)
library(tidyverse)
library(shinythemes)

# globals
data_choices <-  tibble("names" = c("Sepal Length","Sepal Width","Petal Length","Petal Width", "Species"), 
                            "values" = c("Sepal.Length", "Sepal.Width", "Petal.Length","Petal.Width", "Species"), stringsAsFactors = F)

spec_choices <- c("Setosa" = "setosa","Versicolor" = "versicolor","Virginica" = "virginica","All" = "all")

iris_data <- as_tibble(iris)

# Define UI for application that draws a histogram
ui <- fluidPage(
  theme = shinytheme("united"),
   
   # Application title
   titlePanel("Iris data app"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
     sidebarPanel(
        conditionalPanel(condition="input.Tabs == 'Plot'",
                         selectInput(inputId = "x_axis",
                                     label = "X-axis:",
                                     choices =  setNames(data_choices$values, data_choices$names)[1:4],
                                     selected = "Sepal.Length"
                         ),
                         selectInput(inputId = "y_axis",
                                     label = "Y-axis:",
                                     choices =  setNames(data_choices$values, data_choices$names)[1:4],
                                     selected = "Sepal.Width"
                         ),
                         selectInput(inputId = "species",
                                     label = "Species:",
                                     choices =  c("Setosa" = "setosa",
                                                  "Versicolor" = "versicolor",
                                                  "Virginica" = "virginica",
                                                  "All" = "all"
                                     ),
                                     selected = "setosa"
                         ),
                         downloadButton(outputId = "graphdownload",
                                        label = "Download graph")),
        
        
        conditionalPanel(condition="input.Tabs == 'Table'",
                         selectizeInput(inputId = "dataselected",
                                        label = "Select data columns",
                                        choices = setNames(data_choices$values, data_choices$names), multiple = TRUE,
                                        options = list(placeholder = 'Select categories to display', plugins = list('remove_button', 'drag_drop'))),
                         downloadButton(outputId = "datadownload",
                                        label = "Download data")),
        
        
        conditionalPanel(condition="input.Tabs == 'Sample'",
                         numericInput(inputId = "nsamples", label = "Number of samples", min = 1, value = 1),
                         selectizeInput(inputId = "dataselected",
                                        label = "Select data columns",
                                        choices = setNames(data_choices$values, data_choices$names), multiple = TRUE,
                                        options = list(placeholder = 'Select categories to sample')),
                         actionButton(inputId = "sampledata", 
                                      label = "Draw sample"),
                         br(), br(),
                         downloadButton(outputId = "sampledownload",
                                        label = "Download sample"))
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
        tabsetPanel(id = "Tabs",
          tabPanel("Plot", br(), plotOutput("plot",  click = "plot_click",brush = brushOpts(id = "plot_brush")),
                   br(), hr(), p("Brush over points for more info"), 
                   DT::dataTableOutput("selectTable")),
          tabPanel("Table", br(), DT::dataTableOutput("datatable")), # this is the DT library version! not the normal renderTable!
          tabPanel("Sample",{})
          )
        
      )
   )
)

# Define server logic
server <- function(input, output) {
  
  # when you have lots of values - use reactive values! This functions like a normal list, except it updates when the inputs update
  values <- reactiveValues()
  observe({
    values$x_name <- filter(data_choices, values == input$x_axis)[["names"]]
  })
  
  observe({
    values$y_name <- filter(data_choices, values == input$y_axis)[["names"]]
  })
  
  observe({
    values$iris_graph_data <- iris_data %>%
      select(input$x_axis, input$y_axis, Species) %>%
     {if(input$species != "all")filter(., Species == input$species) %>% select(-Species) else .}
  })
  
  output$plot <- renderPlot({
      # plot - notice no parentheses after data - we don't have to call it because it is stored in the reactive value, normally we would!
      ggplot(values$iris_graph_data, aes_string(x = input$x_axis, y = input$y_axis, color = ifelse(input$species == "all", "Species", "NULL"))) + #pass NULL if species is not all
        geom_point() + #if(input$species == "all") aes_string(color = "Species") # you can do it this way as well
        labs(x = values$x_name, y = values$y_name) +
        theme_bw()
   })
  
  output$selectTable <- DT::renderDataTable({
    brushedPoints(values$iris_graph_data, input$plot_brush) %>%
     rename(!!values$x_name := input$x_axis, !!values$y_name := input$y_axis)
    })
  
  output$datatable <- DT::renderDataTable({
    req(input$dataselected)
    select(iris_data, !!(data_choices %$% slice(.,match(input$dataselected,.$values)) %$% setNames(.$values, .$names)))
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

