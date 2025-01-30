library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Old Faithful Geyser Data"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "Model",
                  label = "Select a model:",
                  choices = unique(exp_results$Model), # Populate with distinct values
                  selected ="Llama")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotlyOutput("plot1")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  countdata<-reactive({
    countdata<-exp_results%>%
      filter(Model == input$Model)%>%
      transmute(Primary_Cat,ResponseAuth,ResponseTitle)%>%
      group_by(Primary_Cat)%>%
      summarise(CountAuth=sum(ResponseAuth), CountTitle=sum(ResponseTitle), .groups = 'drop')%>%
      pivot_longer(CountAuth:CountTitle, names_to = "Type", values_to = "Count")
    return(countdata)
    })
  
  output$plot1 <- renderPlotly({
    data<-countdata()
    p<-data%>%plot_ly(
      x = ~Primary_Cat, 
      y = ~Count/200, 
      type = "bar",
      color = ~Type, 
      colors = "Set3" 
    )%>%
      layout(
        title = paste("Histogram of B vs. C for category:", input$Model),
        xaxis = list(title = "Category"),
        yaxis = list(title = "Count"),
        barmode = "group"
      )
    return(p)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
