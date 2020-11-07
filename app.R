#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)
library(sparklyr)
library(dplyr)
library(ggplot2)
library(reshape2)
library(tidyr)
library(stringr)
library(DT)
source('coindeskAPI.R')

#Connect to Spark
spark_install(version = "2.1.0")
sc <- spark_connect(master = "local")
#Get Historic data and reshape it 
historic_data <- getHistoricPrice(start = '2010-07-17', end = today())
colnames(historic_data) <- str_replace_all(colnames(historic_data), "bpi.", '')
historic_data <- melt(historic_data)
historic_data <- historic_data %>%
  select(variable, value) %>%
  rename('Date' = variable, 'Price' = value)
historic_data$Date <- ymd(historic_data$Date)

# Define UI for application 
ui <- fluidPage(theme = shinytheme("readable"),
                titlePanel("Bitcoin Price Explorer"),
                sidebarLayout(
                  sidebarPanel(
                    # Select data source
                    selectInput(inputId = "type", label = strong("Data Source"),
                                choices = c("Historic" = "his", "Real-time(not working)" = "rt"),
                                selected = "his"),
                    
                    # Select date range to be plotted
                    dateRangeInput("date", strong("Date range"), start = "2010-07-17", end = today(),
                                   min = "2010-07-17", max = today()),
                    
                    # Select whether to overlay smooth trend line
                    checkboxInput(inputId = "smoother", label = strong("Overlay smooth trend line"), value = FALSE),
                    
                    # Display only if the smoother is checked
                    conditionalPanel(condition = "input.smoother == true",
                                     sliderInput(inputId = "f", label = "Smoother span:",
                                                 min = 0.01, max = 1, value = 0.67, step = 0.01,
                                                 animate = animationOptions(interval = 100)),
                                     HTML("Higher values give more smoothness.")
                    ),
                    br(),
                    checkboxInput(inputId = 'logscale', label = strong('Change to log y-axis'), value = FALSE)
                    
                  ),
                  
                  # Output: Description, scatterplot, and reference
                  mainPanel(
                    tabsetPanel(
                      tabPanel("Plot",
                               plotOutput(outputId = "scatterplot"),
                               br(),
                               verbatimTextOutput(outputId = "plotdesc")),
                      tabPanel("Table",
                               DT::dataTableOutput(outputId = 'table'),
                               br(),
                               verbatimTextOutput(outputId = 'tabledesc')),
                      tabPanel("Real-time Price",
                               verbatimTextOutput(outputId = 'realtime'),
                               br(),
                               verbatimTextOutput(outputId = 'realtimedesc'))
                    ),
                    br(),
                    br(),
                    br(),
                    
                    tags$footer(a(href = "https://www.coindesk.com/price/", "Powered by CoinDesk",
                                  target = "_blank"), style = 'float:right')
                  )
      )
)

# Define server logic 
server <- function(input, output, session) {
  #browser()
  selected_trends <- reactive({
    req(input$date)
    #browser()
    shiny::validate(need(!is.na(input$date[1]) && !is.na(input$date[2]), "Error: Please provide both a start and an end date."))
    shiny::validate(need(ymd(input$date[1]) < ymd(input$date[2]), "Error: Start date should be earlier than end date."))
    #browser()
    historic_data %>%
      filter(
        #type == input$type,
        Date > ymd(input$date[1]) & Date < ymd(input$date[2]
        ))
  })

  
  
  ranges <- reactiveValues(x = NULL, y = NULL)
  # Create scatterplot object the plotOutput function is expecting
  output$scatterplot <- renderPlot({
    #browser()
    p <- ggplot(selected_trends(),
                aes(x = selected_trends()$Date, y = selected_trends()$Price)) +
      geom_point(size = .75) +
      #scale_y_log10() +
      #geom_smooth(method = 'lm') +
      labs(
        title = paste0('Bitcoin Price from ', input$date[1], ' to ', input$date[2]),
           x = 'Date',
           y = 'Actual Price') +
      scale_x_date() +
      coord_cartesian(xlim = ranges$x , ylim = ranges$y, expand = F)

    # Display only if smoother is checked
    if(input$smoother){
      #browser()
      p <- p + geom_smooth(method = 'auto', span = input$f)
    }
    if(input$logscale){
      #browser()
      p <- p + scale_y_log10() +
        labs(y = 'Log Price')
    }
    p
  })

  # Help notes to show plot
  output$plotdesc <- renderText({
    paste("Notes:","1) Some actions may take a long time given a large date range.",
          "2) Ensure the date range is wide enough to apply smooth trend lines.", sep = "\n")
  })
  
  # reative data table
  output$table <- DT::renderDataTable({
    datatable(selected_trends(), options = list(order = list(2,'desc')))
    
  })

  # Help notes to show table
  output$tabledesc <- renderText({
    paste("Notes:","1) Ordered by descending price.",
           sep = "\n")
  })
 
  
  output$realtime<- renderText({
    invalidateLater(5000, session)
    curPrice <- getCurrentPrice()$bpi.USD.rate_float
    paste0("As of ",format(Sys.time()) ,', the current price of Bitcoin is: ', "\n", curPrice,"\n")
  })
  
  output$realtimedesc <- renderText({
    paste("The price is updated every 5 seconds by making call to CoinDesk API.")
  })
     
}
  

# Run the application 
shinyApp(ui = ui, server = server)

