'Title: Lecture 3 - Introduction to Shiny: Interactive Web Apps with R
  Author: Jordan Simonov
  Date: 2025-11-25
'
" Participant name:  "
library(shiny)
library(data.table)

# test 

# ---- UI ----
ui <- fluidPage(
  titlePanel("Simple Tax Microsimulation"),
  
  sidebarLayout(
    sidebarPanel(
      numericInput(
        inputId = "n",
        label   = "Number of taxpayers:",
        value   = 1000,
        min     = 100,
        max     = 100000,
        step    = 1000
      ),
      
      sliderInput(
        inputId = "tax_rate",
        label   = "Flat tax rate (%):",
        min     = 0,
        max     = 50,
        value   = 20
      ),
      
      sliderInput(
        inputId = "mean_income",
        label   = "Average gross income:",
        min     = 5000,
        max     = 100000,
        value   = 30000,
        step    = 5000
      )
    ),
    
    mainPanel(
      plotOutput("incomePlot"),
      tableOutput("summaryTable")
    )
  )
)

# ---- SERVER ----
server <- function(input, output, session) {
  
  # Reactive artificial tax data
  tax_data <- reactive({
    set.seed(123)  # for reproducibility
    
    gross <- rlnorm(
      n       = input$n,
      meanlog = log(input$mean_income) - 0.5,
      sdlog   = 0.6
    )
    
    rate <- input$tax_rate / 100
    tax  <- gross * rate
    net  <- gross - tax
    
    data.frame(
      gross_income = gross,
      tax          = tax,
      net_income   = net
    )
  })
  
  # Histogram of net income
  output$incomePlot <- renderPlot({
    dat <- tax_data()
    
    hist(
      dat$net_income,
      breaks = 30,
      main   = "Net Income Distribution Across Taxpayers",
      xlab   = "Net income",
      col    = "darkgray",
      border = "white"
    )
  })
  
  # Simple summary table 
  output$summaryTable <- renderTable({
    dat <- tax_data()
    
    data.frame(
      Indicator = c(
        "Average gross income",
        "Average net income",
        "Average tax",
        "Total tax revenue"
      ),
      Value = c(
        mean(dat$gross_income),
        mean(dat$net_income),
        mean(dat$tax),
        sum(dat$tax)
      )
    )
  }, digits = 0)
}

# ---- RUN APP ----
shinyApp(ui = ui, server = server)
