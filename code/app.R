library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)

# Define UI
ui <- fluidPage(
  
  # App title
  titlePanel("Monthly Expenses Calculator"),
  
  # Input fields
  sidebarLayout(
    sidebarPanel(
      selectInput("month", "Select Month:",
                  c("January", "February", "March", "April", "May", "June",
                    "July", "August", "September", "October", "November", "December")),
      numericInput("rent", "Monthly Rent:", min = 0, value = 0),
      numericInput("utilities", "Utilities:", min = 0, value = 0),
      numericInput("groceries", "Groceries:", min = 0, value = 0),
      numericInput("transportation", "Transportation:", min = 0, value = 0),
      numericInput("entertainment", "Entertainment:", min = 0, value = 0),
      actionButton("submit", "Submit Expenses")
    ),
    
    # Output fields
    mainPanel(
      h3("Monthly Expenses:"),
      verbatimTextOutput("monthly_expenses"),
      plotOutput("expenses_plot")
    )
  )
)

# Define server
server <- function(input, output) {
  
  # Create expenses dataframe
  expenses <- reactiveValues(data = data.frame(month = character(), rent = numeric(),
                                               utilities = numeric(), groceries = numeric(),
                                               transportation = numeric(), entertainment = numeric()))
  
  # Calculate total expenses for selected month
  total <- reactive({
    input$rent + input$utilities + input$groceries + input$transportation + input$entertainment
  })
  
  # Output monthly expenses
  output$monthly_expenses <- renderText({
    paste("Total Expenses for", input$month, ":", "$", formatC(total(), format = "d", big.mark = ","), sep = " ")
  })
  
  # Update expenses dataframe
  observeEvent(input$submit, {
    expenses$data <- rbind(expenses$data, data.frame(month = input$month, rent = input$rent,
                                                     utilities = input$utilities, groceries = input$groceries,
                                                     transportation = input$transportation, entertainment = input$entertainment))
  })
  
  # Output expenses plot
  output$expenses_plot <- renderPlot({
    expenses$data %>% 
      pivot_longer(cols = -month, names_to = "expense_category", values_to = "expense") %>% 
      mutate(month = factor(month, levels = c("January", "February", "March", "April", "May", "June",
                                              "July", "August", "September", "October", "November", "December"))) %>%
      ggplot(aes(x = month, y = expense, fill = expense_category)) +
      geom_col() +
      labs(title = "Monthly Expenses", x = "Month", y = "Expenses", fill = "Expense Category") +
      scale_fill_brewer(type = "qual", palette = "Set3")
  })
}

# Run the app
shinyApp(ui = ui, server = server)
