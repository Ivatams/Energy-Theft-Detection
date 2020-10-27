library(shiny)
library(stats)
library(caret)

shinyUI(fluidPage(
    titlePanel("Customer Meter Bypass Prediction"),
    title = "Customer Bypass Prediction",
    p(em("Help:", a("How to use this App", href="index.html"))),
    sidebarLayout(
        sidebarPanel(
            fileInput("file1", "Choose a CSV file to import:",
                      accept = c("text/csv", "text/comma-separated-values, text/plain", ".csv")),
            
            tags$hr(),
            h5("Or fill in the following fields:"),
            numericInput(inputId = "Average.Consumption",
                         label =   "Average Consumption (Kwh)",
                         value = 10),
            numericInput(inputId = "Availability",
                         label = "Availability (hrs)",
                         value = 600),
            numericInput(inputId = "Estimated.Connected.Load",
                         label = "Estimated Connected Load (kw)",
                         value = 0.00),
            selectInput(inputId = "group_cons",
                        label = "Consumption Range",
                        choices = c("0-1846", "1846-3692", "3692-7384", "7384-9233")),
            selectInput(inputId = "MeterBypass",
                        label = "Meter Feature",
                        choices = c("high", "Low")),
            selectInput(inputId = "FeederBypass",
                        label = "Feeder Feature",
                        choices = c("high", "Low")),
            selectInput(inputId = "TariffBypass",
                        label = "Tariff Feature",
                        choices = c("high", "Low")),
            actionButton("submit", ("Submit"))
            
            ),
        mainPanel(
            h4("Customers Account Information"),
            dataTableOutput("content"),
            dataTableOutput("content2"),
            tags$hr(),
            wellPanel(
                p(actionButton("pred", "Predict"), downloadButton("dnld", "Download"))
            ),
            h4("Result of Prediction (Potentially Meter Bypass list):"),
            textOutput("text1"),
            dataTableOutput("result")
            
            )
        )
    ))