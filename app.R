require(tidyquant)
require(tidyverse)
require(ggrepel)
require(plotly)
require(shiny)
require(shinydashboard)
require(shinythemes)

options(scipen = 999)

df <- tq_get(c('BTC-USD', 'SHIB-USD', 'ETH-USD', 'SOL1-USD', 'DOT1-USD', 'ALGO-USD', 'MATIC-USD', 'ADA-USD', 'AMZN', 'TSLA', 'GOOG', 'MSFT'), get = 'stock.prices') %>%
    filter(date <= today() - 1) %>%
    group_by(symbol) %>%
    mutate(first_price = min(adjusted[date == min(date)]),
           last_price = max(adjusted[date == max(date)])) %>%
    ungroup() %>%
    mutate(log2_returns = round(log2(last_price) - log2(adjusted), 2),
           log2_price = log2(adjusted))

##### UI #####
ui <- fluidPage(
    theme = shinytheme('slate'),
    headerPanel(
        wellPanel(
            list(HTML('<a href="https://github.com/jwychor"><img src="https://i.ibb.co/n3r8vLx/logo.png" alt="logo" border="0" style="height: 100px; width: 100px;" /></a>','Investment Comparison',
                      HTML('<br /><h4><a href="https://github.com/Jwychor/Graphing-Top-Bilboard-Data">Source Code</a></h4>'))
            ),
            tags$hr()
        )
    ),
    
    wellPanel(
        selectizeInput(
            inputId = 'metrics',
            label = 'Select a Metric',
            choices = c('log2_returns', 'log2_price'),
            select = 'log2_returns',
            multiple = FALSE
        ),
        
        selectizeInput(
            inputId = 'symbols',
            label = 'Select Tickers',
            choices = unique(df$symbol),
            select = c('BTC-USD', 'ETH-USD', 'SOL1-USD', 'TSLA', 'GOOG'),
            multiple = TRUE
        ),
        
        
        fluidRow(
            tags$style('min-height: 90vh;'),
            column(12,
                   titlePanel("Chart"),
                   plotlyOutput("log2_returns_chart",
                                height = '70vh')
            )
        )
    )
)

##### Server #####
server <- function(input, output, session){
    t1 <- list(size = 14, color = 'white', family = 'Ariel')
    
    ####Outputs####
    output$log2_returns_chart <- renderPlotly({
        plot_ly(df, x =~ date, y =~ get(input$metrics), color =~ symbol) %>%
            filter(symbol %in% input$symbols) %>%
            group_by(symbol) %>%
            add_lines() %>%
            layout(plot_bgcolor = '#404040',
                   paper_bgcolor = '#404040',
                   font = list(color = '#FFFFFF'),
                   xaxis = list(title = 'Date'),
                   yaxis = list(title = 'Log2 Returns/Price'))
    })
}

shinyApp(ui, server)

