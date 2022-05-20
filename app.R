require(tidyquant)
require(tidyverse)
require(plotly)
require(shiny)
require(shinydashboard)
require(shinythemes)

options(scipen = 999)

if(!exists('stocks') || max(stocks$date) < today() - 1)
{
    stocks <<- tq_get(c('BTC-USD', 'SHIB-USD', 'ETH-USD', 'SOL-USD', 'DOT1-USD', 'ALGO-USD', 'MATIC-USD', 'ADA-USD', 'AMZN', 'TSLA', 'GOOG', 'MSFT', 'NFLX'), get = 'stock.prices') %>%
        filter(date <= today() - 1) %>%
        group_by(symbol) %>%
        mutate(first_price = min(adjusted[date == min(date)]),
               last_price = max(adjusted[date == max(date)])) %>%
        ungroup() %>%
        mutate(log2_returns = round(log2(last_price) - log2(adjusted), 2),
               log2_price = log2(adjusted))
}
##### UI #####
ui <- fluidPage(
    theme = shinytheme('superhero'),
    headerPanel(
        wellPanel(
            list(HTML('<a href="https://github.com/jwychor"><img src="https://i.ibb.co/n3r8vLx/logo.png" alt="logo" border="0" style="height: 100px; width: 100px;" /></a>','Investment Comparison',
                      HTML('<br /><h4><a href="https://github.com/Jwychor/Investment-Analysis">Source Code</a></h4>'))
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
            choices = unique(stocks$symbol),
            select = c('BTC-USD', 'ETH-USD', 'SOL-USD', 'TSLA', 'GOOG'),
            multiple = TRUE
        ),
        
       titlePanel("Chart"),
       plotlyOutput("log2_returns_chart",
                    height = '75vh')
    )
)

##### Server #####
server <- function(input, output, session){
    t1 <- list(size = 14, color = 'white', family = 'Ariel')
    
    ####Outputs####
    output$log2_returns_chart <- renderPlotly({
        plot_ly(stocks, x =~ date, y =~ get(input$metrics), color =~ symbol) %>%
            filter(symbol %in% input$symbols) %>%
            group_by(symbol) %>%
            layout(plot_bgcolor = '#404040',
                   paper_bgcolor = '#404040',
                   font = list(color = '#FFFFFF'),
                   xaxis = list(title = 'Date',
                                gridcolor = 'black'), 
                   yaxis = list(title = input$metrics,
                                gridcolor = 'black',
                                zerolinecolor = 'black',
                                zerolinewidth = 2)) %>%
            add_lines()
    })
}

shinyApp(ui, server)
