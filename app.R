require(tidyquant)
require(tidyverse)
require(plotly)
require(shiny)
require(shinydashboard)
require(shinythemes)
require(quantmod)

options(scipen = 999)

if(!exists('stocks') || max(stocks$date) < today() - 1)
{
    stocks <<- tq_get(c('BTC-USD', 'SHIB-USD', 'ETH-USD', 'SOL-USD', 'ALGO-USD', 'MATIC-USD', 'AMZN', 'TSLA', 'GOOG', 'MSFT', 'NFLX'), get = 'stock.prices') %>%
        filter(date <= today() - 1) %>%
        group_by(symbol) %>%
        mutate(symbol_max_date = max(date),
               symbol_min_date = min(date),
               first_price = min(adjusted[date == symbol_min_date]),
               last_price = max(adjusted[date == symbol_max_date])) %>%
        ungroup() %>%
        mutate(log2_returns = round(log2(last_price) - log2(adjusted), 2),
               log2_price = log2(adjusted))
}

##### UI #####
ui <- fluidPage(
    theme = shinytheme('superhero'),
    headerPanel(
        wellPanel(
            list(
              HTML('<a href="https://github.com/jwychor"><img src="https://i.ibb.co/n3r8vLx/logo.png" alt="logo" border="0" style="height: 100px; width: 100px;" /></a>','Investment Comparison'),
              HTML('<br /><h4><a href="https://github.com/Jwychor/Crypto-Analysis">Source Code</a></h4>')
             ),
            tags$hr()
        )
    ),
    tabsetPanel(type = 'tabs',
                tabPanel('Plots',
                  wellPanel(
                    # Stock Symbols
                    selectizeInput(
                      inputId = 'symbols',
                      label = 'Select Tickers',
                      choices = unique(stocks$symbol),
                      select = c('BTC-USD', 'ETH-USD', 'TSLA', 'GOOG', 'MSFT', 'AMZN', 'NFLX'),
                      multiple = TRUE
                    ),
                    # Metrics
                    fixedRow(
                      column(3,
                       selectizeInput(
                         inputId = 'metrics',
                         label = 'Select a Metric',
                         choices = c('log2_returns', 'log2_price'),
                         select = 'log2_returns',
                         multiple = FALSE
                       )
                      ),
                      column(3,
                        selectizeInput(
                          inputId = 'highlightSymbol',
                          label = 'Highlight a Symbol',
                          choices = c("None", c('BTC-USD', 'ETH-USD', 'TSLA', 'GOOG', 'MSFT', 'AMZN', 'NFLX')),
                          select = "None",
                          multiple = FALSE
                        )
                      ),
                    ),

                    titlePanel("Chart"),
                    plotlyOutput("log2_returns_chart",
                                 height = '75vh')
                  ) 
                ),
                tabPanel('Debug',
                  wellPanel(
                    htmlOutput("sessionInfo"),
                    htmlOutput("debug")
                  )
                )
    )
)

##### Server #####
server <- function(input, output, session){
    t1 <- list(size = 14, color = 'white', family = 'Ariel')
    
    ####Outputs####
    output$log2_returns_chart <- renderPlotly({
      stocks %>%
      mutate(thickness = case_when(symbol == input$highlightSymbol ~ 30, T ~ 2)) %>%
      plot_ly(x =~ date, y =~ get(input$metrics), color =~ symbol, line = list(width =~ thickness)) %>%
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
                              zerolinewidth = 4),
                 tickvals = seq(1, 10, 1)) %>%
          add_lines()
    })
    # update highlight dropdown
    output$selectedSymbols <- renderUI({
      selectInput("highlightSymbol", 
                  label = 'Highlight a Symbol',
                  choices = as.character(input$symbols),
                  selected = input$highlightSymbol)
    })
    # debug
    output$sessionInfo <- renderPrint({
      capture.output(sessionInfo())
    })
    
    output$debug <- renderprint({
      c(input$highlightSymbol, input$symbols)
    })
}

shinyApp(ui, server)
