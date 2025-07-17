require(tidyquant)
require(tidyverse)
require(plotly)
require(shiny)
require(shinydashboard)
require(shinythemes)
require(quantmod)

options(scipen = 999)

base_stock_list <- c('BTC-USD', 'SHIB-USD', 'ETH-USD', 'SOL-USD', 'ALGO-USD', 'MATIC-USD', 'AMZN', 'TSLA', 'GOOG', 'MSFT', 'NFLX',
                     'RIOT', 'MARA', 'CIFR', 'HUT', 'WULF', 'BITF', 'CLSK', 'GLD', 'NVDA')


dataRefresh <- function(newTickers = NULL, getBaseData = FALSE){
  stock_list <- c(newTickers)
  
  if(getBaseData){
    stock_list <- c(stock_list, base_stock_list)
  }
  
  stock_list <- stock_list[order(stock_list)]
  
  tryCatch({
    newStocks <- tq_get(stock_list, get = 'stock.prices') %>%
      filter(date <= today() - 1) %>%
      group_by(symbol) %>%
      mutate(symbol_max_date = max(date),
             symbol_min_date = min(date),
             first_price = min(adjusted[date == symbol_min_date]),
             last_price = max(adjusted[date == symbol_max_date])) %>%
      ungroup() %>%
      mutate(log2_returns = round(log2(last_price) - log2(adjusted), 2),
             log2_price = log2(adjusted))
    
    if(getBaseData){
      stocks <<- newStocks
      }
    else {
      stocks <<- stocks %>% union_all(newStocks)
      }
    },
    error = function(e){
      message('Error fetching data: ')
      print(e)
  })
}
if(!exists('stocks') || max(stocks$date) < today() - 1)
{
  dataRefresh(getBaseData = TRUE)
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
                      label = 'Select Tickers (Type to Add New Tickers)',
                      choices = unique(stocks$symbol),
                      select = c('AMZN', 'BTC-USD', 'GOOG', 'MSFT', 'NFLX', 'TSLA', 'GLD', 'NVDA'),
                      multiple = TRUE,
                      options = list(create = TRUE)
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
                          label = 'Highlighted Symbols',
                          choices = c('AMZN', 'BTC-USD', 'GOOG', 'MSFT', 'NFLX', 'TSLA', 'GLD', 'NVDA'),
                          multiple = TRUE
                        )
                      ),
                    ),

                    titlePanel(""),
                    plotlyOutput("log2_returns_chart",
                                 height = '70vh')
                  ) 
                ),
                tabPanel('Debug',
                  wellPanel(
                    htmlOutput("sessionInfo")
                  )
                )
    ),
    uiOutput("highlightSymbol")
)

##### Server #####
server <- function(input, output, session){
    t1 <- list(size = 14, color = 'white', family = 'Ariel')
    
    # update highlight dropdown
    observeEvent(input$symbols,{
      # if any new tickers, get new data
      if(!all(input$symbols %in% unique(stocks$symbol))){
        newTickers <- input$symbols[!(input$symbols %in% stocks$symbol)]
        
        dataRefresh(newTickers)
        
        print(paste0("new ticker(s): ", paste(newTickers), " added"))
      }
      
      symb <- as.character(input$symbols)
      symb <- symb[order(symb)]
      
      updateSelectInput(session,
                        "highlightSymbol", 
                        label = 'Highlighted Symbols',
                        choices = c(symb),
                        selected = input$highlightSymbol)
    })
    
    ####Outputs####
    output$log2_returns_chart <- renderPlotly({
      highlightSymbol <- ""
      if(!is.null(input$highlightSymbol)){
        highlightSymbol <- input$highlightSymbol
      }
      
      stocks %>%
        mutate(thickness = case_when(symbol %in% highlightSymbol ~ 3, T ~ 1)) %>%
        filter(symbol %in% input$symbols) %>%
        arrange(symbol) %>%
        plot_ly(x =~ date, 
                y =~ get(input$metrics),
                color =~ symbol,
                mode = 'lines',
                split =~ thickness,
                line = list(width =~ thickness)
        ) %>%
        layout(plot_bgcolor = '#404040',
               paper_bgcolor = '#404040',
               font = list(color = '#FFFFFF'),
               xaxis = list(title = 'Date',
                            gridcolor = 'black'),
               yaxis = list(title = input$metrics,
                            gridcolor = 'black',
                            zerolinecolor = 'black',
                            zerolinewidth = 4,
                            tickvals = seq(1, 10, 1))) %>%
        add_lines(name =~ symbol)
    })
    
    # debug
    output$sessionInfo <- renderPrint({
      capture.output(sessionInfo())
    })
}

shinyApp(ui, server)

