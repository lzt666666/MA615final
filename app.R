#install.packages("shinythemes")
library(shinythemes)
library(tidyverse)
library(lubridate)
library(tidyquant)
library(data.table)
library(shiny)
Sys.setlocale("LC_TIME", "English")
groupdata=read.csv("groupdata.csv")
groupdata=as_tibble(groupdata) 
groupdata$Date=as.Date(groupdata$Date)

returndata=read.csv("returndata.csv")
returndata=as_tibble(returndata) 
returndata$Date=as.Date(returndata$Date)

returndata_weekly=read.csv("returndata_weekly.csv")
returndata_weekly=as_tibble(returndata_weekly) 
returndata_weekly$Date=as.Date(returndata_weekly$Date)

baseline_returns_weekly=read.csv("baseline_returns_weekly.csv")
baseline_returns_weekly=as_tibble(baseline_returns_weekly) 
baseline_returns_weekly$Date=as.Date(baseline_returns_weekly$date)


dividend=read.csv("dividend.csv")


# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("united"),
    mainPanel(
        h6("author: Zhitian Liu"),
        headerPanel("investment analysis"),
        wellPanel(helpText(
            "We track the closing price of 5 stocks that were recommended by",
            a("5 Motley Fool contributors",
              href = "https://www.fool.com/investing/2020/07/20/5-top-stocks-for-july.aspx"),
            " in July, and make comparisons for these stocks, and to see the performance of portfolios consist of these 5 stocks. We assume we had an initial position of $250,000 in cash starting on 1 July 2020. "
        )),
        tabsetPanel(
            tabPanel(
            "single ticker closing price",br(),
            sidebarPanel(
                selectInput("ticker","choose tickers",choices=c("NVEE","AAPL","NEE","BRK","WORK"))
            ),
            mainPanel(
                h3('closing price from 7.1 to 12.1 '),
                plotOutput('CP_plot'),
            )),
            
            tabPanel(
                "monthly return comparisons ",br(),
                sidebarPanel(
                    selectInput("stock","choose tickers",choices=c("NVEE","AAPL","NEE","BRK","WORK"),multiple = TRUE)
                ),
                mainPanel(
                    h3('monthly return comparisons from July to December '),
                    plotOutput('single_stock'),
                )),
            tabPanel(
                "portfolio growth",br(),
                sidebarPanel("All the weights should add up to 1",
                    numericInput("weight1_NVEE","Weight of NVEE",0,0,1,0.05),
                    numericInput("weight1_AAPL","Weight of AAPL",0,0,1,0.05),
                    numericInput("weight1_NEE","Weight of NEE",0,0,1,0.05),
                    numericInput("weight1_BRK","Weight of BRK",0,0,1,0.05),
                    selectInput("weight_WORK","weight of WORK",choices="1-(sum of other weights)")
                    ),
                mainPanel(
                    h3('portfolio comparisons'),
                    plotOutput("portfolio"),
                    h4("We can also check the CAPM table to see whether a portfolio is great to keep invest after December."),
                    DT::dataTableOutput("CAPM")
                    )
                ),
            tabPanel(
                "closing price table",br(),h4("stock price table source from ",a("yahoo finance",
                                                                             href = "https://finance.yahoo.com/")),br(),
                sidebarPanel(
                    dateRangeInput('dateRange',
                                   label = 'Date range input: yyyy-mm-dd',
                                   start = ymd(20200701), end = ymd(20201201)
                    ),
                    selectInput("symbol",
                                "stock name",
                                c("All",
                                  unique(as.character(groupdata$symbol))))),
                mainPanel(
                          
                    DT::dataTableOutput("table"))
                ),
            tabPanel("limitation",br(),
                     h4("there are limitations to this analysis, Firstly, in the real world, we can long or short our position any time we want. Secondly, We didn't take account of some additional revenue such as the dividends and interest."),
                     h4("for interest, if we save all the money into the bank account at the beginning of July, assume the interest rate is 0.08%, we can obtain around
                        500 dollars as a return, which is lower than most of the portfolios we tried."),
                     h4(" for dividend, AAPL and NEE paid dividends twice respectively between July and December, So these 2 tickers may
                        had a better performance than we expected."),
                     mainPanel(
                         h5("dividend payment from AAPL and NEE"),
                         DT::dataTableOutput("dividend"))
                     
                     
                
            )
                
                
            )
    )
)
            
            


# Define server logic required to draw a histogram
server <- function(input, output,session) {
    
    output$CP_plot<-renderPlot({
        CP=groupdata%>%filter(symbol==input$ticker)
        CP %>%
            ggplot(aes(x = Date, y = Close,color=symbol)) +
            geom_line() +
            labs(title = "single stock Line Chart", y = "Closing Price", x = "") + 
            theme_tq()+facet_wrap(~ symbol, ncol = 2, scale = "free_y")
    })
    
    
    
    output$single_stock<-renderPlot({
        return=returndata%>%filter(symbol%in%input$stock)
        return%>%
            ggplot(aes(x = Date, y = monthlyreturn,color=symbol,fill=symbol)) +
            geom_line(size = 2) +
            labs(title = "monthly return line chart", y = "monthlyreturn", x = "") + 
            theme_tq()+scale_color_tq()
        })
    observe({
        updateSelectInput(
            session,
            "weight_WORK",
            label = "weight of WORK",
            choices = 1 - (
                input$weight1_NVEE + input$weight1_AAPL + input$weight1_NEE + input$weight1_BRK
            )
        )
    })
    
    output$portfolio<-renderPlot({
        wts=c(input$weight1_NVEE,input$weight1_AAPL,input$weight1_NEE,input$weight1_BRK,1-(input$weight1_NVEE+input$weight1_AAPL+input$weight1_NEE+input$weight1_BRK))
        
        portfoliogrowth<- returndata_weekly %>%
            tq_portfolio(assets_col   = symbol, 
                         returns_col  = weekly_return, 
                         weights      = wts, 
                         col_rename   = "growth",
                         wealth.index = TRUE) %>%
            mutate(growth = growth * 250000)
        portfoliogrowth %>%
            ggplot(aes(x = Date, y = growth)) +
            geom_line(size = 2) +
            labs(title = "Portfolio Growth",
                 subtitle = "Comparing  Portfolios",
                 x = "", y = "Portfolio Value") +
            geom_smooth(method = "loess") +
            theme_tq() +
            scale_color_tq() +
            scale_y_continuous(labels = scales::dollar)
    })
    output$CAPM<-DT::renderDataTable(DT::datatable({
        wts=c(input$weight1_NVEE,input$weight1_AAPL,input$weight1_NEE,input$weight1_BRK,1-(input$weight1_NVEE+input$weight1_AAPL+input$weight1_NEE+input$weight1_BRK))
        portfolio_<- returndata_weekly %>%
            tq_portfolio(assets_col   = symbol, 
                         returns_col  = weekly_return, 
                         weights      = wts, 
                         col_rename   = "growth",
                         wealth.index = TRUE)
        
        
        compare_data <- left_join(portfolio_, 
                                  baseline_returns_weekly,
                                  by = "Date")
        compare_data %>%
            tq_performance(Ra = growth, Rb = Rb, performance_fun = table.CAPM)

    }))
    
    
    
    output$table<- DT::renderDataTable(DT::datatable({
        data=groupdata %>% filter(Date >= input$dateRange[1] & Date <= input$dateRange[2])
        
        if (input$symbol != "All") {
            data <- data[data$symbol == input$symbol,]
        }
        
        data
        
        
        
    }))
    
    output$dividend<- DT::renderDataTable(DT::datatable({
        dividend
        
        
        
    }))

}

# Run the application 
shinyApp(ui = ui, server = server)
