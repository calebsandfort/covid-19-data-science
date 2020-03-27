#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#   R -e "shiny::runApp('./Covid-19-Dashboard')"

library(shiny)
library(plotly)
library(ggplot2)
library(ggthemes)
library(stringr)
library(wesanderson)
library(maps)
source("code/daily-report-wrangling.R")

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Covid19 Dashboard"),

    fluidRow(
        column(12, 
            tabsetPanel(type = "tabs",
                        tabPanel("States",
                                 fluidRow(style = "padding-top: 10px;",
                                          column(6,
                                                 plotlyOutput("state_total_cases_map_plot")),
                                          column(6,
                                             fluidRow(style = "padding-top: 10px;",
                                                 column(12,
                                                    selectInput("uxSelect_State", "State:", getStates(), "Washington")
                                                    )
                                             ),
                                             
                                             fluidRow(
                                                      column(6,
                                                            plotlyOutput("state_total_cases_by_day_plot") 
                                                      ),
                                                      column(6,
                                                             plotlyOutput("state_new_cases_by_day_plot") 
                                                      )
                                             )
                                        )
                                    )
                             ),
                        tabPanel("US vs Italy",
                                 h3("Stuff Here Test"))
            )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    summaryReport <- getSummaryReport()
    states_map <- map_data("state")
    
    output$state_total_cases_map_plot <- renderPlotly({
        p <- summaryReport %>% 
            mutate(State = str_to_lower(State)) %>%
            ggplot(aes(map_id = State)) +
            geom_map(aes(fill = Total), map = states_map) +
            expand_limits(x = states_map$long, y = states_map$lat) +
            ggtitle("Confirmed Cases") +
            theme_gdocs() +
            scale_fill_gradientn(colors = heat.colors(10, rev = TRUE))
        
        p <- ggplotly(p)
        p
    })
    
    stateRows <- reactive(
        getStateRows(input$uxSelect_State)
    )
    
    output$state_total_cases_by_day_plot <- renderPlotly({
        p <- stateRows() %>% 
            ggplot(aes(Date, Total, fill = Total)) +
            geom_bar(stat = "identity") +
            ggtitle("Confirmed Cases by Day") +
            theme_gdocs() +
            scale_fill_gradientn(colors = heat.colors(10, rev = TRUE))
        
        p <- ggplotly(p)
        p
    })
    
    newCasesMinMax <- getNewCasesMinMax()
    
    output$state_new_cases_by_day_plot <- renderPlotly({
        p <- stateRows() %>% 
            ggplot(aes(Date, New, fill = New)) +
            geom_bar(stat = "identity") +
            ggtitle("New Cases by Day") +
            theme_gdocs() +
            scale_fill_gradientn(colors = wes_palette("Zissou1"))
        
        p <- ggplotly(p)
        p
    })
    
    # get_plot_states_by_day <- function(){
    #     hotzone_states_df %>%
    #         ggplot(aes(Date, Total, color = State, fill = State)) +
    #         geom_bar(stat = "identity", position = "dodge") +
    #         ggtitle("Coronavirus Cases - Hotzone States") +
    #         ylab("Confirmed Cases")
    # }
}

# Run the application 
shinyApp(ui = ui, server = server)
