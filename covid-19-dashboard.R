library(shinydashboard)
library(shiny)

dim(covid19)



country_list = unique(covid19$country)
type_list    = unique(covid19$type)


header = dashboardHeader(title='COVID-19 Insights')

sidebar = dashboardSidebar(
    sidebarMenu( 
    menuItem(text='Dashboard', 
           icon=icon('tachometer-alt'),
           tabName = 'dashboard'),
    menuItem(text='Trends',
           icon =icon('chart-line'),
           tabName = 'trends')
    )
)

body = dashboardBody(
  tabItems(
    tabItem(tabName = 'dashboard',
            titlePanel("Top 10 Countries"),
            fluidRow(
              column(width=3,
                     selectInput(inputId = 'top_n',
                                 label   = 'Select Top Countires:',
                                 choices = c(2,5,10),
                                 selected = 2)
              ),
              
              column(width=3,
                     selectInput(inputId = 'type_top_n',
                                 label   = 'Case Type:',
                                 choices = type_list,
                                 selected = head(type_list,1) )                                          
                     
              )
              
            ),
            fluidRow(
              column(width = 8, offset = 2,
                     plotOutput(outputId = 'plot_topn')
              )
              
            )
    ),
    tabItem(tabName = 'trends',
            titlePanel("Compare Countries"),
            fluidRow(
              column(width = 3,
                     selectInput(inputId = 'country1',
                                 label   = 'Select Country-1 :',
                                 choices = country_list,
                                 selected = head(country_list,1)
                     )
              ),
              column(width = 3,
                     selectInput(inputId = 'country2',
                                 label   = 'Select Country-2 :',
                                 choices = country_list,
                                 selected = tail(country_list,1)
                     )
                     
              ),
              column(width = 3,
                     selectInput(inputId = 'type1',
                                 label   = 'Select Case Type :',
                                 choices = type_list,
                                 selected = head(type_list,1)
                     )
              )
              
              
            ),
            fluidRow(
              column(width = 8, offset = 2,
                     plotOutput(outputId = 'plot_compare')
              )
              
              
            )
    )
    
  )
  
)

ui = dashboardPage(header, sidebar, body)


server <- function(input, output) {
  
  output$plot_topn = renderPlot({
    
    topn = covid19 %>% 
      filter(type == input$type_top_n) %>% 
      group_by(country) %>% 
      summarise(max.cases = max(total.cases)) %>% 
      arrange(desc(max.cases)) %>% head(as.numeric(input$top_n)) 
    
    covid19 %>% 
      filter(country %in% topn$country, type == input$type_top_n) %>%
      ggplot(aes(date, total.cases,group=country, color=country))+
      geom_line()
    
    
  })
  
  
  output$plot_compare = renderPlot({
    
    covid19 %>% 
      filter(country %in% c(input$country1, input$country2), type == input$type1) %>%
      ggplot(aes(date, total.cases,group=country, color=country))+
      geom_line()
    
    
  })
  
}

shinyApp(ui, server)






