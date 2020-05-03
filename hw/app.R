library(shiny)
library(shinythemes)
library(tidyverse)
library(plotly)
library(RColorBrewer)


mortality <- read.csv("https://raw.githubusercontent.com/charleyferrari/CUNY_DATA_608/master/module3/data/cleaned-cdc-mortality-1999-2010-2.csv", header = T, stringsAsFactors = F)

mortality_2010 <- mortality %>% dplyr::filter(Year == 2010)

nat_avg <- mortality %>% 
  dplyr::group_by(Year, ICD.Chapter) %>% 
  dplyr::mutate(Nat_avg = round((sum(Deaths)/sum(Population)*100000),1)) %>% 
  dplyr::select(Year, ICD.Chapter, State, Crude.Rate, Nat_avg)

ui <- fluidPage(
  theme = shinytheme("yeti"),
  titlePanel("DATA 608: Module 3 - Deaths in the US"),
             h3("Wei Zhou"),
             h4("03/08/2020"),
  
  sidebarLayout(
    sidebarPanel(
      
      h2("Question 1:"),
      p("As a researcher, you frequently compare mortality rates from particular causes across
different States. You need a visualization that will let you see (for 2010 only) the crude
mortality rate, across all States, from one cause (for example, Neoplasms, which are
effectively cancers). Create a visualization that allows you to rank States by crude mortality
for each cause of death."),
           
      selectInput("var", 
                  label = "Choose a Cause of Death",
                  choices = mortality_2010$ICD.Chapter),
           br(),
           br(),
           br(),
           br(),
           br(),
      
      h2("Question 2:"),
      p("Often you are asked whether particular States are improving their mortality rates (per cause)
faster than, or slower than, the national average. Create a visualization that lets your clients
see this for themselves for one cause of death at the time. Keep in mind that the national
average should be weighted by the national population."),
      
           selectInput("var2", 
                label = "Choose a State",
                choices = mortality_2010$State),
          checkboxInput("check", "View National Average", value = FALSE)
      ),
    mainPanel(plotlyOutput("selected_var"),
              plotlyOutput("selected_var2"),
              
              br(),
              br(),
              br(),
              p("Source code for this app can be found on",
                a(strong("Github."), 
                  href = "https://github.com/javernw/DATA608-Knowledge-and-Visual-Analytics/blob/master/Module%203/app.R")))
  )
)


server <- function(input, output) {
output$selected_var <- renderPlotly({
    mortality_2010 %>% filter(ICD.Chapter == input$var) %>% 
    ggplot(aes(x = State, y = Crude.Rate, fill = ICD.Chapter)) +
    geom_bar(stat = "identity") +
    scale_fill_brewer(palette="Dark2")+
    theme(axis.text.x = element_text(angle = 90, hjust = 1), 
          axis.text.y = element_text(face = "bold", color = "black", size = 12)) +
    theme(axis.text=element_text(size=6)) + 
    theme(legend.position='none')
  })
 
  output$selected_var2 <- renderPlotly({ 
    na <- nat_avg %>% filter(State == input$var2, ICD.Chapter == input$var) 
    if(input$check){
      na %>% ggplot() + 
      geom_bar(stat = "identity", aes(x=Year, y=Crude.Rate, fill = Year) )+
      geom_line(aes(x=Year, y=Nat_avg), color = "brown", size = 2)
    }else{
      na %>% ggplot() + 
        geom_bar(stat = "identity", aes(x=Year, y=Crude.Rate, fill = Year))
    }
      
    })
}


shinyApp(ui = ui, server = server)
