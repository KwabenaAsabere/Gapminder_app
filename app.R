library(shiny)
library(tidyverse)
library(plotly)


# Data library
library(gapminder)

ui <- fluidPage(
  titlePanel("Gapminder Explorer"),
  
  sidebarLayout(
    
    # contains user input
    sidebarPanel(
      
      h1(textOutput("text")),
      strong(textOutput("text"),
             style = "font-size:100px;"),
      
      selectInput(
        inputId = "year",
        label = "Year:",
        choices = unique(gapminder$year),
        selected = unique(gapminder$year)[1]
      ),
      
      checkboxGroupInput(
        inputId = "continent",
        label = "Continent:",
        choices = unique(gapminder$continent),
        selected = unique(gapminder$continent)
    )
    ),
    
    
    
    # Display main plot outputs
    mainPanel(
      
      h4("Gapminder Description:"),
      
      p("The Gapminder Foundation is a non-profit organization based in Sweden that
        promotes sustainable global development and
        aims to combat misconceptions about world trends. It leverages engaging data 
        visualization tools and presentations to make statistics about social, 
        economic, and environmental issues
        more accessible and understandable. By fostering critical thinking and providing
        evidence-based insights, Gapminder supports informed decision-making and strives to 
        create a more fact-based worldview among the public."
        ),
      
      plotlyOutput("bubblePlot")
    )
  )
)



server <- function(input, output, session) {

   color_map <- rainbow(length(unique(gapminder$continent)))
   names(color_map) <- unique(gapminder$continent)
  
 # create bubble plot comparing life expectancy (dependent)
  # to GDP per capita (independent), by
  #country,with points scaled by population
   
  output$bubblePlot <- renderPlotly({
   
     gapminder_single_year <- filter(gapminder,
                                     year == input$year,
                                     continent %in% input$continent)
    
    ggplot(data = gapminder_single_year,
           aes(
           x = gdpPercap,
           y = lifeExp,
           size = pop,
           fill = continent,
           text = paste("Country:",country)))+
      geom_point(alpha = 0.5,
                 shape = 21,
                 color = "black")+
      scale_fill_manual(values = color_map, aesthetics = "fill")+
      labs(x = "GDP Per Capita (M)",
           y = "Life Expectancy (Years)",
           size = "Population"
           )+
      #set our axes ranges so that they are consistent between selections
      xlim(0, max(gapminder$gdpPercap))+
      ylim(min(gapminder$lifeExp),
           max(gapminder$lifeExp))+
     
       scale_size(range = c(.1,20),
                 name = "Population (M)")+
      theme_bw()+
      theme(
        panel.border = element_blank()
      )
    
    # convert the integer year to string
    # for visualization in the ui
    
  })
  
  output$text <- renderText({
    toString(input$year)
  })
  
  
  
}

shinyApp(ui, server)
colnames(auto)









































