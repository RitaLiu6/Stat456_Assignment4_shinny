library(shiny)
library(tidyverse)
library(ggplot2)

# load in data
billboard <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-14/billboard.csv')
billboard <- billboard %>% 
  mutate(week_id = as.Date(week_id,"%m/%d/%Y"))
# select the the most popular 10 performers with multiple songs on the list 
topsinger <- billboard %>% 
  group_by(song,performer) %>% 
  count() %>% 
  group_by(performer) %>% 
  count() %>% 
  arrange(by = desc(n)) %>% 
  head(10) %>% 
  pull(performer)

# Define UI for application
ui <- fluidPage(
  # Application title
  titlePanel("Select a song of a performer"),
  
  # Sidebar with inputs 
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "singer", # to use in code
                  label = "Performer:", # how it looks in UI
                  choices = topsinger, 
                  selected = "Taylor Swift"
      ),
      selectInput(inputId = "song", # to use in code
                  label = "Song:", # how it looks in UI
                  multiple = FALSE, # can choose more than one
                  choices = NULL # this will depend on veggie selection! 
      )
      
    ),
    # Show a barplot of chosen varieties within chosen veggie
    mainPanel(
      plotOutput(outputId = "song_ranking")
    )
  )
)

# Define server logic 
server <- function(input, output,session) {
  performersong <- reactive({
    billboard %>% 
      filter(performer == input$singer)
  })
  
  # This is the key piece of code    
  # We use a combination of observeEvent and updateSelectInput
  # There are many updateXXX() functions
  # Think of this as observe the vegetable() (filtered to the chosen veggie)
  # Then, make my list of variety choices and update the "variety" inputId with those choices.
  observeEvent(performersong(), {
    choices <- performersong() %>% 
      distinct(song) %>% 
      arrange(song) %>% 
      pull(song)
    updateSelectInput(session, inputId = "song", choices = choices) 
  })
  
  persong <- reactive({
    performersong() %>% 
      filter(song == input$song)
  })
  
  output$song_ranking <- renderPlot({
    req(input$song) # check that this is here (you've done the selecting)
    persong() %>% 
      group_by(song) %>% 
      ggplot(aes(y = week_position,
                 x = week_id)) +
      geom_point() +
      geom_line() + 
      labs(title = paste("Ranking trend for", input$singer, "'s", input$song),
           x = "week",
           y = "ranking")
  })
}

shinyApp(ui = ui, server = server)

