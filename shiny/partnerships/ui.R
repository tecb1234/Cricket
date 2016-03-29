library(shiny)

# load("matches.RData")
# load("ball_by_ball.RData")
source("partnership_networks.R")

# Define the overall UI
shinyUI(
  
  # Use a fluid Bootstrap layout
  fluidPage(    
    
    # Give the page a title
    titlePanel("Partnership Networks"),
    
    # Generate a row with a sidebar
    sidebarLayout(      
      
      # Define the sidebar with one input
      sidebarPanel(
        selectInput("country", "Choose Country", 
                    choices = countries$batting_side),
        
        selectInput("match_ID", "choose match",
                    choices = c(1,2)),
        
        hr(),
        helpText("Data from cricsheet.org")
      ),
      
      # The output diagram
      mainPanel(
        forceNetworkOutput("networkDiagram")
      )
      
    )
  )
)