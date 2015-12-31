library(shiny)

source("process_cricsheet.R")
source("partnership_networks.R")

# Define a server for the Shiny app
shinyServer(function(input, output, session) {
  
  matches <- reactive({
    return <- matches_df %>%
      filter(teams == input$country) %>%
      select(match_id) %>%
      distinct(match_id)
  })
  
  observe({
    updateSelectInput(session, inputId = "match_ID", choices = matches())
  })
  
  nodes_df_to_plot <- reactive({
    return <- batsman_at_crease %>%
      select(batting_side, batter, match_id) %>%
      filter(batting_side == input$country) %>%
      filter(match_id == input$match_ID) %>%
      distinct(batter) %>%
      mutate(node_key = 0:(n()-1)) %>%
      mutate(Group = rep(1,n()))
  })
  
  links_df_to_plot <- reactive({
    return <- partnerships_by_match %>%
      filter(batting_side == input$country) %>%
      filter(match_id == input$match_ID) %>%
      left_join(nodes_df_to_plot(), by = c("batsman" = "batter", "batting_side", "match_id")) %>%
      rename(batter_node_key = node_key) %>%
      left_join(nodes_df_to_plot(), by = c("non_striker" = "batter", "batting_side", "match_id")) %>%
      rename(non_striker_node_key = node_key)
  })
  
  # Fill in the spot we created for a plot
  output$networkDiagram <- renderForceNetwork({
    forceNetwork(Links = links_df_to_plot(),
                 Nodes = nodes_df_to_plot(),
                 NodeID = "batter",
                 Source = "batter_node_key", 
                 Target = "non_striker_node_key", 
                 Group = "Group", 
                 Value = "runs_total")
  
  })
})