
library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)


# Load the data outside the server function so it only loads once when the app starts
# (Assumes NE_normalized.csv is in your working directory)
df <- read.csv("shiny_dat.csv")

ui <- fluidPage(
  
  titlePanel("New England Fishing Port Activity Comparison (2007-2024)"),
  
  sidebarLayout(
    sidebarPanel(
      # Dropdown for selecting the Port(s)
      selectInput(inputId = "place",
                  label = "Select Port(s) (place_id):",
                  choices = unique(df$place_id), 
                  selected = unique(df$place_id)[1],
                  multiple = TRUE)
      
      # The indicator dropdown has been removed!
    ),
    
    mainPanel(
      # Increased the height of the plot output to comfortably fit a 2x2 grid
      plotOutput("trendPlot", height = "600px")
    )
  )
)

# -------------------------------------------------------------------
# SERVER LOGIC
# -------------------------------------------------------------------
server <- function(input, output) {
  
  output$trendPlot <- renderPlot({
    req(input$place)
    
    # Reshape data to handle multiple ports and hardcode the 4 indicators
    plot_data <- df %>%
      filter(place_id %in% input$place) %>% 
      # Hardcode the specific 4 indicator columns here
      select(place_id, year, port_actors_score, port_volume_score, port_work_score, port_ind_score) %>% 
      pivot_longer(cols = -c(year, place_id), 
                   names_to = "Indicator", 
                   values_to = "Value") %>%
      # Clean up the raw column names so the facet labels look professional
      mutate(Indicator = case_when(
        Indicator == "port_actors_score" ~ "Actors Score",
        Indicator == "port_volume_score" ~ "Volume Score",
        Indicator == "port_work_score" ~ "Work Score",
        Indicator == "port_ind_score" ~ "Ind Score",
        TRUE ~ Indicator
      ))
    
    # Generate the ggplot
    ggplot(plot_data, aes(x = year, y = Value, color = as.factor(place_id))) +
      geom_line(linewidth = 1) +
      geom_point(size = 2.5) +
      # Create a 2x2 grid of panels, allowing each to have its own y-axis scale
      facet_wrap(~ Indicator, scales = "free_y", ncol = 2) + 
      scale_x_continuous(breaks = seq(2007, 2024, by = 2)) + 
      theme_minimal(base_size = 14) +
      labs(title = "Activity Trends Comparison",
           x = "Year",
           y = "Score",
           color = "Port (place_id)") +
      theme(legend.position = "bottom",
            plot.title = element_text(face = "bold"),
            strip.text = element_text(face = "bold", size = 12)) 
  })
}

# -------------------------------------------------------------------
# RUN THE APP
# -------------------------------------------------------------------
shinyApp(ui = ui, server = server)
