
library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(base64enc)

# Load the data outside the server function so it only loads once when the app starts
# (Assumes NE_normalized.csv is in your working directory)
df <- read.csv("shiny_dat.csv")

# -------------------------------------------------------------------
# PRE-COMPUTE DEFAULTS
# -------------------------------------------------------------------
# Calculate the top 5 ports in 2024 based on port_ind_score
top_5_ports_2024 <- df %>%
  filter(year == 2024) %>%
  arrange(desc(port_ind_score)) %>%
  slice_head(n = 5) %>%
  pull(place_id)

# If the data doesn't have 2024 yet, fallback to the top 5 of the max year available
if (length(top_5_ports_2024) == 0) {
  top_5_ports_2024 <- df %>%
    filter(year == max(year, na.rm = TRUE)) %>%
    arrange(desc(port_ind_score)) %>%
    slice_head(n = 5) %>%
    pull(place_id)
}

# -------------------------------------------------------------------
# USER INTERFACE (UI)
# -------------------------------------------------------------------
ui <- fluidPage(
  
  # CHANGED: Enhanced Title Panel to include the PCFA logo inline with the text
  # Inside your UI fluidPage...
  titlePanel(
    title = div(style = "display: flex; align-items: center; margin-bottom: 15px;",
                
                # R will now grab the image and encode it automatically
                # Replace the placeholder below with your EXACT computer file path
                img(src = dataURI(file = "C:/Users/robert.murphy/Documents/Fishing_Social_Indicators_2026/READ_SSB_Port_Activity_Indicators/data_folder/internal/PCFA_logo_1.png"), 
                    height = "160px", 
                    style = "margin-right: 20px;"),
                
                "New England Port Commercial Fishing Activity Comparison (2007-2024)"
    ),
    windowTitle = "PCFA Dashboard" 
  ),
  
  sidebarLayout(
    sidebarPanel(
      
      # State Filter Checkboxes
      checkboxGroupInput(inputId = "state",
                         label = "Filter by State(s):",
                         choices = sort(unique(df$STATE_ABB)), 
                         selected = unique(df$STATE_ABB), 
                         inline = TRUE),
      
      # The Active Selections box
      wellPanel(
        style = "padding: 10px; background-color: #f8f9fa; border-radius: 5px;",
        strong("Currently Selected Ports (Click to remove):"),
        tags$br(), tags$br(),
        uiOutput(outputId = "selectedPortsBox")
      ),
      
      # Text input for searching ports
      textInput(inputId = "search_port", 
                label = "Search Ports:", 
                value = "", 
                placeholder = "Type a port name..."),
      
      # Wrapped the checkbox group in a scrollable div
      div(style = "max-height: 350px; overflow-y: auto; padding-right: 10px;",
          checkboxGroupInput(inputId = "place",
                             label = "Select Port(s):",
                             choices = sort(unique(df$place_id)), 
                             selected = top_5_ports_2024)
      )
    ),
    
    mainPanel(
      # ADDED: Text box containing sample text placed directly above the plot
      wellPanel(
        style = "background-color: #fcfcfc; border: 1px solid #e3e3e3; border-radius: 6px; padding: 15px; margin-bottom: 20px;",
        h4("Dashboard Information & Overview", style = "margin-top: 0; font-weight: bold; color: #2c3e50;"),
        p("Welcome to the PCFA Dashboard. This tool allows users to explore and compare the commercial fishing activity of select ports across New England. The 'overall activity indicator' is a relative metric and is a reflection of the overall activity of a port in the commercial fishing industry. To generate these scores, we include the following data; the number of dealers buying fish, the number of vessels selling fish, the total pounds and value of fish landed, and the number of dealer and commercial permits registered in that location. Each of these variables is normalized using a min-max scaling approach (between 0 and 1) and an overall mean is calculated for each port and year combination."),
        p(style = "margin-bottom: 0; font-size: 0.95em; color: #555;",
          tags$strong("Note:"), " Use the sidebar controls to filter options by state, search specific port names, or quickly remove active ports by clicking their respective badge buttons."),
        
      
        
        # Added a styled horizontal line to separate sections
        tags$hr(style = "border-top: 1px solid #e0e0e0; margin-top: 25px; margin-bottom: 20px;"),
        
        h4("Sub-dimensions of Activity", style = "margin-top: 5px; font-weight: bold; color: #2c3e50;"),
        p(style = "margin-bottom: 5px;", tags$strong("Port Transaction Activity:"), " This relative metric reflects the magnitude of fish sale transactions in each port from the perspective of dealers and commercial fishermen."),
        p(style = "margin-bottom: 5px;", tags$strong("Port Volume Activity:"), " This relative metric reflects the volume of fish landed including both the overall pounds of fish and the value of fish (in 2024 dollars) landed in each port."),
        p(style = "margin-bottom: 0;", tags$strong("Port Permit Activity:"), " This relative metric reflects the number of dealer permits and commercial fishing permits that are registered in each port."),

      ),
      plotOutput("trendPlot", height = "600px")
    )
  )
)

# -------------------------------------------------------------------
# SERVER LOGIC
# -------------------------------------------------------------------
server <- function(input, output, session) {
  
  # Dynamic observer that listens to BOTH the State filter AND the Search bar
  observeEvent(c(input$state, input$search_port), {
    
    # 1. First, get the baseline list of ports allowed by the State checkboxes
    state_ports <- df %>%
      filter(STATE_ABB %in% input$state) %>%
      pull(place_id) %>%
      unique()
    
    # 2. Then, apply the text search filter (ignoring uppercase/lowercase)
    if (input$search_port == "") {
      search_filtered_ports <- state_ports
    } else {
      search_filtered_ports <- state_ports[grepl(input$search_port, state_ports, ignore.case = TRUE)]
    }
    
    # 3. Figure out which checked ports belong to the currently checked states
    valid_active_selections <- intersect(input$place, state_ports)
    
    # 4. SMART MERGE: Combine the searched ports with the active selections.
    final_choices <- sort(unique(c(search_filtered_ports, valid_active_selections)))
    
    # 5. Update the checklist UI
    updateCheckboxGroupInput(
      session = session,
      inputId = "place",
      choices = final_choices,
      selected = valid_active_selections
    )
  }, ignoreInit = TRUE) 
  
  
  # Render the interactive tags for the "Active Selections" box
  output$selectedPortsBox <- renderUI({
    if (length(input$place) == 0) {
      return("None")
    }
    
    tag_list <- lapply(input$place, function(port) {
      tags$button(
        type = "button",
        class = "btn btn-default btn-sm",
        style = "margin: 2px; border: 1px solid #ccc; border-radius: 15px; background-color: white;",
        onclick = sprintf("Shiny.setInputValue('remove_port', '%s', {priority: 'event'});", port),
        HTML(paste(port, "&times;")) 
      )
    })
    
    do.call(tagList, tag_list)
  })
  
  # Observe clicks on the generated "remove" tags
  observeEvent(input$remove_port, {
    current_selections <- input$place
    new_selections <- setdiff(current_selections, input$remove_port)
    
    updateCheckboxGroupInput(
      session = session,
      inputId = "place",
      selected = new_selections
    )
  })
  
  # Render the Plot
  output$trendPlot <- renderPlot({
    req(input$place)
    
    plot_data <- df %>%
      filter(place_id %in% input$place) %>% 
      select(place_id, year, port_actors_score, port_volume_score, port_work_score, port_ind_score) %>% 
      pivot_longer(cols = -c(year, place_id), 
                   names_to = "Indicator", 
                   values_to = "Value") %>%
      mutate(Indicator = case_when(
        Indicator == "port_actors_score" ~ "Port Transaction Activity",
        Indicator == "port_volume_score" ~ "Port Volume Activity",
        Indicator == "port_work_score" ~ "Port Permit Activity",
        Indicator == "port_ind_score" ~ "Port Overall Activity",
        TRUE ~ Indicator
      ))
    
    ggplot(plot_data, aes(x = year, y = Value, color = as.factor(place_id))) +
      geom_line(linewidth = 1) +
      geom_point(size = 2.5) +
      facet_wrap(~ Indicator, scales = "free_y", axes="all", ncol = 2) + 
      scale_x_continuous(breaks = seq(2007, 2024, by = 2)) + 
      theme_minimal(base_size = 16) +
      labs(title = "Activity Trends Comparison",
           x = "Year",
           y = "Score",
           color = "Port") +
      theme(legend.position = "right",
            plot.title = element_text(face = "bold"),
            strip.text = element_text(face = "bold", size = 14)) 
  })
}

# -------------------------------------------------------------------
# RUN THE APP
# -------------------------------------------------------------------
shinyApp(ui = ui, server = server)







