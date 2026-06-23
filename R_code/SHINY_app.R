
library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(base64enc)

# Load the data outside the server function so it only loads once when the app starts
#### set working directory ####
setwd("C:/Users/robert.murphy/Documents/Fishing_Social_Indicators_2026/READ_SSB_Port_Activity_Indicators/data_folder/internal")

# (Assumes NE_normalized.csv is in your working directory)
df_raw <- read.csv("shiny_dat.csv")

#filter for 0
df <- df_raw %>%
  filter(
    port_actors_score != 0,
    port_volume_score != 0,
    port_work_score   != 0,
    port_ind_score    != 0
  )

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
# USER INTERFACE (UI) - FIXED
# -------------------------------------------------------------------
ui <- fluidPage(
  titlePanel(
    title = div(style = "display: flex; align-items: center; margin-bottom: 15px;",
                img(src = dataURI(file = "C:/Users/robert.murphy/Documents/Fishing_Social_Indicators_2026/READ_SSB_Port_Activity_Indicators/data_folder/internal/PCFA_logo_1.png"), height = "160px", style = "margin-right: 20px;"),
                "New England Port Commercial Fishing Activity Comparison (2007-2024)"
    ),
    windowTitle = "PCFA Dashboard"
  ),
  
  wellPanel(
    style = "background-color: #fcfcfc; border: 1px solid #e3e3e3; border-radius: 6px; padding: 15px; margin-bottom: 20px;",
    h4("Dashboard Information & Overview", style = "margin-top: 0; font-weight: bold; color: #2c3e50;"),
    p("Welcome to the PCFA Dashboard. This tool allows users to explore and compare the commercial fishing activity of select ports across New England."),
    p(style = "margin-bottom: 0; font-size: 0.95em; color: #555;",
      tags$strong("Note:"), "Use the sidebar controls to filter options by state, search specific port names, or quickly remove active ports by clicking their respective badge buttons."
    ),
    # FIXED: Wrapped style property in double quotes
    tags$hr(style = "border-top: 1px solid #e0e0e0; margin-top: 25px; margin-bottom: 20px;"),
    h4("Sub-dimensions of Activity", style = "margin-top: 5px; font-weight: bold; color: #2c3e50;"),
    p(style = "margin-bottom: 5px;", tags$strong("Port Transaction Activity:"), "Reflects the magnitude of fish sale transactions."),
    p(style = "margin-bottom: 5px;", tags$strong("Port Volume Activity:"), "Reflects the volume and value of fish landed."),
    p(style = "margin-bottom: 0;", tags$strong("Port Permit Activity:"), "Reflects the number of registered dealer and commercial permits.")
  ),
  
  sidebarLayout(
    sidebarPanel(
      # FIXED: Moved closing parenthesis to the end of inline = TRUE
      checkboxGroupInput(inputId = "state", label = "Filter by State(s):", 
                         choices = sort(unique(df$STATE_ABB)), selected = unique(df$STATE_ABB), inline = TRUE),
      
      # The Active Selections box
      wellPanel(
        style = "padding: 10px; background-color: #f8f9fa; border-radius: 5px;",
        strong("Currently Selected Comparison Ports (Click to remove):"), tags$br(), tags$br(),
        uiOutput(outputId = "selectedPortsBox")
      ),
      
      # Text input for searching ports
      textInput(inputId = "search_port", label = "Search Comparison Ports:", value = "", placeholder = "Type a port name..."),
      
      # Scrollable container for comparison checklist
      div(style = "max-height: 250px; overflow-y: auto; padding-right: 10px; margin-bottom: 20px;",
          checkboxGroupInput(inputId = "place", label = "Select Ports to Compare:", 
                             choices = sort(unique(df$place_id)), selected = top_5_ports_2024)
      ),
      
      # Separator line before single selection
      tags$hr(style = "border-top: 1px solid #dcdcdc;"),
      selectInput(inputId = "single_place", 
                  label = tags$span(style = "color: #2c3e50; font-weight: bold;", "Select ONE Port for Detailed Plot Below:"),
                  choices = sort(unique(df$place_id)), 
                  selected = top_5_ports_2024[1]) # Pick first port as fallback default
    ),
    
    mainPanel(
      plotOutput("trendPlot", height = "720px")
    )
  ),
  
  # Full-width row containing a centered box for the single port profile and rankings
  fluidRow(
    column(width = 7, offset = 4,
           wellPanel(
             style = "background-color: #ffffff; border: 1px solid #dcdcdc; border-radius: 6px; padding: 20px; margin-top: 20px; margin-bottom: 30px;",
             h3(textOutput("singlePortTitle"), style = "margin-top: 0; margin-bottom: 15px; font-weight: bold; color: #2c3e50; text-align: center;"),
             plotOutput("singlePortPlot", height = "500px") # Increased height slightly for readability
           )
    )
  )
)
# -------------------------------------------------------------------
# SERVER LOGIC
# -------------------------------------------------------------------
server <- function(input, output, session) {
  
  # Dynamic observer for the state/search filter system (keeps unselected states in plots)
  observeEvent(c(input$state, input$search_port), {
    state_ports <- df %>% filter(STATE_ABB %in% input$state) %>% pull(place_id) %>% unique()
    
    if (input$search_port == "") {
      search_filtered_ports <- state_ports
    } else {
      search_filtered_ports <- state_ports[grepl(input$search_port, state_ports, ignore.case = TRUE)]
    }
    
    valid_active_selections <- input$place
    final_choices <- sort(unique(c(search_filtered_ports, valid_active_selections)))
    
    updateCheckboxGroupInput(
      session = session,
      inputId = "place",
      choices = final_choices,
      selected = valid_active_selections
    )
  }, ignoreInit = TRUE)
  
  # Render active comparison badge buttons
  output$selectedPortsBox <- renderUI({
    if (length(input$place) == 0) { return("None") }
    tag_list <- lapply(input$place, function(port) {
      tags$button(
        type = "button", class = "btn btn-default btn-sm",
        style = "margin: 2px; border: 1px solid #ccc; border-radius: 15px; background-color: white;",
        onclick = sprintf("Shiny.setInputValue('remove_port', '%s', {priority: 'event'});", port),
        HTML(paste(port, "&times;"))
      )
    })
    do.call(tagList, tag_list)
  })
  
  # Observe clicks on the generated remove tags
  observeEvent(input$remove_port, {
    current_selections <- input$place
    new_selections <- setdiff(current_selections, input$remove_port)
    
    state_ports <- df %>% filter(STATE_ABB %in% input$state) %>% pull(place_id) %>% unique()
    if (input$search_port == "") {
      search_filtered_ports <- state_ports
    } else {
      search_filtered_ports <- state_ports[grepl(input$search_port, state_ports, ignore.case = TRUE)]
    }
    final_choices <- sort(unique(c(search_filtered_ports, new_selections)))
    
    updateCheckboxGroupInput(session = session, inputId = "place", choices = final_choices, selected = new_selections)
  })
  
  # Render the Faceted Comparison Plot
  output$trendPlot <- renderPlot({
    req(input$place)
    plot_data <- df %>%
      filter(place_id %in% input$place) %>%
      select(place_id, year, port_actors_score, port_volume_score, port_work_score, port_ind_score) %>%
      pivot_longer(cols = -c(year, place_id), names_to = "Indicator", values_to = "Value") %>%
      mutate(Indicator = case_when(
        Indicator == "port_actors_score" ~ "Port Transaction Activity",
        Indicator == "port_volume_score" ~ "Port Volume Activity",
        Indicator == "port_work_score"  ~ "Port Permit Activity",
        Indicator == "port_ind_score"   ~ "Port Overall Activity",
        TRUE ~ Indicator
      ))
    
    ggplot(plot_data, aes(x = year, y = Value, color = as.factor(place_id))) +
      geom_line(linewidth = 1, alpha=0.6) + geom_point(size = 3, alpha=0.6) +
      facet_wrap(~ Indicator, scales = "free_y", axes = "all", ncol = 2) +
      scale_x_continuous(breaks = seq(2007, 2024, by = 2)) +
      scale_y_continuous(limits = c(0, NA))+
      theme_minimal(base_size = 16) +
      labs(title = "PORT Comparisons", x = "Year", y = "Score", color = "Port") +
      theme(legend.position = "right", 
            plot.title = element_text(face = "bold", hjust = 0.5), 
            strip.text = element_text(face = "bold", size = 18, hjust = 0.5),
            panel.spacing.y = unit(4, "lines"))
  })
  
  # Render Single Port Combined Timeline Plot with Dual Axis (Scores & Ranks)
  output$singlePortPlot <- renderPlot({
    req(input$single_place)
    
    # 1. Calculate ranks globally across all ports for each year before isolating the selected port
    raw_port_data <- df %>% 
      group_by(year) %>%
      mutate(
        rank_actors_score = min_rank(desc(port_actors_score)),
        rank_volume_score = min_rank(desc(port_volume_score)),
        rank_work_score   = min_rank(desc(port_work_score)),
        rank_ind_score    = min_rank(desc(port_ind_score))
      ) %>% 
      ungroup() %>%
      filter(place_id == input$single_place)
    
    # 2. Reshape and clean the Score series
    scores_long <- raw_port_data %>%
      select(year, port_actors_score, port_volume_score, port_work_score, port_ind_score) %>%
      pivot_longer(cols = -year, names_to = "Indicator", values_to = "Value") %>%
      mutate(
        Type = "Indicator Score (Solid Line)",
        Indicator = case_when(
          Indicator == "port_actors_score" ~ "Port Transaction Activity",
          Indicator == "port_volume_score" ~ "Port Volume Activity",
          Indicator == "port_work_score"   ~ "Port Permit Activity",
          Indicator == "port_ind_score"    ~ "Port Overall Activity",
          TRUE ~ Indicator
        )
      )
    
    # 3. Reshape the Rank series and scale them into a 0-1 range to align with the primary axis
    # Formula used: Mapped_Value = (20 - Rank) / 19
    ranks_long <- raw_port_data %>%
      select(year, rank_actors_score, rank_volume_score, rank_work_score, rank_ind_score) %>%
      pivot_longer(cols = -year, names_to = "Indicator", values_to = "Raw_Rank") %>%
      mutate(
        Type = "Annual Rank (Dashed Line)",
        Value = (20 - Raw_Rank) / 19,
        Indicator = case_when(
          Indicator == "rank_actors_score" ~ "Port Transaction Activity",
          Indicator == "rank_volume_score" ~ "Port Volume Activity",
          Indicator == "rank_work_score"   ~ "Port Permit Activity",
          Indicator == "rank_ind_score"    ~ "Port Overall Activity",
          TRUE ~ Indicator
        )
      )
    
    # Combine both tables together
    combined_plot_data <- bind_rows(scores_long, ranks_long)
    
    # 4. Draw the dual-axis chart
    ggplot(combined_plot_data, aes(x = year, y = Value, color = Indicator, linetype = Type, shape = Type, group = interaction(Indicator, Type))) +
      geom_line(linewidth = 1.2) +
      geom_point(size = 3, stroke = 1.2, fill = "white") + 
      scale_x_continuous(breaks = seq(2007, 2024, by = 2)) +
      scale_y_continuous(
        name = "Score",
        limits = c(0, NA),
        sec.axis = sec_axis(
          trans = ~ 20 - (. * 19), 
          name = "Annual Rank Position (Top 1 to 20)",
          breaks = c(5, 10, 15, 20) 
        )
      ) +
      scale_color_viridis_d(option = "viridis", end = 0.9) +
      scale_linetype_manual(values = c("Indicator Score (Solid Line)" = "solid", "Annual Rank (Dashed Line)" = "dashed")) +
      scale_shape_manual(values = c("Indicator Score (Solid Line)" = 16, "Annual Rank (Dashed Line)" = 24)) + 
      theme_minimal(base_size = 16) +
      labs(
        title = paste("Activity Trends & Rankings for:", input$single_place), # Centered Plot Title Added Here
        x = "Year",
        color = "Activity Categories",
        linetype = "Data Overlay Type",
        shape = "Data Overlay Type"
      ) +
      theme(
        plot.title = element_text(face = "bold", hjust = 0.5, margin = margin(b = 15)), # Centers title & adds bottom padding
        legend.position = "bottom",
        legend.box = "vertical",
        panel.grid.minor = element_blank()
      )
  })
}


# -------------------------------------------------------------------
# RUN THE APP
# -------------------------------------------------------------------
shinyApp(ui = ui, server = server)







