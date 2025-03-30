server <- function(input, output, session) {
  # Reactive value for current set data
  current_set <- reactive({
    req(input$set_select)
    
    switch(input$set_select,
           "Set 11" = get_set_data(11),
           "Set 12" = get_set_data(12),
           "Set 13" = get_set_data(13))
  })
  
  # Update emblem choices when set changes
  observeEvent(current_set(), {
    req(current_set()$craftable)
    emblems <- current_set()$craftable
    updatePickerInput(session, "emblem1", 
                      choices = c("None", sort(unique(emblems$trait_name))),
                      selected = input$emblem1 %||% "None")
    updatePickerInput(session, "emblem2", 
                      choices = c("None", sort(unique(emblems$trait_name))),
                      selected = input$emblem2 %||% "None")
    updatePickerInput(session, "emblem3", 
                      choices = c("None", sort(unique(emblems$trait_name))),
                      selected = input$emblem3 %||% "None")
  })
  
  # Update trait choices when set changes
  observeEvent(current_set(), {
    req(current_set()$traits)
    traits <- current_set()$traits %>%
      filter(map_int(breakpoints, length) > 1) %>%
      pull(trait_name) %>%
      sort()
    updatePickerInput(session, "force_vertical", 
                     choices = c("None", traits),
                     selected = input$force_vertical %||% "None")
  })
  
  # Generate compositions when button is clicked
  compositions <- eventReactive(input$generate, {
    req(current_set())
    
    # Get selected emblems (now using trait names directly)
    emblems <- c(input$emblem1, input$emblem2, input$emblem3)
    emblems <- emblems[emblems != "" & emblems != "None"]
    team_size <- input$team_size
    
    # Get forced trait if selected (using trait name)
    forced_trait <- if (input$force_vertical != "") input$force_vertical else NULL
    
    # Generate compositions
    if (!is.null(forced_trait)) {
      # Vertical composition
      vertical_comp <- generate_teams_correct(
        emblems = emblems,
        set_data = current_set(),
        strategy = "vertical",
        team_size = team_size,
        forced_trait = forced_trait
      )
    } else {
      vertical_comp <- NULL
    }
    
    # Balanced composition
    horiz_set_data <- current_set()
    if (input$horiz_cost_limit < 5) {
      horiz_set_data$units <- horiz_set_data$units %>% 
        filter(cost <= input$horiz_cost_limit)
    }
    
    balanced_comp <- generate_teams_correct(
      emblems = emblems,
      set_data = horiz_set_data,
      strategy = "horizontal",
      team_size = team_size
    )
    
    list(
      vertical = vertical_comp,
      balanced = balanced_comp
    )
  })
  
  # Vertical composition outputs
  output$primary_trait <- renderText({
    req(compositions()$vertical)
    compositions()$vertical$primary_trait
  })
  
  output$vertical_traits <- renderUI({
    req(compositions()$vertical)
    traits <- compositions()$vertical$activated_traits
    tagList(
      tags$ul(
        lapply(names(traits), function(trait) {
          tags$li(paste(trait, ":", traits[[trait]]))
        })
      )
    )
  })
    
  output$vertical_team <- renderUI({
    req(compositions()$vertical)
    team <- compositions()$vertical$team
    HTML(paste(team, collapse = "<br>"))
  })
  
  # Balanced composition outputs
  output$num_traits <- renderText({
    req(compositions()$balanced)
    compositions()$balanced$num_activated
  })
  
  output$balanced_traits <- renderUI({
    req(compositions()$balanced)
    traits <- compositions()$balanced$activated_traits
    tagList(
      tags$ul(
        lapply(names(traits), function(trait) {
          tags$li(paste(trait, ":", traits[[trait]]))
        })
      )
    )
  })
    
  output$balanced_team <- renderUI({
    req(compositions()$balanced)
    team <- compositions()$balanced$team
    HTML(paste(team, collapse = "<br>"))
  })
  
  # Trait breakdown plot
  output$trait_plot <- renderPlot({
    req(compositions())
    # You would implement your plotting logic here
    # For example:
    traits_df <- data.frame(
      Trait = names(compositions()$balanced$activated_traits),
      Count = unlist(compositions()$balanced$activated_traits)
    )
    
    ggplot(traits_df, aes(x = reorder(Trait, -Count), y = Count)) +
      geom_col(fill = "steelblue") +
      labs(title = "Trait Breakdown", x = "Trait", y = "Count") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  # Item guide
  output$item_guide <- renderUI({
    req(compositions())
    # You would implement your item recommendation logic here
    tagList(
      h3("Recommended Items"),
      tags$ul(
        tags$li("Example Item 1"),
        tags$li("Example Item 2"),
        tags$li("Example Item 3")
      )
    )
  })
  
  output$vertical_team <- renderUI({
    req(compositions()$vertical)
    display_team_with_images(compositions()$vertical$team, current_set())
  })
  
  output$balanced_team <- renderUI({
    req(compositions()$balanced)
    display_team_with_images(compositions()$balanced$team, current_set())
  })
  
  output$team_display <- renderUI({
    req(compositions()$vertical, current_set())
    
    # Get team data with validated costs
    team_data <- current_set()$units %>% 
      filter(unit_name %in% compositions()$vertical$team) %>%
      select(unit_name, pic, cost) %>%
      mutate(
        cost = as.integer(cost),
        cost = ifelse(is.na(cost) | cost < 1 | cost > 5, 1, cost),  # Validate cost
        pic = ifelse(is.na(pic) | pic == "", 
                     "https://via.placeholder.com/60?text=Champion", 
                     pic)  # Ensure image URL exists
      ) %>%
      arrange(desc(cost), unit_name)  # Sort by cost then name
    
    # Create champion cards with proper styling
    tags$div(
      class = "team-display",
      lapply(1:nrow(team_data), function(i) {
        tags$div(
          class = "champion-card",
          style = paste0("border: 3px solid ", 
                         get_cost_color(team_data$cost[i]), ";"),  # Dynamic border color
          tags$img(
            src = team_data$pic[i],
            class = "champion-image",
            alt = team_data$unit_name[i],
            onerror = "this.src='https://via.placeholder.com/60?text=Champion';"
          ),
          tags$div(
            class = "champion-info",
            tags$span(class = "champion-name", team_data$unit_name[i]),
            tags$span(class = "champion-cost", paste0("(", team_data$cost[i], ")"))
          )
        )
      })
    )
  })
}