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
  
  # Generate multiple horizontal compositions
  horizontal_comps <- reactive({
    req(input$generate)
    
    # Get validated emblems
    emblems <- c(input$emblem1, input$emblem2, input$emblem3)
    emblems <- emblems[emblems != "" & emblems != "None"]
    
    # Get current set data
    set_data <- current_set()
    
    # Validate we have units to work with
    if (nrow(set_data$units) == 0) {
      message("No units available in set data")
      return(NULL)
    }
    
    # Apply cost limit if specified
    if (input$horiz_cost_limit < 5) {
      set_data$units <- set_data$units %>% 
        filter(cost <= input$horiz_cost_limit)
      
      # Check if we still have units after filtering
      if (nrow(set_data$units) == 0) {
        message("No units available after cost filtering")
        return(NULL)
      }
    }
    
    # Generate multiple compositions with error handling
    tryCatch({
      comps <- map(1:5, ~ {
        set.seed(.x)
        generate_teams_correct(
          emblems = emblems,
          set_data = set_data,
          strategy = "horizontal",
          team_size = input$team_size
        )
      })
      
      # Remove NULL results and duplicates
      comps <- comps %>% 
        compact() %>% 
        unique()
      
      # Order by number of activated traits
      if (length(comps) > 0) {
        comps[order(-map_dbl(comps, ~ .x$num_activated))]
      } else {
        message("No valid compositions generated")
        NULL
      }
    }, error = function(e) {
      message("Error generating teams: ", e$message)
      NULL
    })
  })
  
  # Display the list of horizontal comps
  output$horizontal_comps_list <- renderUI({
    comps <- horizontal_comps()
    
    if (is.null(comps)) {
      return(tags$div(
        class = "alert alert-danger", 
        "Could not generate team compositions. This might happen if:",
        tags$ul(
          tags$li("The cost limit is too restrictive"),
          tags$li("The selected emblems don't match any traits"),
          tags$li("There aren't enough units in the selected set")
        ),
        "Try adjusting your settings and generating again."
      ))
    }
    
    tagList(
      h3("Top Horizontal Compositions", style = "color: #3a5a78;"),
      map(comps, function(comp) {
        div(
          class = "team-comp-container",
          h4(paste("Variant with", comp$num_activated, "Activated Traits")),
          
          h5("Activated Traits:"),
          if (length(comp$activated_traits) > 0) {
            tags$ul(
              map(names(comp$activated_traits), function(trait) {
                tags$li(paste(trait, ":", comp$activated_traits[[trait]]))
              })
            )
          } else {
            p("No traits activated")
          },
          
          h5("Team Composition:"),
          display_team_with_images(comp$team, current_set()),
          
          hr()
        )
      })
    )
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