library(shinyWidgets)

server <- function(input, output, session) {
  
  # Reactive value for current set data
  current_set <- reactive({
    req(input$set_select)
    
    switch(input$set_select,
           "Set 11" = get_set_data(11),
           "Set 12" = get_set_data(12),
           "Set 13" = get_set_data(13),
           "Set 14" = get_set_data(14))
  })
  
  # Reactive value for selected units
  selected_units <- reactiveVal(character(0))
  
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
  
  # Render unit selection grid
  # Helper function to create selectable unit cards
  create_unit_cards <- function(units_df, selected_units) {
    div(
      class = "team-display",
      apply(units_df, 1, function(unit) {
        unit <- as.list(unit)  # Convert row to list
        cost <- as.integer(unit$cost)
        selected <- unit$unit_name %in% selected_units()
        
        tags$div(
          class = "champion-container",
          style = if(selected) "border: 2px solid gold; border-radius: 8px;" else "",
          tags$div(
            class = paste("champion-card", paste0("cost-", cost)),
            style = paste0("--cost-color: ", get_cost_color(cost), ";"),
            # Click handler
            onclick = paste0(
              "Shiny.setInputValue('unit_click', '", 
              unit$unit_name, 
              "', {priority: 'event'});"
            ),
            # Trait tooltip
            tags$div(
              class = "trait-tooltip",
              if (!is.null(unit$traits) && unit$traits != "") {
                lapply(strsplit(unit$traits, ", ")[[1]], function(trait) {
                  tags$span(class = "trait-badge", trait)
                })
              }
            ),
            tags$img(
              src = unit$pic,
              class = "champion-img",
              alt = unit$unit_name,
              onerror = "this.src='https://via.placeholder.com/60?text=Champion';"
            ),
            tags$div(
              class = "champion-name",
              unit$unit_name
            ),
            tags$div(
              class = "champion-cost",
              cost
            )
          )
        )
      })
    )
  }
  
  # Render unit cards by cost
  output$cost1_units <- renderUI({
    req(current_set())
    units <- current_set()$units %>% 
      filter(cost == 1) %>%
      arrange(unit_name) %>%
      as.data.frame()  # Ensure it's a data frame
    create_unit_cards(units, selected_units)
  })
  
  output$cost2_units <- renderUI({
    req(current_set())
    units <- current_set()$units %>% 
      filter(cost == 2) %>%
      arrange(unit_name) %>%
      as.data.frame()
    create_unit_cards(units, selected_units)
  })
  
  output$cost3_units <- renderUI({
    req(current_set())
    units <- current_set()$units %>% 
      filter(cost == 3) %>%
      arrange(unit_name) %>%
      as.data.frame()
    create_unit_cards(units, selected_units)
  })
  
  output$cost4_units <- renderUI({
    req(current_set())
    units <- current_set()$units %>% 
      filter(cost == 4) %>%
      arrange(unit_name) %>%
      as.data.frame()
    create_unit_cards(units, selected_units)
  })
  
  output$cost5_units <- renderUI({
    req(current_set())
    units <- current_set()$units %>% 
      filter(cost == 5) %>%
      arrange(unit_name) %>%
      as.data.frame()
    create_unit_cards(units, selected_units)
  })
  
  # Handle unit selection clicks
  observeEvent(input$unit_click, {
    current <- selected_units()
    unit <- input$unit_click
    
    if (unit %in% current) {
      selected_units(setdiff(current, unit))
    } else {
      selected_units(c(current, unit))
    }
  })
  
  # Generate vertical composition with forced units
  # Generate vertical composition with forced units
  vertical_comp <- eventReactive(input$generate, {
    req(current_set())
    
    # Get selected emblems
    emblems <- c(input$emblem1, input$emblem2, input$emblem3)
    emblems <- emblems[emblems != "" & emblems != "None"]
    
    # Get forced trait if selected (now optional)
    forced_trait <- if (input$force_vertical != "None") input$force_vertical else NULL
    
    # Get selected units
    forced_units <- selected_units()
    
    # Generate composition with forced units
    if (length(forced_units) > 0) {
      # First, create a team with the forced units
      if (length(forced_units) >= input$team_size) {
        # If we already have enough selected units, just use them
        team <- forced_units[1:input$team_size]
      } else {
        # Generate a partial team for remaining slots
        partial_team <- generate_vertical_team(
          emblems = emblems,
          set_data = current_set(),
          team_size = input$team_size - length(forced_units),
          forced_trait = forced_trait,
          excluded_units = forced_units
        )$team
        
        # Combine forced units with generated units
        team <- unique(c(forced_units, partial_team))[1:input$team_size]
      }
      
      # Get traits for the combined team
      activated_traits <- count_activated_traits(
        team, 
        emblems, 
        preprocess_units(current_set()$units), 
        precompute_trait_thresholds(current_set()$traits)
      )
      
      # Determine primary trait (most activated)
      primary_trait <- names(which.max(
        map_dbl(names(activated_traits), ~ max(
          precompute_trait_thresholds(current_set()$traits)[[.x]] %||% 0
        ))
      ))
      
      list(
        team = team,
        activated_traits = activated_traits,
        num_activated = length(activated_traits),
        primary_trait = primary_trait
      )
    } else {
      # No forced units - generate normally
      generate_vertical_team(
        emblems = emblems,
        set_data = current_set(),
        team_size = input$team_size,
        forced_trait = forced_trait
      )
    }
  })
  
  # Helper function for vertical team generation
  generate_vertical_team <- function(emblems, set_data, team_size, forced_trait = NULL, excluded_units = NULL) {
    # Preprocess data
    units_df <- preprocess_units(set_data$units)
    trait_thresholds <- precompute_trait_thresholds(set_data$traits)
    spec_traits <- get_unit_specific_traits(set_data$units)
    
    # If no trait forced, find the best trait to go vertical with
    if (is.null(forced_trait)) {
      # Get all traits from current emblems and units
      all_traits <- unique(c(emblems, unlist(units_df$traits)))
      
      # Find trait with highest potential activation
      trait_potential <- map_dbl(all_traits, ~ {
        max(trait_thresholds[[.x]] %||% 0)
      })
      
      if (length(trait_potential) > 0) {
        forced_trait <- all_traits[which.max(trait_potential)]
      } else {
        # Fallback if no traits found
        forced_trait <- names(trait_thresholds)[1]
      }
    }
    
    # Filter out excluded units if any
    if (!is.null(excluded_units)) {
      units_df <- units_df %>% 
        filter(!unit_name %in% excluded_units)
    }
    
    # Find units with the forced trait
    primary_units <- units_df %>%
      filter(map_lgl(traits, ~ forced_trait %in% .x)) %>%
      arrange(desc(cost)) %>%
      pull(unit_name)
    
    selected_units <- character(0)
    current_counts <- setNames(rep(0, length(names(trait_thresholds))), names(trait_thresholds))
    current_counts[emblems] <- table(emblems)
    
    # Add primary trait units first
    for (unit in primary_units) {
      if (length(selected_units) >= team_size) break
      
      selected_units <- c(selected_units, unit)
      unit_traits <- units_df$traits[[which(units_df$unit_name == unit)]]
      for (trait in unit_traits) {
        if (trait %notin% spec_traits) {
          current_counts[trait] <- current_counts[trait] + 1
        }
      }
    }
    
    # Fill remaining slots with units that activate the most new traits
    if (length(selected_units) < team_size) {
      remaining_units <- units_df %>%
        filter(!unit_name %in% selected_units) %>%
        mutate(
          new_activations = map_dbl(unit_name, ~ {
            unit_traits <- units_df$traits[[which(units_df$unit_name == .x)]]
            sum(map_dbl(unit_traits, ~ {
              thresh <- trait_thresholds[[.x]] %||% numeric(0)
              current_active <- any(current_counts[.x] >= thresh)
              new_active <- any((current_counts[.x] + 1) >= thresh)
              ifelse(new_active && !current_active, 1, 0)
            }))
          })
        ) %>%
        arrange(desc(new_activations), desc(cost)) %>%
        pull(unit_name)
      
      to_add <- min(length(remaining_units), team_size - length(selected_units))
      selected_units <- c(selected_units, remaining_units[1:to_add])
    }
    
    # Calculate final activated traits
    activated_traits <- count_activated_traits(
      selected_units, 
      emblems, 
      units_df, 
      trait_thresholds
    )
    
    list(
      team = selected_units,
      activated_traits = activated_traits,
      num_activated = length(activated_traits),
      primary_trait = forced_trait
    )
  }
  
  # Vertical composition outputs
  output$primary_trait <- renderText({
    req(vertical_comp())
    vertical_comp()$primary_trait
  })
  
  output$vertical_traits <- renderUI({
    req(vertical_comp())
    traits <- vertical_comp()$activated_traits
    tagList(
      tags$ul(
        lapply(names(traits), function(trait) {
          tags$li(paste(trait, ":", traits[[trait]]))
        })
      )
    )
  })
  
  output$vertical_team <- renderUI({
    req(vertical_comp())
    display_team_with_images(vertical_comp()$team, current_set())
  })
  
  # Handle Deselect All button click
  observeEvent(input$deselect_all, {
    selected_units(character(0))
  })
  
  # Generate horizontal compositions with forced units
  horizontal_comps <- eventReactive(input$generate, {
    req(current_set(), input$ga_iterations_horizontal)
    
    # Get selected emblems
    emblems <- c(input$emblem1, input$emblem2, input$emblem3)
    emblems <- emblems[emblems != "" & emblems != "None"]
    
    # Get selected units
    forced_units <- selected_units()
    
    # Prepare set data with cost limit if needed
    horiz_set_data <- current_set()
    if (input$horiz_cost_limit < 5) {
      horiz_set_data$units <- horiz_set_data$units %>% 
        filter(cost <= input$horiz_cost_limit)
    }
    
    # Generate using the correct number of iterations
    generate_teams_correct(
      emblems = emblems,
      set_data = horiz_set_data,
      strategy = "horizontal",
      team_size = input$team_size,
      ga_iterations = input$ga_iterations_horizontal  # Use tab-specific input
    )
  })
  
  # Horizontal compositions output
  output$horizontal_comps_list <- renderUI({
    comps <- horizontal_comps()
    
    if (is.null(comps)) {
      return(tags$div(
        class = "alert alert-info", 
        "Generate compositions to see results. Make sure you're on the Horizontal Compositions tab when generating."
      ))
    }
    
    # Handle both single comp and multiple comps
    if (!is.list(comps[[1]])) {  # Single composition case
      comps <- list(comps)
    }
    
    # Find maximum activated traits
    max_activated <- max(map_dbl(comps, ~ .x$num_activated))
    top_comps <- keep(comps, ~ .x$num_activated == max_activated)
    
    tagList(
      h4(paste("Showing", length(top_comps), "top composition(s) with", max_activated, "activated traits")),
      
      map(top_comps, function(comp) {
        div(
          class = "team-comp-container",
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
  
}

