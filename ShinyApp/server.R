server <- function(input, output, session) {
  # Reactive value for current set data
  current_set <- reactive({
    req(input$set_select)  # Ensure set is selected
    
    # Load set data
    switch(input$set_select,
           "Set 11" = get_set_data(11),
           "Set 12" = get_set_data(12),
           "Set 13" = get_set_data(13))
  })
  
  # Update emblem choices when set changes
  observeEvent(current_set(), {
    req(current_set()$craftable)  # Ensure emblems data exists
    
    emblems <- current_set()$craftable
    choices <- c("None" = "", setNames(emblems$trait_id, emblems$trait_name))
    
    updatePickerInput(session, "emblem1", choices = choices)
    updatePickerInput(session, "emblem2", choices = choices)
    updatePickerInput(session, "emblem3", choices = choices)
  })
  
  # Update trait choices when set changes
  observeEvent(current_set(), {
    req(current_set()$traits)  # Ensure traits data exists
    
    traits <- current_set()$traits %>%
      filter(map_int(breakpoints, length) > 1) # Remove units specific traits
    choices <- c("None" = "", setNames(traits$trait_id, traits$trait_name))
    
    updatePickerInput(session, "force_vertical", choices = choices)
  })
  
  # Reactive emblem selections
  selected_emblems <- reactive({
    req(input$emblem1, input$emblem2, input$emblem3)
    
    emblems <- c(input$emblem1, input$emblem2, input$emblem3)
    emblems[emblems != ""]  # Remove empty selections
  })
  
  # Reactive team configuration
  team_config <- reactive({
    req(input$team_size)
    
    list(
      size = input$team_size,
      force_vertical = input$force_vertical
    )
  })
  
  # Generate compositions when button is clicked
  compositions <- eventReactive(input$generate, {
    req(selected_emblems(), team_config())
    
    # Replace with your actual composition generation logic
    generate_compositions(
      emblems = selected_emblems(),
      config = team_config(),
      set_data = current_set()
    )
  })
  
  # Render compositions table
  output$compositions_table <- DT::renderDT({
    req(compositions())
    
    DT::datatable(
      compositions()$data,
      options = list(
        pageLength = 5,
        autoWidth = TRUE,
        dom = 'tip',
        scrollX = TRUE
      ),
      rownames = FALSE,
      selection = 'single'
    ) %>%
      DT::formatStyle(columns = names(compositions()$data), fontSize = '14px')
  })
  
  # Render trait breakdown plot
  output$trait_plot <- renderPlot({
    req(compositions())
    
    # Replace with your actual plotting logic
    plot_trait_breakdown(compositions()$traits)
  })
  
  # Render item guide
  output$item_guide <- renderUI({
    req(compositions())
    
    # Replace with your actual item guide logic
    generate_item_guide(compositions()$items)
  })
  
  generate_compositions <- function(emblems, config, set_data) {
    # Your composition generation logic
    list(
      data = data.frame(
        Composition = c("Comp 1", "Comp 2"),
        Traits = c("Trait A + B", "Trait C + D"),
        Units = c("Unit1, Unit2", "Unit3, Unit4"),
        stringsAsFactors = FALSE
      ),
      traits = data.frame(
        Trait = c("A", "B"),
        Count = c(3, 5)
      ),
      items = c("Item1", "Item2")
    )
  }
  
  plot_trait_breakdown <- function(traits) {
    # Your plotting logic
    ggplot(traits, aes(x = Trait, y = Count)) +
      geom_col(fill = "steelblue") +
      theme_minimal()
  }
  
  generate_item_guide <- function(items) {
    # Your item guide logic
    tagList(
      h3("Recommended Items"),
      tags$ul(
        lapply(items, function(item) tags$li(item))
      )
    )
  }
}