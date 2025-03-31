ui <- fluidPage(
  theme = bs_theme(bootswatch = "flatly"),
  titlePanel("TFT Team Compositions Generator"),
  
  # Add custom CSS for champion displays
  tags$style(HTML("
    /* Main team container */
    .team-display {
      display: grid;
      grid-template-columns: repeat(auto-fill, minmax(90px, 1fr));
      gap: 15px;
      padding: 15px;
      background: #f8f9fa;
      border-radius: 8px;
    }
    
    /* Champion card */
    .champion-card {
      display: flex;
      flex-direction: column;
      align-items: center;
      padding: 8px;
      border-radius: 8px;
      background: color-mix(in srgb, var(--cost-color) 10%, white 90%);
      border: 3px solid var(--cost-color);
      box-shadow: 0 2px 4px rgba(0,0,0,0.1);
      transition: transform 0.2s;
    }
    
    /* Hover effect */
    .champion-card:hover {
      transform: scale(1.05);
      z-index: 1;
    }
    
    /* Champion image */
    .champion-img {
      width: 60px;
      height: 60px;
      border-radius: 5px;
      object-fit: cover;
      margin-bottom: 5px;
      border: 2px solid rgba(255,255,255,0.3);
    }
    
    /* Champion name - improved visibility */
    .champion-name {
      font-size: 12px;
      font-weight: 700;
      color: #333;
      text-align: center;
      margin-top: 3px;
      text-shadow: 0 1px 1px rgba(255,255,255,0.7);
      white-space: nowrap;
      overflow: hidden;
      text-overflow: ellipsis;
      max-width: 100%;
    }
    
    /* Cost indicator */
    .champion-cost {
      font-size: 10px;
      font-weight: 600;
      color: white;
      background-color: var(--cost-color);
      border-radius: 10px;
      padding: 1px 6px;
      margin-top: 3px;
      display: inline-block;
    }
    
    /* Responsive adjustments */
    @media (max-width: 768px) {
      .team-display {
        grid-template-columns: repeat(auto-fill, minmax(80px, 1fr));
      }
      .champion-img {
        width: 50px;
        height: 50px;
      }
      .champion-name {
        font-size: 11px;
      }
    }
    
    /* Trait popup styling */
    .trait-tooltip {
      position: absolute;
      bottom: calc(100% + 5px);
      left: 50%;
      transform: translateX(-50%);
      background: #2d3748;
      color: white;
      padding: 6px 10px;
      border-radius: 4px;
      font-size: 12px;
      white-space: nowrap;
      z-index: 10;
      opacity: 0;
      visibility: hidden;
      transition: all 0.15s ease-out;
      pointer-events: none;
      box-shadow: 0 2px 8px rgba(0,0,0,0.2);
      border: 1px solid rgba(255,255,255,0.1);
    }
    
    /* Arrow for tooltip */
    .trait-tooltip::after {
      content: '';
      position: absolute;
      top: 100%;
      left: 50%;
      transform: translateX(-50%);
      border-width: 5px;
      border-style: solid;
      border-color: #2d3748 transparent transparent transparent;
    }
    
    .champion-container:hover .trait-tooltip {
      opacity: 1;
      visibility: visible;
      transform: translateX(-50%) translateY(-5px);
    }
    
    /* Individual trait styling */
    .trait-badge {
      display: inline-block;
      background: rgba(255,255,255,0.1);
      padding: 2px 6px;
      border-radius: 10px;
      margin: 1px;
      font-size: 11px;
    }
    
    .team-comp-container {
    margin-bottom: 30px;
    padding: 20px;
    background: #f8f9fa;
    border-radius: 8px;
    border-left: 5px solid #3a5a78;
    box-shadow: 0 2px 4px rgba(0,0,0,0.1);
  }
  
  .team-comp-container h4 {
    color: #3a5a78;
    margin-top: 0;
    border-bottom: 1px solid #ddd;
    padding-bottom: 10px;
  }
  
  .team-comp-container ul {
    margin-top: 5px;
    margin-bottom: 15px;
    padding-left: 20px;
  }
  
  .team-comp-container li {
    margin-bottom: 5px;
  }
  
  .team-comp-container {
    margin-bottom: 30px;
    padding: 20px;
    background: #f8f9fa;
    border-radius: 8px;
    border-left: 5px solid #3a5a78;
  }
  .alert-danger {
    padding: 15px;
    background-color: #f8d7da;
    border-color: #f5c6cb;
    color: #721c24;
    border-radius: 4px;
  }
  
  /* Selectable unit cards */
  .champion-container {
    cursor: pointer;
    transition: all 0.2s;
  }
  
  .champion-container:hover {
    transform: scale(1.05);
  }
  
  /* Selected unit style */
  .champion-container.selected {
    border: 2px solid gold;
    border-radius: 8px;
    box-shadow: 0 0 10px gold;
  }
  
  /* Cost headers */
  h4 {
    color: #3a5a78;
    margin-bottom: 15px;
    padding-bottom: 5px;
    border-bottom: 1px solid #ddd;
  }
  
  /* Deselect All button styling */
  .btn-danger {
    background-color: #dc3545;
    border-color: #dc3545;
    color: white;
    font-weight: bold;
    padding: 5px 10px;
    margin-bottom: 15px;
  }
  
  .btn-danger:hover {
    background-color: #c82333;
    border-color: #bd2130;
  }
  
  /* Horizontal compositions styling */
  .team-comp-container {
    margin-bottom: 30px;
    padding: 20px;
    background: #f8f9fa;
    border-radius: 8px;
    border-left: 5px solid #3a5a78;
    box-shadow: 0 2px 4px rgba(0,0,0,0.1);
  }
  
  .team-comp-container h4 {
    color: #3a5a78;
    margin-top: 0;
    border-bottom: 1px solid #ddd;
    padding-bottom: 10px;
  }
  
  .alert-warning {
    padding: 15px;
    background-color: #fff3cd;
    border-color: #ffeeba;
    color: #856404;
    border-radius: 4px;
  }
  
  /* Style for the number of comps input */
  #num_horizontal_comps {
    width: 80px;
    margin-left: 10px;
  }
  
  /* Horizontal tab header styling */
  #horizontal-comps .well-panel {
    padding: 15px;
  }
  
  #horizontal-comps .well-panel > div {
    margin-bottom: 15px;
  }
  
  /* Numeric input in header */
  #ga_iterations_horizontal {
    width: 80px;
    margin-left: 10px;
  }
  
  #ga_iterations_horizontal .form-control {
    height: 34px;
    padding: 6px 12px;
  }
")),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      h4("Set Selection"),
      selectInput("set_select", "Choose Set:",
                  choices = c("Set 11", "Set 12", "Set 13"),
                  selected = "Set 13"),
      
      h4("Team Configuration"),
      numericInput("team_size", "Team Size", 
                   value = 8, min = 1, max = 10, step = 1),
      
      pickerInput("horiz_cost_limit", "Max Unit Cost (BFL Only):",
                  choices = c("No limit" = 5, 1, 2, 3, 4, 5),
                  selected = 5,
                  multiple = FALSE,
                  options = list(`live-search` = FALSE)),
      
      hr(),
      h4("Emblem Selection"),
      pickerInput("emblem1", "Emblem 1",
                  choices = c("None"),
                  selected = "None",
                  options = list(`live-search` = TRUE)),
      pickerInput("emblem2", "Emblem 2", 
                  choices = c("None"),
                  selected = "None",
                  options = list(`live-search` = TRUE)),
      pickerInput("emblem3", "Emblem 3",
                  choices = c("None"),
                  selected = "None",
                  options = list(`live-search` = TRUE)),
      hr(),
      h4("Strategy Options"),
      pickerInput("force_vertical", "Force Vertical Trait",
                  choices = c("None"),
                  selected = "None",
                  multiple = FALSE,
                  options = list(`live-search` = TRUE)),
      actionButton("generate", "Generate Compositions",
                   class = "btn-primary",
                   icon = icon("chess-board"))
    ),
    
    mainPanel(
      width = 9,
      tabsetPanel(
        id = "tabs",
        tabPanel("Unit Selection",
                 fluidRow(
                   column(12,
                          wellPanel(
                            div(style = "display: flex; justify-content: space-between; align-items: center;",
                                h3("Select Units to Force in Compositions"),
                                actionButton("deselect_all", "Deselect All", 
                                             class = "btn-danger",
                                             icon = icon("times-circle"))
                            ),
                            # Cost 1 Units
                            h4("1-Cost Units", style = "margin-top: 20px;"),
                            uiOutput("cost1_units"),
                            # Cost 2 Units
                            h4("2-Cost Units", style = "margin-top: 20px;"),
                            uiOutput("cost2_units"),
                            # Cost 3 Units
                            h4("3-Cost Units", style = "margin-top: 20px;"),
                            uiOutput("cost3_units"),
                            # Cost 4 Units
                            h4("4-Cost Units", style = "margin-top: 20px;"),
                            uiOutput("cost4_units"),
                            # Cost 5 Units
                            h4("5-Cost Units", style = "margin-top: 20px;"),
                            uiOutput("cost5_units")
                          )
                   )
                 )
        ),
        tabPanel("Vertical Composition",
                 fluidRow(
                   column(12,
                          wellPanel(
                            h3("Best Vertical Composition", style = "color: #4a7c59;"),
                            h4("Primary Trait:", style = "font-weight: bold;"),
                            textOutput("primary_trait"),
                            hr(),
                            h4("Activated Traits:"),
                            uiOutput("vertical_traits"),
                            hr(),
                            h4("Team Composition:"),
                            uiOutput("vertical_team")
                          )
                   )
                 )
        ),
        tabPanel("Horizontal Compositions",
                 wellPanel(
                   div(style = "display: flex; justify-content: space-between; align-items: center;",
                       h3("Bronze for Life Compositions", style = "color: #4a7c59;"),
                       numericInput("ga_iterations_horizontal", 
                                    label = NULL,
                                    value = 3,
                                    min = 1, 
                                    max = 20,
                                    step = 1,
                                    width = '100px')
                   ),
                   uiOutput("horizontal_comps_list")
                 )
        )
      )
    )
  )
)