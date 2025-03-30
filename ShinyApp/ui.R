library(shiny)
library(shinyWidgets)
library(bslib)

ui <- fluidPage(
  theme = bs_theme(bootswatch = "flatly"),
  titlePanel("Wandering Trainer Helper"),
  
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
      
      hr(),
      h4("Emblem Selection"),
      pickerInput("emblem1", "Emblem 1",
                  choices = NULL,
                  options = list(`live-search` = TRUE)),
      pickerInput("emblem2", "Emblem 2", 
                  choices = NULL,
                  options = list(`live-search` = TRUE)),
      pickerInput("emblem3", "Emblem 3",
                  choices = NULL,
                  options = list(`live-search` = TRUE)),
      
      hr(),
      h4("Strategy Options"),
      pickerInput("force_vertical", "Force Vertical",
                  choices = NULL,
                  multiple = FALSE,
                  options = list(`actions-box` = TRUE,
                                 `live-search` = TRUE)),
      
      actionButton("generate", "Generate Composition",
                   class = "btn-primary",
                   icon = icon("chess-board"))
    ),
    
    mainPanel(
      width = 9,
      tabsetPanel(
        tabPanel("Recommended Comps",
                 DT::DTOutput("compositions_table")),  # Corrected here
        tabPanel("Trait Breakdown",
                 plotOutput("trait_plot")),
        tabPanel("Item Guide",
                 uiOutput("item_guide"))
      )
    )
  )
)