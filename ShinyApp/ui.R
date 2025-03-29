library(plotly)
fluidPage(
  # Application title
  titlePanel("Exploring Fake News"),
  
  sidebarLayout(
    # Sidebar with a select input for choosing between website, country, and author
    sidebarPanel(
      selectInput("selection_type", "Choose Analysis Type:",
                  choices = c("Country", "Website", "Author")),  # Add website, country, and author options
      uiOutput("analysis_input"),  # Dynamic UI for website, country, or author selection
      actionButton("update", "Submit"),
      
      # Title above sliders
      hr(),
      tags$br(),  # Line break
      tags$h4("For Word Cloud"),  # Title
      
      sliderInput("freq",
                  "Minimum Frequency:",
                  min = 1,  max = 50, value = 5),
      sliderInput("max",
                  "Maximum Number of Words:",
                  min = 1,  max = 100,  value = 50)
    ),
    
    # Show Plots
    mainPanel(
      plotOutput("cloudplot"),
      plotOutput("barchart"),
      plotlyOutput("piechart")
    )
    
  )
)
