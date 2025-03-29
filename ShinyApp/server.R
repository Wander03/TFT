library(shiny)
library(ggplot2)
library(scales)
library(RColorBrewer)

function(input, output, session) {
  # Dynamic UI for website, country, or author selection based on user's choice
  output$analysis_input <- renderUI({
    if (input$selection_type == "Website") {
      selectInput("website_input", "Choose a website:",
                  choices = unique(fake_news$site_url))
    } else if (input$selection_type == "Country") {
      selectInput("country_input", "Choose a country:",
                  choices = countries)
    } else if (input$selection_type == "Author") {
      selectInput("author_input", "Choose an author:",
                  choices = authors)
    } else {
      NULL
    }
  })
  
  # Define a reactive expression for the document term matrix
  terms <- reactive({
    # Change when the "update" button is pressed...
    input$update
    # ...but not for anything else
    isolate({
      withProgress({
        setProgress(message = "Processing corpus...")
        if (input$selection_type == "Website") {
          getTermMatrix(input$website_input, "website")
        } else if (input$selection_type == "Country") {
          getTermMatrix(input$country_input, "country")
        } else if (input$selection_type == "Author") {
          getTermMatrix(input$author_input, "author")
        }
      })
    })
  })
  
  selected_value <- eventReactive(
    # Change when the "update" button is pressed...
    input$update,
    {
      # ...but not for anything else      
      isolate({
        paste("Percentage of sentiments expressed\n",
              "in news articles ", 
              case_when(
                input$selection_type == "Country" 
                ~ paste("in the", input$country_input),
                input$selection_type == "Website"
                ~ paste("from", input$website_input),
                input$selection_type == "Author"
                ~ paste("by", input$author_input),
                TRUE ~ ""
              ),
              sep = "")
      })
    })
  
  # Make the wordcloud drawing predictable during a session
  wordcloud_rep <- repeatable(wordcloud)
  
  output$cloudplot <- renderPlot({
    tryCatch({    
      v <- terms()
      wordcloud_rep(names(v), v, scale=c(4,0.5),
                    min.freq = input$freq, max.words = input$max,
                    colors = brewer.pal(8, "Dark2"))
    }, error = function(e) {
      # If there's an error, return NULL (no plot)
      NULL
    })  
  })
  
  output$barchart <- renderPlot({
    tryCatch({  
      v <- terms()
      sentiment_scores <- term_sentiment_analysis(v)
      
      # Define the user-specified sorting order
      custom_order <- c("joy", "trust", "anticipation", "surprise",
                        "sadness", "fear", "disgust", "anger")
      
      sentiment_props <- sentiment_scores[, 1:8]  %>%
        prop.table() %>%
        colSums()
      
      # Plot using ggplot
      data.frame(emotions = names(sentiment_props), counts = sentiment_props) %>%
        # Convert the 'name' column to a factor with the custom order
        mutate(emotions = factor(emotions, levels = custom_order)) %>%
        arrange(emotions) %>%
        ggplot(aes(x = emotions, y = counts, fill = emotions)) +
        geom_bar(stat = "identity", position = "dodge") +
        labs(
          title = selected_value(),
          x = "",
          y = ""
        ) +
        theme_bw() +
        scale_y_continuous(
          labels = label_percent(),
          sec.axis = dup_axis()
        ) +
        scale_fill_brewer(palette = "RdYlGn", direction = -1) + 
        coord_flip() +
        theme(
          legend.position = "none",
          plot.title = element_text(hjust = 0, size = 16),
        )
      # Note: each word can have multiple sentiments associated with it
    }, error = function(e) {
      # If there's an error, return NULL (no plot)
      NULL
    })      
  })
  output$piechart <- renderPlotly({
    tryCatch({    
      v <- terms()
      sentiment_scores <- term_sentiment_analysis(v)
      sentiment_props <- sentiment_scores[, 1:8] %>%
        prop.table() %>%
        colSums()
      
      pie_chart_data <- data.frame(emotions = names(sentiment_props), Percent = sentiment_props)
      
      plot_ly(pie_chart_data, labels = ~emotions, 
              values = ~Percent, type = "pie",
              textinfo = "label+percent",
              hoverinfo = "text",
              text = ~paste(emotions, "<br>", Percent, "%"),
              marker = list(colors = rainbow(length(unique(pie_chart_data$emotions)))))
    }, error = function(e) {
      # If there's an error, return NULL (no plot)
      NULL
    })    
  })
  
  
  # output$barchart2 <- renderPlot({
  #   pos_neg_props <- sentiment_scores[, 9:10]  %>%
  #     prop.table() %>%
  #     colSums
  #   
  #   # Positive vs Negative
  #   data.frame(emotions = names(pos_neg_props), counts = pos_neg_props) %>%
  #     ggplot(aes(x = "", y = counts, fill = emotions)) +
  #     geom_bar(stat = "identity") +
  #     labs(
  #       title = "<span style = 'color:blue;'>Positive</span> vs 
  #   <span style = 'color:orange;'>Negative</span> Sentiment",
  #       x = "",
  #       y = ""
  #     ) +
  #     geom_text(
  #       aes(label = emotions), 
  #       position = position_stack(vjust = 0.5), 
  #       color = "white", 
  #       size = 5) +
  #     scale_fill_manual(values = c("#ff7f0e", "#1f77b4")) +   
  #     theme_minimal() +  
  #     theme(legend.position = "none")
  # })
}
