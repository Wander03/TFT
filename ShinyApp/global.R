library(tidyverse)
library(jsonlite)
library(memoise)

# TODO: add check for if set exists in get_set (or manually list these options for user to choose)

url <- "https://raw.communitydragon.org/latest/cdragon/tft/en_us.json"
tft_data <- fromJSON(url)

# Unit and Trait Info for Set X
get_set <- memoise(function(set = NA) {
  
  if (is.na(set)) {
    set_data <- tft_data$sets[[length(tft_data$setData)]]
  } else {
    set_data <- tft_data$sets[[as.character(set)]]
  }

  # Create the units table
  units_table <- data.frame(
    unit_id = set_data$champions$apiName,
    unit_name = set_data$champions$name,
    cost = set_data$champions$cost,
    traits = sapply(set_data$champions$traits, 
                    function(x) paste(x, collapse = ", ")),
    stringsAsFactors = FALSE
  ) %>%
    filter(nchar(traits) > 0)  # Remove units with no traits
  
  # Create the traits table
  traits_table <- data.frame(
    trait_id = set_data$traits$apiName,
    trait_name = set_data$traits$name,
    stringsAsFactors = FALSE
  )
  
  breakpoints <- lapply(set_data$traits$effects, function(eff) {
    if (!is.null(eff)) {
      units_needed = eff$minUnits
    }
  })
  
  traits_table <- traits_table %>%
    mutate(
      breakpoints = map(breakpoints, function(bp) {
        if (is.null(bp)) NA else as.character(bp)
      })
    )
  
  return(list(units = units_table, traits = traits_table))
  
})
