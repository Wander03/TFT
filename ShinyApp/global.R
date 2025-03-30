library(tidyverse)
library(jsonlite)
library(memoise)

# TODO: add check for if set exists in get_set (or manually list these options for user to choose)

url <- "https://raw.communitydragon.org/latest/cdragon/tft/en_us.json"
tft_data <- fromJSON(url)

# -----------------------------------------------------------------------------#
# Get Data for Set X

get_set <- function(set = NA) {

  # Get all available set numbers
  set_numbers <- tft_data$setData$number %>% 
    as.integer() %>% 
    unique() %>%
    sort()
  
  if (is.na(set)) {
    set_data <- tft_data$setData[tft_data$setData$number == max(set_numbers),][1,]
  } else {
    # Check if set number is available
    if (!set %in% set_numbers) {
      stop(paste("Set", set, "not available. Available sets:", 
                 paste(set_numbers, collapse = ", ")))
    }
    set_data <- tft_data$setData[tft_data$setData$number == set,][1,]
  }
  
  return(set_data)
  
}

# -----------------------------------------------------------------------------#
# Unit and Trait Info for Set X

get_set_data <- memoise(function(set = NA) {
  
  set_data <- get_set(set)

  # Create the units table
  units_table <- data.frame(
    unit_id = set_data$champions[[1]]$apiName,
    unit_name = set_data$champions[[1]]$name,
    cost = set_data$champions[[1]]$cost,
    traits = sapply(set_data$champions[[1]]$traits, 
                    function(x) paste(x, collapse = ", ")),
    stringsAsFactors = FALSE
  ) %>%
    filter(nchar(traits) > 0)  # Remove units with no traits
  
  # Create the traits table
  traits_table <- data.frame(
    trait_id = set_data$traits[[1]]$apiName,
    trait_name = set_data$traits[[1]]$name,
    stringsAsFactors = FALSE
  )
  
  breakpoints <- lapply(set_data$traits[[1]]$effects, function(eff) {
    if (!is.null(eff)) {
      units_needed = eff$minUnits
    }
  })
  
  traits_table <- traits_table %>%
    mutate(
      breakpoints = map(breakpoints, function(bp) {
        if (is.null(bp)) NA else as.character(bp)
      })
    ) %>%
    arrange(trait_name)
  
  # Get craftable emblems
  craftable_emblems <- get_craftable_emblems(set_data) %>%
    mutate(
      lookup_trait_id = sub("^(.*)_Item_([A-Za-z]+)EmblemItem$", "\\1_\\2", item)
    ) %>%
    left_join(
      traits_table %>% select(trait_id, trait_name), 
      by = c("lookup_trait_id" = "trait_id")
    ) %>%
    left_join(
      traits_table %>% select(trait_name) %>% mutate(trait_id = paste0("TFT13_", gsub(" ", "", trait_name))), 
      by = c("lookup_trait_id" = "trait_id")
    ) %>%
    mutate(trait_name = coalesce(trait_name.x, trait_name.y)) %>%
    select(
      items = item,
      trait_id = lookup_trait_id,
      trait_name
    ) %>%
    filter(!is.na(trait_name)) %>%
    arrange(trait_name)
  
  return(list(units = units_table, traits = traits_table, craftable = craftable_emblems))
  
})

# -----------------------------------------------------------------------------#
# Get craftable emblems for Set X

get_craftable_emblems <- function(set_data) {

  # Extract all items and filter for emblems
  all_items <- set_data$items[[1]]
  
  all_items <- enframe(all_items, name = "id", value = "item") %>% 
    unnest_wider(item, names_sep = " ")

  # Identify craftable emblems
  craftable_emblems <- all_items %>% 
    filter(
      grepl("Emblem", `item 1`)
    ) %>%
    mutate(item = `item 1`, .keep = "none")
  
  return(craftable_emblems)
}

# -----------------------------------------------------------------------------#
