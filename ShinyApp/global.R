library(tidyverse)
library(jsonlite)
library(memoise)

# TODO: improve BFL alg

url <- "https://raw.communitydragon.org/latest/cdragon/tft/en_us.json"
image_url <- "https://raw.communitydragon.org/pbe/game/assets/ux/tft/championsplashes/patching/"
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
    pic = paste0(image_url,
                 tolower(set_data$champions[[1]]$apiName),
                         "_teamplanner_splash.png"),
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
      units_needed = as.numeric(eff$minUnits)
    }
  })
  
  traits_table <- traits_table %>%
    mutate(
      breakpoints = map(breakpoints, function(bp) {
        if (is.null(bp)) NA else bp
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
# Generate Team Compositions

# Helper functions
`%||%` <- function(a, b) if (!is.null(a)) a else b

precompute_trait_thresholds <- function(traits_df) {
  traits_df %>%
    select(trait_name, breakpoints) %>%
    mutate(breakpoints = map(breakpoints, ~ as.numeric(.x))) %>%
    {set_names(.$breakpoints, .$trait_name)} # Using trait_name as names
}

preprocess_units <- function(units_df) {
  units_df %>%
    mutate(
      traits = strsplit(traits, ", "),
      unit_id = row_number()
    ) %>%
    select(unit_id, unit_name, cost, traits)
}

# Improved trait counting that properly checks thresholds
count_activated_traits <- function(units, emblems, units_df, trait_thresholds) {
  # Combine unit traits and emblems
  all_traits <- unique(c(emblems, unlist(units_df$traits[units_df$unit_name %in% units])))
  
  # Count each trait
  counts <- setNames(rep(0, length(all_traits)), all_traits)
  counts[emblems] <- table(emblems)
  
  for (unit in units) {
    unit_traits <- units_df$traits[[which(units_df$unit_name == unit)]]
    for (trait in unit_traits) {
      counts[trait] <- counts[trait] + 1
    }
  }
  
  # Check which traits meet their thresholds
  activated <- map_lgl(names(counts), ~ {
    thresh <- trait_thresholds[[.x]] %||% numeric(0)
    any(counts[.x] >= thresh)
  })
  
  # Return only activated traits with their counts
  counts[activated]
}

# Helper function to identify unit-specific traits
get_unit_specific_traits <- function(units_df) {
  # First split all trait combinations into individual traits
  all_traits <- units_df %>%
    select(unit_name, traits) %>%
    mutate(
      # Split combined traits into individual ones
      traits_split = map(traits, ~ unlist(strsplit(., ", ")))
    )
      
  # Count how many units have each individual trait
  trait_counts <- all_traits %>%
    select(unit_name, traits = traits_split) %>%
    unnest(traits) %>%
    count(traits, name = "unit_count") %>%
    filter(unit_count == 1) %>%
    pull(traits)
  
  return(trait_counts)
}

# Optimized horizontal composition with correct trait activation
find_horizontal_comp_correct <- function(emblems, units_df, trait_thresholds, spec_traits, team_size = 8) {
  # Precompute trait weights (prioritize higher-breakpoint traits)
  trait_weights <- map_dbl(names(trait_thresholds), ~ {
    max_units <- max(trait_thresholds[[.x]] %||% 0)
    # Count how many units have this trait
    units_with_trait <- sum(map_lgl(units_df$traits, function(unit_traits) .x %in% unit_traits))
    max_units * units_with_trait  # Weight by both breakpoint and availability
  }) %>% set_names(names(trait_thresholds))

  best_team <- NULL
  best_score <- -Inf
  best_traits <- list()
  
  # Try multiple starting points based on different criteria
  starting_units <- list(
    # Highest cost units first
    units_df %>% arrange(desc(cost)) %>% pull(unit_name),
    # Units with most traits
    units_df %>% 
      mutate(trait_count = map_int(traits, length)) %>% 
      arrange(desc(trait_count)) %>% pull(unit_name),
    # Units that activate the most valuable traits
    units_df %>% 
      mutate(score = map_dbl(traits, ~ sum(trait_weights[.x]))) %>% 
      arrange(desc(score)) %>% pull(unit_name)
  )
  
  # Try each starting strategy
  for (start_seq in starting_units) {
    current_team <- character(0)
    remaining_units <- start_seq
    
    while (length(current_team) < team_size && length(remaining_units) > 0) {
      # Score all remaining units
      scores <- map_dbl(remaining_units, ~ {
        potential_team <- c(current_team, .x)
        traits <- count_activated_traits(potential_team, emblems, units_df, trait_thresholds)
        
        # Score based on activated traits and their weights
        sum(trait_weights[names(traits) %||% character(0)]) - 
          sum(names(traits) %in% spec_traits) * 0.5  # Penalize unit-specific traits
      })
      
      # Select best unit
      best_idx <- which.max(scores)
      if (length(best_idx) == 0) break
      
      current_team <- c(current_team, remaining_units[best_idx])
      remaining_units <- remaining_units[-best_idx]
    }
    
    # Evaluate final team
    current_traits <- count_activated_traits(current_team, emblems, units_df, trait_thresholds)
    current_score <- sum(trait_weights[names(current_traits) %||% character(0)])
    
    if (current_score > best_score) {
      best_team <- current_team
      best_score <- current_score
      best_traits <- current_traits
    }
  }
  
  # Fill any remaining slots with highest value units
  if (length(best_team) < team_size) {
    remaining <- units_df %>% 
      filter(!unit_name %in% best_team) %>%
      mutate(score = map_dbl(traits, ~ sum(trait_weights[.x %||% character(0)]))) %>%
               arrange(desc(score), desc(cost)) %>%
               pull(unit_name)
             
             if (length(remaining) > 0) {
               best_team <- c(best_team, remaining[1:min(length(remaining), team_size - length(best_team))])
               best_traits <- count_activated_traits(best_team, emblems, units_df, trait_thresholds)
             }
  }
  
  list(
    team = best_team,
    activated_traits = best_traits,
    num_activated = length(best_traits)
  )
}

# Optimized vertical composition with correct trait activation
find_vertical_comp_correct <- function(emblems, units_df, trait_thresholds, spec_traits, team_size = 8, forced_trait = NULL) {
  # Initialize
  selected_units <- character(0)
  current_counts <- table(emblems)
  all_traits <- unique(c(names(current_counts), unlist(units_df$traits)))
  current_counts <- setNames(rep(0, length(all_traits)), all_traits)
  current_counts[names(table(emblems))] <- table(emblems)
  
  # Determine primary trait
  if (is.null(forced_trait)) {
    primary_trait <- names(which.max(map_dbl(all_traits, ~ max(trait_thresholds[[.x]] %||% 0))))
  } else {
    primary_trait <- forced_trait
  }
  
  primary_thresholds <- trait_thresholds[[primary_trait]] %||% numeric(0)
  
  # First pass: maximize primary trait with highest cost units first
  primary_units <- units_df %>%
    filter(map_lgl(traits, ~ primary_trait %in% .x)) %>%
    arrange(desc(cost)) %>%
    pull(unit_name)
  
  for (unit in primary_units) {
    if (length(selected_units) >= team_size) break
    
    current_count <- current_counts[primary_trait]
    next_threshold <- primary_thresholds[primary_thresholds > current_count][1]
    
    if (is.na(next_threshold)) break
    
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
          unit_traits <- traits[[which(unit_name == .x)]]
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
  activated_traits <- count_activated_traits(selected_units, emblems, units_df, trait_thresholds)
  
  list(
    team = selected_units,
    activated_traits = activated_traits,
    num_activated = length(activated_traits),
    primary_trait = primary_trait
  )
}

# Wrapper function
generate_teams_correct <- function(emblems, set_data, strategy = "horizontal", team_size = 8, forced_trait = NULL) {
  # Preprocess data
  units_df <- preprocess_units(set_data$units)
  trait_thresholds <- precompute_trait_thresholds(set_data$traits)
  spec_traits <- get_unit_specific_traits(set_data$units)
  
  if (strategy == "horizontal") {
    find_horizontal_comp_correct(emblems, units_df, trait_thresholds, spec_traits, team_size)
  } else if (strategy == "vertical") {
    find_vertical_comp_correct(emblems, units_df, trait_thresholds, spec_traits, team_size, forced_trait)
  } else {
    stop("Invalid strategy. Choose 'horizontal' or 'vertical'")
  }
}

# -----------------------------------------------------------------------------#
# Format Champion Images

display_team_with_images <- function(team, set_data) {
  # Get image URLs for each champion and sort by cost
  team_df <- set_data$units %>% 
    filter(unit_name %in% team) %>%
    select(unit_name, pic, cost, traits) %>%
    arrange(cost)  # Sort by cost low to high
  
  # Create HTML for each champion with image
  champion_tags <- lapply(1:nrow(team_df), function(i) {
    cost <- team_df$cost[i]
    cost_class <- ifelse(cost %in% 1:5, paste0("cost-", cost), "cost-1")
    
    # Format traits as badges
    trait_badges <- lapply(strsplit(team_df$traits[i], ", ")[[1]], function(trait) {
      tags$span(class = "trait-badge", trait)
    })
    
    tags$div(
      class = "champion-container",
      tags$div(
        class = paste("champion-card", cost_class),
        style = paste0("--cost-color: ", get_cost_color(cost), ";"),
        # Trait tooltip
        tags$div(
          class = "trait-tooltip",
          do.call(tags$div, trait_badges)
        ),
        tags$img(
          src = team_df$pic[i],
          class = "champion-img",
          alt = team_df$unit_name[i]
        ),
        tags$div(
          class = "champion-name",
          team_df$unit_name[i]
        ),
        tags$div(
          class = "champion-cost",
          cost
        )
      )
    )
  })
  
  tags$div(class = "team-display", champion_tags)
}