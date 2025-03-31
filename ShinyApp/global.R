library(tidyverse)
library(jsonlite)
library(memoise)

# TODO: improve BFL alg: add multiplke tries and fix 2nd tab

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
`%notin%` <- Negate(`%in%`)

precompute_trait_thresholds <- function(traits_df) {
  traits_df %>%
    select(trait_name, breakpoints) %>%
    mutate(
      breakpoints = map(breakpoints, ~ {
        if (is.null(.x) || length(.x) == 0) {
          numeric(0)  # Return empty numeric for traits with no breakpoints
        } else {
          as.numeric(.x)
        }
      })
    ) %>%
    {set_names(.$breakpoints, .$trait_name)}
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

# -----------------------------------------------------------------------------#

# Optimized horizontal composition with correct trait activation
find_horizontal_comp_correct <- function(emblems, units_df, trait_thresholds, 
                                         spec_traits, team_size = 8, 
                                         ga_iterations = 3) {
  # Preprocessing
  units_df <- units_df %>%
    mutate(
      traits = map(traits, ~ .x[.x != ""]),
      trait_count = map_int(traits, length)
    ) %>%
    filter(trait_count > 0)
  
  # Improved scoring focused purely on trait activation
  score_team <- function(team) {
    traits <- count_activated_traits(team, emblems, units_df, trait_thresholds)
    length(traits)  # Just count activated traits
  }
  
  # Generate initial population of teams
  generate_initial_teams <- function(n = 20) {
    map(1:n, ~ {
      # Sample different team sizes to encourage diversity
      sample_size <- min(max(1, rpois(1, team_size - 1) + 1), team_size)
      sample(units_df$unit_name, sample_size, replace = FALSE)
    })
  }
  
  # Genetic algorithm approach
  find_best_team <- function() {
    population <- generate_initial_teams()
    best_team <- NULL
    best_score <- -Inf
    
    for (i in 1:50) {  # Generations
      # Evaluate population
      scores <- map_dbl(population, score_team)
      
      # Track best team
      current_best_idx <- which.max(scores)
      if (scores[current_best_idx] > best_score) {
        best_score <- scores[current_best_idx]
        best_team <- population[[current_best_idx]]
      }
      
      # Selection (keep top 25%)
      keep <- order(-scores)[1:ceiling(length(population)*0.25)]
      elites <- population[keep]
      
      # Crossover and mutation
      new_population <- elites
      while (length(new_population) < length(population)) {
        # Select parents
        parents <- sample(elites, 2, prob = scores[keep]/sum(scores[keep]))
        
        # Crossover
        child <- unique(c(parents[[1]], parents[[2]]))
        child <- child[1:min(length(child), team_size)]
        
        # Mutation (20% chance)
        if (runif(1) < 0.2 && length(child) > 1) {
          to_remove <- sample(1:length(child), 1)
          to_add <- sample(setdiff(units_df$unit_name, child), 1)
          child <- c(child[-to_remove], to_add)
        }
        
        # Ensure team size
        if (length(child) < team_size) {
          child <- c(child, sample(setdiff(units_df$unit_name, child), 
                                   team_size - length(child)))
        }
        
        new_population <- c(new_population, list(child))
      }
      
      population <- new_population
    }
    
    list(
      team = best_team,
      activated_traits = count_activated_traits(best_team, emblems, units_df, trait_thresholds),
      num_activated = best_score
    )
  }
  
  ## Generate multiple results
  results <- map(1:ga_iterations, ~ {
    set.seed(.x)
    find_best_team()
  })
  
  # Find all teams with maximum activated traits
  max_activated <- max(map_dbl(results, ~ .x$num_activated))
  top_comps <- keep(results, ~ .x$num_activated == max_activated)
  
  # Remove duplicates (teams with same units regardless of order)
  unique_comps <- unique(top_comps)
  
  return(unique_comps)
}

# Helper function for team refinement
try_improve_team <- function(team, units_df, score_team, team_size, emblems, trait_thresholds) {
  current_score <- score_team(team)
  improved <- TRUE
  
  while (improved) {
    improved <- FALSE
    
    # Try replacing each unit with a better alternative
    for (i in seq_along(team)) {
      original_unit <- team[i]
      remaining_units <- setdiff(units_df$unit_name, team[-i])
      
      if (length(remaining_units) == 0) next
      
      # Find best replacement
      replacement_scores <- map_dbl(remaining_units, ~ {
        new_team <- c(team[-i], .x)
        score_team(new_team)
      })
      
      best_replacement <- remaining_units[which.max(replacement_scores)]
      new_score <- max(replacement_scores)
      
      if (new_score > current_score) {
        team[i] <- best_replacement
        current_score <- new_score
        improved <- TRUE
      }
    }
  }
  
  list(
    team = team,
    traits = count_activated_traits(team, emblems, units_df, trait_thresholds),
    score = current_score
  )
}

# -----------------------------------------------------------------------------#

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
generate_teams_correct <- function(emblems, set_data, strategy = "horizontal", 
                                   team_size = 8, forced_trait = NULL, 
                                   ga_iterations = 3) {
  # Preprocess data
  units_df <- preprocess_units(set_data$units)
  trait_thresholds <- precompute_trait_thresholds(set_data$traits)
  spec_traits <- get_unit_specific_traits(set_data$units)
  
  if (strategy == "horizontal") {
    find_horizontal_comp_correct(
      emblems = emblems,
      units_df = units_df,
      trait_thresholds = trait_thresholds,
      spec_traits = spec_traits,
      team_size = team_size,
      ga_iterations = ga_iterations
    )
  } else if (strategy == "vertical") {
    find_vertical_comp_correct(
      emblems = emblems,
      units_df = units_df,
      trait_thresholds = trait_thresholds,
      spec_traits = spec_traits,
      team_size = team_size,
      forced_trait = forced_trait
    )
  }
}

# -----------------------------------------------------------------------------#
# Format Champion Images

# Helper function for cost colors
get_cost_color <- function(cost) {
  case_when(
    cost == 1 ~ "#b0c3d9",
    cost == 2 ~ "#5a9e6e",
    cost == 3 ~ "#4a6ba8",
    cost == 4 ~ "#c44feb",
    cost == 5 ~ "#f0b040",
    TRUE ~ "#b0c3d9"  # Default to 1-cost color
  )
}

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
