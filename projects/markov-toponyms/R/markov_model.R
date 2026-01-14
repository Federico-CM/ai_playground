# A markov Model to generate toponyms
library(dplyr)
library(stringr)
library(readr)
library(purrr)

# Choose the country you want to generate toponyms of
# Curated data includes 12 countries, you can add more
# Among others, options include:
# UK       - GB
# Italy    - IT
# Germany  - DE
# France   - FR
# Portugal - PT

my_file <- paste("data/",i,".tsv",sep="")

loc_places <- read_tsv(
  my_file,
  col_types = cols(
    cc2 = col_character(),
    admin1_code = col_character(),
    admin3_code = col_character(),
    admin4_code = col_character()
  )
)

# Basic cleaning: lower-case, rm whitespaces, remove duplicates
names_clean <- loc_places |>
  pull(name) |>                                      # extract vector from tibble
  tolower() |>                                       # remove capitalization
  str_trim() |>                                      # remove trailing spaces
  na.omit() |>                                       # remove NAs
  str_replace_all("[^a-z' -]", "") |>                # strip weird chars
  keep(~ nchar(.x) >= 3) |>                          # remove very short words
  unique() |>                                        # remove duplicates
  discard(~ str_detect(.x, " |-"))                   # remove words with spaces or hyphens

# Check if I got enough data
length(names_clean)

# Train the Model ---------------------------------------------------------

build_markov_model <- function(names_vec, n = 3, report_every = 1000) {
  stopifnot(n >= 2)
  
  start_token <- "^"
  end_token   <- "$"
  pad_len     <- n - 1
  total       <- length(names_vec)
  
  # For each name, build a tibble of prefix / next_char pairs
  pieces <- lapply(seq_along(names_vec), function(i) {
    nm <- names_vec[i]
    
    if (i %% report_every == 0) {
      message("Processed ", i, " / ", total, " names (",
              round(100 * i / total, 1), "%)")
    }
    
    padded <- paste0(str_dup(start_token, pad_len), nm, end_token)
    chars  <- padded |> str_split("") |> unlist()
    
    idx <- seq_len(length(chars) - (n - 1))
    
    prefixes <- idx |>
      map_chr(\(j) paste(chars[j:(j + n - 2)], collapse = ""))
    
    next_chars <- idx |>
      map_chr(\(j) chars[j + n - 1])
    
    tibble(prefix = prefixes, next_char = next_chars)
  })
  
  df <- pieces |> bind_rows()
  
  model <- df |>
    count(prefix, next_char, name = "count") |>
    group_by(prefix) |>
    mutate(prob = count / sum(count)) |>
    ungroup()
  
  message("Model built successfully. Total prefixes: ", nrow(model))
  model
}

set.seed(123)
# Training the model might a couple mins
markov_model <- build_markov_model(names_clean, n = 4)

# Generate toponyms -------------------------------------------------------

generate_name <- function(model, n = 3, max_len = 15) {
  start_token <- "^"
  end_token   <- "$"
  pad_len     <- n - 1
  
  prefix <- str_dup(start_token, pad_len)
  result_chars <- character()
  
  # Sample letters tepending on what goes before 
  repeat {
    # all rows for this prefix
    rows <- model[model$prefix == prefix, ]
    if (nrow(rows) == 0) {
      # no data for this prefix, stop
      break
    }
    
    # sample next char according to probs
    next_ch <- sample(rows$next_char, size = 1, prob = rows$prob)
    
    if (next_ch == end_token || length(result_chars) >= max_len) {
      break
    }
    
    result_chars <- c(result_chars, next_ch)
    
    # update prefix: drop first char, append new one
    prefix <- paste0(substr(prefix, 2, nchar(prefix)), next_ch)
  }
  
  # collapse to string and title-case it
  name <- paste0(result_chars, collapse = "")
  str_to_title(name)
}


# Avoid existing names
names_set <- unique(str_to_title(names_clean))

generate_new_name <- function(model, names_set,
                              n = 4, 
                              min_len = 4, 
                              max_len = 15, 
                              max_tries = 50) {
  for (i in seq_len(max_tries)) {
    nm <- generate_name(model, n = n, max_len = max_len)
    
    # length check (count characters in the generated name)
    if (nchar(nm) < min_len) next
    
    # duplicate check
    if (!(nm %in% names_set)) {
      return(nm)
    }
  }
  # if nothing found
  NA_character_
}

# The following produces 50 toponyms
# Adjust as desired
replicate(
  n = 50,
  generate_new_name(
    markov_model,
    names_set,
    min_len = 5,
    max_len = 15),
  simplify = TRUE)
