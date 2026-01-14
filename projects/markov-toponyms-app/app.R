# --------- Config ----------
library(shiny)
library(dplyr)
library(stringr)
library(readr)

# Models are in: markov_toponyms-app/models
# When you runApp("markov_toponyms-app"), getwd() is typically that folder.
MODEL_DIR <- normalizePath("models", mustWork = FALSE)

# Expected model filename pattern
MODEL_PATTERN <- "^markov_model_([A-Z]{2})\\.csv$"

# Friendly names (extend as you like)
COUNTRY_LABELS <- c(
  GB = "United Kingdom (GB)",
  IT = "Italy (IT)",
  DE = "Germany (DE)",
  FR = "France (FR)",
  PT = "Portugal (PT)",
  GR = "Greece (GR)",
  NL = "Netherlands (NL)",
  SE = "Sweden (SE)",
  RO = "Romania (RO)",
  UA = "Ukraine (UA)",
  LU = "Luxembourg (LU)"
)

# Markov order used when training
MARKOV_N <- 4

# Fixed name length defaults
MIN_LEN <- 5
MAX_LEN <- 8

# --------- Model discovery ----------
discover_models <- function(model_dir = MODEL_DIR) {
  if (!dir.exists(model_dir)) return(character())
  files <- list.files(model_dir, pattern = MODEL_PATTERN, full.names = FALSE)
  if (length(files) == 0) return(character())
  codes <- stringr::str_match(files, MODEL_PATTERN)[, 2]
  stats::na.omit(codes)
}

available_codes <- discover_models()
if (length(available_codes) == 0) {
  stop(
    "No models found in ", MODEL_DIR,
    ". Expected files like markov_model_GB.csv"
  )
}

choices_named <- setNames(
  available_codes,
  ifelse(
    available_codes %in% names(COUNTRY_LABELS),
    COUNTRY_LABELS[available_codes],
    paste0("Country (", available_codes, ")")
  )
)

# --------- Generation functions ----------
generate_name <- function(model, n = MARKOV_N, max_len = MAX_LEN) {
  start_token <- "^"
  end_token   <- "$"
  pad_len     <- n - 1
  
  prefix <- str_dup(start_token, pad_len)
  result_chars <- character()
  
  repeat {
    rows <- model[model$prefix == prefix, ]
    if (nrow(rows) == 0) break
    
    next_ch <- sample(rows$next_char, size = 1, prob = rows$prob)
    
    if (next_ch == end_token || length(result_chars) >= max_len) break
    
    result_chars <- c(result_chars, next_ch)
    prefix <- paste0(substr(prefix, 2, nchar(prefix)), next_ch)
  }
  
  name <- paste0(result_chars, collapse = "")
  str_to_title(name)
}

generate_new_name <- function(model,
                              n = MARKOV_N,
                              min_len = MIN_LEN,
                              max_len = MAX_LEN,
                              max_tries = 100) {
  for (i in seq_len(max_tries)) {
    nm <- generate_name(model, n = n, max_len = max_len)
    if (nchar(nm) < min_len) next
    return(nm)
  }
  NA_character_
}

generate_many <- function(model, n_out = 20,
                          min_len = MIN_LEN,
                          max_len = MAX_LEN) {
  out <- character()
  tries <- 0
  max_total_tries <- n_out * 20
  
  while (length(out) < n_out && tries < max_total_tries) {
    tries <- tries + 1
    nm <- generate_new_name(
      model,
      min_len = min_len,
      max_len = max_len
    )
    if (!is.na(nm) && !(nm %in% out)) out <- c(out, nm)
  }
  
  out <- out[seq_len(min(length(out), n_out))]
  out
}

# --------- Loaders + caching ----------
model_path <- function(code) file.path(MODEL_DIR, paste0("markov_model_", code, ".csv"))

read_model <- function(code) {
  p <- model_path(code)
  readr::read_csv(p, show_col_types = FALSE) |>
    mutate(
      prefix = as.character(prefix),
      next_char = as.character(next_char),
      prob = as.numeric(prob)
    )
}

# --------- UI ----------
ui <- fluidPage(
  titlePanel("Markov Toponym Generator"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("country", "Choose a country model:", choices = choices_named),
      numericInput("n_out", "How many toponyms?", value = 15, min = 1, max = 500, step = 1),
      
      actionButton("go", "Generate", class = "btn-primary"),
      br(), br(),
      
      tags$em("If it gets strange, relax—the model is being artsy."),
      br(), br(),
      
      downloadButton("download_csv", "Download CSV")
    ),
    
    mainPanel(
      tableOutput("results")
    )
  )
)

# --------- Server ----------
server <- function(input, output, session) {
  
  cache <- reactiveValues(models = list())
  
  get_cached_model <- function(code) {
    if (!is.null(cache$models[[code]])) return(cache$models[[code]])
    m <- read_model(code)
    cache$models[[code]] <- m
    m
  }
  
  generated <- eventReactive(input$go, {
    code <- input$country
    model <- get_cached_model(code)
    
    vals <- generate_many(
      model,
      n_out = input$n_out
    )
    
    tibble(
      country = code,
      toponym = vals
    )
  }, ignoreInit = TRUE)
  
  output$results <- renderTable({
    req(generated())
    generated()
  })
  
  output$download_csv <- downloadHandler(
    filename = function() {
      paste0("toponyms_", input$country, "_", Sys.Date(), ".csv")
    },
    content = function(file) {
      req(generated())
      readr::write_csv(generated(), file)
    }
  )
}

shinyApp(ui, server)
