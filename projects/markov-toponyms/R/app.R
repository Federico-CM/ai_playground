library(shiny)
library(dplyr)
library(stringr)
library(readr)
library(rprojroot)

# --------- Config ----------
MODEL_DIR <- file.path("..", "outputs", "models")
#MODEL_DIR <- file.path("outputs", "models")
#MODEL_PATTERN <- "^markov_model_([A-Z]{2})\\.csv$"
PROJECT_ROOT <- rprojroot::find_root(rprojroot::is_rstudio_project)
MODEL_DIR <- file.path(PROJECT_ROOT, "outputs", "models")

# Friendly names (extend as you like)
COUNTRY_LABELS <- c(
  GB = "United Kingdom (GB)",
  IT = "Italy (IT)",
  ES = "Spain (ES)",
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

# Markov order used when training (your example uses n = 4)
MARKOV_N <- 4

# --------- Model discovery ----------
discover_models <- function(model_dir = MODEL_DIR) {
  if (!dir.exists(model_dir)) return(character())
  files <- list.files(model_dir, pattern = MODEL_PATTERN, full.names = FALSE)
  codes <- stringr::str_match(files, MODEL_PATTERN)[,2]
  stats::na.omit(codes)
}

available_codes <- discover_models()
if (length(available_codes) == 0) {
  stop(
    "No models found in ", MODEL_DIR, ". Expected files like markov_model_GB.csv"
  )
}

# Create select choices with labels where possible
choices_named <- setNames(
  available_codes,
  ifelse(
    available_codes %in% names(COUNTRY_LABELS),
    COUNTRY_LABELS[available_codes],
    paste0("Country (", available_codes, ")")
  )
)

# --------- Generation functions ----------
generate_name <- function(model, n = MARKOV_N, max_len = 15) {
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
                              names_set = NULL,
                              n = MARKOV_N,
                              min_len = 5,
                              max_len = 15,
                              max_tries = 100) {
  for (i in seq_len(max_tries)) {
    nm <- generate_name(model, n = n, max_len = max_len)
    if (nchar(nm) < min_len) next

    # If provided, avoid existing names
    if (!is.null(names_set) && nm %in% names_set) next

    return(nm)
  }
  NA_character_
}

generate_many <- function(model, n_out = 50, names_set = NULL,
                          min_len = 5, max_len = 15) {
  # generate a bit extra to compensate for NAs / duplicates
  out <- character()
  tries <- 0
  max_total_tries <- n_out * 50

  while (length(out) < n_out && tries < max_total_tries) {
    tries <- tries + 1
    nm <- generate_new_name(
      model,
      names_set = names_set,
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
names_set_path <- function(code) file.path(MODEL_DIR, paste0("names_set_", code, ".rds"))

read_model <- function(code) {
  p <- model_path(code)
  # CSV must have: prefix,next_char,count,prob  (count optional if you kept it)
  readr::read_csv(p, show_col_types = FALSE) |>
    mutate(
      prefix = as.character(prefix),
      next_char = as.character(next_char),
      prob = as.numeric(prob)
    )
}

read_names_set_optional <- function(code) {
  p <- names_set_path(code)
  if (!file.exists(p)) return(NULL)
  x <- readRDS(p)
  unique(as.character(x))
}

# --------- UI ----------
ui <- fluidPage(
  titlePanel("Markov Toponym Generator"),

  sidebarLayout(
    sidebarPanel(
      selectInput("country", "Choose a country model:", choices = choices_named),
      numericInput("n_out", "How many toponyms?", value = 50, min = 1, max = 500, step = 1),
      sliderInput("min_len", "Minimum length:", min = 3, max = 12, value = 5, step = 1),
      sliderInput("max_len", "Maximum length:", min = 8, max = 25, value = 15, step = 1),

      checkboxInput(
        "avoid_real",
        "Avoid existing/real names (requires names_set_XX.rds)",
        value = TRUE
      ),

      actionButton("go", "Generate", class = "btn-primary"),
      br(), br(),
      downloadButton("download_csv", "Download CSV")
    ),

    mainPanel(
      uiOutput("status"),
      tableOutput("results")
    )
  )
)

# --------- Server ----------
server <- function(input, output, session) {

  cache <- reactiveValues(models = list(), name_sets = list())

  get_cached_model <- function(code) {
    if (!is.null(cache$models[[code]])) return(cache$models[[code]])
    m <- read_model(code)
    cache$models[[code]] <- m
    m
  }

  get_cached_names_set <- function(code) {
    if (!is.null(cache$name_sets[[code]])) return(cache$name_sets[[code]])
    ns <- read_names_set_optional(code)
    cache$name_sets[[code]] <- ns
    ns
  }

  generated <- eventReactive(input$go, {
    code <- input$country
    model <- get_cached_model(code)

    names_set <- NULL
    if (isTRUE(input$avoid_real)) {
      names_set <- get_cached_names_set(code)
    }

    vals <- generate_many(
      model,
      n_out = input$n_out,
      names_set = names_set,
      min_len = input$min_len,
      max_len = input$max_len
    )

    tibble(
      country = code,
      toponym = vals
    )
  }, ignoreInit = TRUE)

  output$status <- renderUI({
    req(input$country)
    code <- input$country

    # only check file existence; don't force-load unless generating
    has_names_set <- file.exists(names_set_path(code))

    tagList(
      if (isTRUE(input$avoid_real) && !has_names_set) {
        div(
          style = "padding:10px; background:#fff3cd; border:1px solid #ffeeba; border-radius:6px;",
          strong("Note: "),
          "names_set_", code, ".rds not found. The app will still generate names, ",
          "but it can’t filter out real/existing names for this country until you add that file."
        )
      }
    )
  })

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
 
