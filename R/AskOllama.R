##     _        _     ___  _ _
##    / \   ___| | __/ _ \| | | __ _ _ __ ___   __ _
##   / _ \ / __| |/ / | | | | |/ _` | '_ ` _ \ / _` |
##  / ___ \\__ \   <| |_| | | | (_| | | | | | | (_| |
## /_/   \_\___/_|\_\\___/|_|_|\__,_|_| |_| |_|\__,_|
##
## Fearless (2.0.032025)

#' ASKAI: An R Interface to Ollama API
#'
#' @description
#' ASKAI (Fearless 2.0.032025) provides a streamlined interface to interact with Ollama models
#' through R. It offers one function with varying levels of parameter control for
#' different use cases.
#'
#' @author Eduardo Tamaki <eduardo@tamaki.ai>
#' @author Levente Littvay <levi@littvay.com>
#'
#' @details
#' Package: AskOllama
#' Type: Package
#' Version: 2.0.032025
#' Date: 2025-03-26
#' License: MIT

## PREAMBLE --------------------------------------------------------------------
#' @import logger
NULL

#' @name askOllama_preamble
#' @keywords internal
if (!require("pacman")) install.packages("pacman")

pacman::p_load(
  httr,       # Working with URLs and HTTP (1.4.7)
  jsonlite,   # JSON parser and generator (1.8.9)
  logger,     # Logging functionality
  pbapply,    # Progress bars
  crayon      # Colored terminal output
)

## GLOBAL CONFIGURATION -------------------------------------------------------
.askai_env <- new.env()
.askai_env$API_ENDPOINT <- "http://10.147.17.252:11434/api"
.askai_env$LOG_DIR <- "~/.askai/logs"
.askai_env$DEBUG <- FALSE

# Automatically detect the current system user
.askai_env$user <- Sys.info()[["user"]]
if (is.null(.askai_env$user) || .askai_env$user == "") {
  .askai_env$user <- Sys.getenv("USERNAME")
}

# Initialize logging: create log directory and reset log file if it exists
if (!dir.exists(.askai_env$LOG_DIR)) dir.create(.askai_env$LOG_DIR, recursive = TRUE)
log_file <- file.path(.askai_env$LOG_DIR, "askai.log")
if (file.exists(log_file)) file.remove(log_file)
log_appender(appender_file(log_file))

# Set logger threshold based on debug flag
if (.askai_env$DEBUG) {
  log_threshold(DEBUG)
} else {
  log_threshold(INFO)
}

# Define a custom log layout to include the username in every log entry
custom_layout <- function(level, msg, namespace, .logcall, .topcall, .timestamp = Sys.time(), ...) {
  sprintf("%s [%s] [User: %s] %s",
          toupper(level),
          format(.timestamp, "%Y-%m-%d %H:%M:%S"),
          .askai_env$user,
          msg)
}
log_layout(custom_layout)

## AVAILABLE MODELS ----------------------------------------------------------
.askai_models <- list(
  "qwq:32b-fp16" = list(
    size = "65 GB",
    description = "Medium-sized general purpose model"
  ),
  "mistral-small:24b-instruct-2501-fp16" = list(
    size = "47 GB",
    description = "Medium-sized Mistral instruction model"
  ),
  "deepseek-r1:70b-llama-distill-fp16" = list(
    size = "141 GB",
    description = "Large distilled LLaMA model"
  ),
  "deepseek-r1:32b-qwen-distill-fp16" = list(
    size = "65 GB",
    description = "Medium-sized distilled Qwen model"
  ),
  "deepseek-r1:70b-llama-distill-q8_0" = list(
    size = "74 GB",
    description = "Quantized large distilled LLaMA model"
  ),
  "phi4:14b-fp16" = list(
    size = "29 GB",
    description = "Compact general-purpose model"
  ),
  "llama3.3:70b-instruct-fp16" = list(
    size = "141 GB",
    description = "Large instruction-tuned LLaMA model"
  ),
  "mistral-large:123b-instruct-2411-q8_0" = list(
    size = "130 GB",
    description = "Large Mistral model with high accuracy"
  ),
  "falcon3:10b-instruct-fp16" = list(
    size = "20 GB",
    description = "Compact instruction-tuned model"
  ),
  "gemma2:27b-instruct-fp16" = list(
    size = "54 GB",
    description = "Medium-sized instruction-tuned Gemma model"
  ),
  "gemma2:9b-instruct-fp16" = list(
    size = "18 GB",
    description = "Compact instruction-tuned model"
  ),
  "gemma2:2b-instruct-fp16" = list(
    size = "5.2 GB",
    description = "Very compact instruction-tuned model"
  ),
  "mistral-nemo:12b-instruct-2407-fp16" = list(
    size = "24 GB",
    description = "Efficient Mistral model"
  ),
  "llama3.2-vision:11b-instruct-fp16" = list(
    size = "21 GB",
    description = "Vision-capable LLaMA model"
  ),
  "marco-o1:7b-fp16" = list(
    size = "15 GB",
    description = "Compact general-purpose model"
  ),
  "llama3.3:70b-instruct-q8_0" = list(
    size = "74 GB",
    description = "Large instruction-tuned LLaMA model"
  ),
  "dolphin-llama3:70b-v2.9-q8_0" = list(
    size = "74 GB",
    description = "Fine-tuned LLaMA model"
  ),
  "qwen2.5:32b-instruct-fp16" = list(
    size = "65 GB",
    description = "Medium-sized instruction model"
  ),
  "mistral-small:22b-instruct-2409-fp16" = list(
    size = "44 GB",
    description = "Small Mistral model with high accuracy"
  )
)

## ERROR HANDLING -----------------------------------------------------------
.askai_errors <- list(
  MODEL_NOT_FOUND = "Model '%s' not found. Use list_models() to see available models.",
  INVALID_TEMPERATURE = "Temperature must be between 0 and 1",
  INVALID_TOP_P = "Top-p must be between 0 and 1",
  INVALID_TOP_K = "Top-k must be positive",
  INVALID_MAX_TOKENS = "Max tokens must be positive",
  SERVER_ERROR = "Failed to connect to Ollama server at %s",
  INVALID_MIROSTAT = "Mirostat must be 0, 1, or 2"
)

#' Enhanced Error Handler
#' @keywords internal
handle_ollama_error <- function(error, model = NULL) {
  log_error(error$message)

  if (grepl("connection refused", error$message, ignore.case = TRUE)) {
    stop(sprintf(.askai_errors$SERVER_ERROR, .askai_env$API_ENDPOINT))
  }

  if (!is.null(model) && !validate_model(model)) {
    stop(sprintf(.askai_errors$MODEL_NOT_FOUND, model))
  }

  stop(error$message)
}

## HELPER FUNCTIONS ---------------------------------------------------------
#' List Available Ollama Models
#'
#' @return A data frame containing available models and their details
#' @export
list_models <- function() {
  models_df <- data.frame(
    Model = names(.askai_models),
    Size = sapply(.askai_models, function(x) x$size),
    Description = sapply(.askai_models, function(x) x$description),
    stringsAsFactors = FALSE
  )
  row.names(models_df) <- NULL
  return(models_df)
}

#' Validate Model Selection
#' @param model Character string of the model name
#' @return Logical indicating if model exists
#' @keywords internal
validate_model <- function(model) {
  valid <- model %in% names(.askai_models)
  if (!valid) log_warn(sprintf("Invalid model selected: %s", model))
  return(valid)
}

#' Validate Numeric Parameters
#' @param value Numeric value to validate
#' @param min Minimum allowed value
#' @param max Maximum allowed value
#' @param param_name Name of the parameter for error messages
#' @return Logical indicating if value is valid
#' @keywords internal
validate_numeric <- function(value, min, max, param_name) {
  if (!is.numeric(value) || value < min || value > max) {
    log_error(sprintf("Invalid %s: %s (should be between %s and %s)",
                      param_name, value, min, max))
    return(FALSE)
  }
  return(TRUE)
}

## PROGRESS BAR -------------------------------------------------------------
#' Show Progress Bar for API Calls
#' @param expr Expression to evaluate with progress bar
#' @keywords internal
with_progress <- function(expr) {
  message("\nSending request to Ollama...")
  result <- eval(expr)
  message("Request completed.")
  return(result)
}

#' Progress Bar Settings
#' @keywords internal
set_pb_style <- function() {
  style_pb <- progress::progress_bar$new(
    format = "[:bar] :percent eta: :eta",
    total = 100,
    clear = FALSE
  )
  return(style_pb)
}

#' Query Ollama's Chat Completion Endpoint
#'
#' This function provides a versatile interface to query Ollama's Chat Completion endpoint.
#' It supports single-turn, few-shot, and multi-turn conversational prompts and can incorporate
#' web-based research to enhance the prompt context. Users can customize various parameters to
#' control the response generation.
#'
#' @param system_content Character string containing system instructions
#' @param user_content Character string containing user message
#' @param model Character string specifying the Ollama model name
#' @param temperature Numeric between 0 and 1 controlling randomness
#' @param num_predict Integer specifying how many tokens to generate
#' @param top_p Numeric between 0 and 1 for nucleus sampling
#' @param top_k Integer for top-k sampling
#' @param repeat_penalty Numeric penalty for token repetition
#' @param presence_penalty Numeric penalty for token presence
#' @param frequency_penalty Numeric penalty for token frequency
#' @param stop Character vector of strings where the model should stop generating
#' @param stream Logical. If \code{TRUE}, responses are streamed back incrementally
#' @param seed Integer for reproducibility
#' @param strict_token_limit Logical. If \code{TRUE}, forces strict token limiting (uses streaming)
#' @param web Logical. If \code{TRUE}, enables web research.
#' @param web_url URL to fetch web content from.
#' @param web_content_limit Maximum number of characters to be fetched.
#' @param verbose Logical. If \code{TRUE}, shows detailed request/response information.
#'
#' @return Character string containing the model's response
#' @examples
#' \dontrun{
#'   # A simple query
#'   response <- ask_ollama(
#'     system_content = "You are a helpful assistant.",
#'     user_content = "Tell me a short story",
#'     model = "mistral-large:123b-instruct-2411-q8_0"
#'   )
#'   print(response)
#' }
#' @export
ask_ollama <- function(messages = NULL,
                       system_content = "You are a helpful assistant.",
                       user_content = NULL,
                       model = "mistral-large:123b-instruct-2411-q8_0",
                       temperature = 0.8,
                       num_predict = 500,
                       top_p = 0.9,
                       top_k = 40,
                       repeat_penalty = 1.1,
                       presence_penalty = 0,
                       frequency_penalty = 0,
                       stop = NULL,
                       stream = FALSE,
                       seed = NULL,
                       strict_token_limit = FALSE,
                       web = FALSE,
                       web_url = NULL,
                       web_content_limit = 20000,
                       verbose = FALSE) {  # Add verbose parameter with default FALSE

  ## Web research functionality (if enabled)
  if (web && !is.null(web_url)) {
    # Check if required package is available
    if (!require("rvest")) {
      message("Package 'rvest' is required for web functionality. Installing...")
      install.packages("rvest")
      if (!require("rvest")) {
        stop("Failed to install 'rvest' package. Web functionality unavailable.")
      }
    }

    message("Fetching web content from: ", web_url)

    tryCatch({
      # Fetch web content
      web_page <- rvest::read_html(web_url)

      # Extract text content from paragraphs - safely handle multiple elements
      body_text <- tryCatch({
        p_texts <- web_page %>%
          rvest::html_nodes("p") %>%
          rvest::html_text()
        paste(p_texts, collapse = "\n\n")
      }, error = function(e) {
        "Unable to extract paragraph content."
      })

      # Get page title - safely handle multiple title tags
      page_title <- tryCatch({
        title_nodes <- web_page %>% rvest::html_nodes("title")
        if (length(title_nodes) > 0) {
          rvest::html_text(title_nodes[1])  # Take just the first title
        } else {
          "Untitled Page"
        }
      }, error = function(e) {
        "Untitled Page"
      })

      message("Successfully fetched content from: ", page_title)

      # Prepare web context to add to user content
      web_context <- paste0(
        "Web content from URL: ", web_url, "\n",
        "Page title: ", page_title, "\n\n",
        "Content:\n", body_text
      )

      # Truncate if too long - using configurable limit
      if (nchar(web_context) > web_content_limit) {
        web_context <- paste0(
          substr(web_context, 1, web_content_limit),
          "\n\n[Content truncated due to length limitations]"
        )
        message(sprintf("Web content was truncated to %d characters.", web_content_limit))
      }

      # Modify user_content to include web context
      if (!is.null(user_content)) {
        user_content <- paste0(
          user_content,
          "\n\n--- WEB RESEARCH ---\n\n",
          web_context
        )
      } else if (!is.null(messages)) {
        # If using messages format, add web content to the last user message
        user_indices <- which(sapply(messages, function(x) x$role == "user"))
        if (length(user_indices) > 0) {
          last_user_message_index <- max(user_indices)
          messages[[last_user_message_index]]$content <- paste0(
            messages[[last_user_message_index]]$content,
            "\n\n--- WEB RESEARCH ---\n\n",
            web_context
          )
        } else {
          messages <- c(
            messages,
            list(list(
              role = "user",
              content = paste0("Please use this web research:\n\n", web_context)
            ))
          )
        }
      }

    }, error = function(e) {
      message("Error fetching web content: ", e$message)
      message("Continuing without web content...")
    })
  }

  log_info(sprintf("Querying model '%s' via /api/chat", model))

  # Validate model
  if (!validate_model(model)) {
    stop(sprintf(.askai_errors$MODEL_NOT_FOUND, model))
  }

  # Validate numeric parameters
  if (!validate_numeric(temperature, 0, 1, "temperature")) {
    stop(.askai_errors$INVALID_TEMPERATURE)
  }
  if (!validate_numeric(top_p, 0, 1, "top_p")) {
    stop(.askai_errors$INVALID_TOP_P)
  }

  # Allow manual message construction or simple two-turn construction
  if (is.null(messages)) {
    if (is.null(user_content)) {
      stop("You must provide either a 'messages' list or 'user_content'.")
    }
    messages <- list(
      list(role = "system", content = system_content),
      list(role = "user", content = user_content)
    )
  }

  # Use strict token limiting approach if requested
  if (strict_token_limit && num_predict < 100) {
    log_info("Using strict token limiting mode")

    # Always use streaming for strict token limiting
    body <- list(
      model             = model,
      messages          = messages,
      temperature       = temperature,
      max_tokens        = num_predict,
      top_p             = top_p,
      top_k             = top_k,
      repeat_penalty    = repeat_penalty,
      presence_penalty  = presence_penalty,
      frequency_penalty = frequency_penalty,
      stream            = TRUE  # Force streaming for token control
    )

    # Add optional parameters only if they're provided
    if (!is.null(stop)) body$stop <- stop
    if (!is.null(seed)) body$seed <- seed

    # Convert body to JSON
    body_json <- jsonlite::toJSON(body, auto_unbox = TRUE)
    if (verbose) print(body_json)  # Only print when verbose=TRUE
    log_debug(sprintf("Request body: %s", body_json))

    # Initialize variables for token counting
    collected_response <- ""
    token_count <- 0
    token_limit_reached <- FALSE

    # Initialize a progress bar for streaming tokens
    pb <- progress::progress_bar$new(
      format = "Tokens: [:bar] :percent ETA: :eta",
      total = num_predict,
      clear = FALSE
    )

    # Create a custom curl handle for more control
    handle <- curl::new_handle()
    curl::handle_setheaders(handle, "Content-Type" = "application/json")
    curl::handle_setopt(handle,
                        postfields = body_json,
                        post = TRUE)

    # Create connection object
    url <- paste0(.askai_env$API_ENDPOINT, "/chat")

    tryCatch({
      # Use low-level curl streaming with manual connection control
      conn <- curl::curl(url, handle = handle, open = "r")
      on.exit(try(close(conn), silent = TRUE))

      log_info("Starting controlled token streaming...")

      # Read line by line until token limit
      while (!token_limit_reached && length(line <- readLines(conn, n = 1)) > 0) {
        tryCatch({
          # Parse JSON response chunk
          parsed_chunk <- jsonlite::fromJSON(line)

          # Extract token - check both response formats
          token <- NULL
          if (!is.null(parsed_chunk$message) && !is.null(parsed_chunk$message$content)) {
            token <- parsed_chunk$message$content
          } else if (!is.null(parsed_chunk$response)) {
            token <- parsed_chunk$response
          }

          if (!is.null(token) && nchar(token) > 0) {
            # Add token to response
            collected_response <- paste0(collected_response, token)
            token_count <- token_count + 1
            pb$tick()  # Update progress bar for each token received

            # Check if we've reached the token limit
            if (token_count >= num_predict) {
              log_info(sprintf("Token limit reached (%d tokens)", num_predict))
              token_limit_reached <- TRUE
              break  # Exit the while loop
            }
          }
        }, error = function(e) {
          # Silently ignore JSON parsing errors (might be incomplete chunks)
        })
      }

      # Forcibly close connection
      close(conn)

      log_info(sprintf("Completed with %d tokens collected", token_count))
      return(collected_response)

    }, error = function(e) {
      log_error(sprintf("Error in streaming: %s", e$message))
      handle_ollama_error(e, model)
    })

  } else {
    # Original non-streaming code path for normal operation

    # Build regular request body
    body <- list(
      model             = model,
      messages          = messages,
      temperature       = temperature,
      num_predict       = num_predict,
      top_p             = top_p,
      top_k             = top_k,
      repeat_penalty    = repeat_penalty,
      presence_penalty  = presence_penalty,
      frequency_penalty = frequency_penalty,
      stream            = stream
    )

    # Add optional parameters only if they're provided
    if (!is.null(stop)) body$stop <- stop
    if (!is.null(seed)) body$seed <- seed

    # Convert body to JSON
    body_json <- jsonlite::toJSON(body, auto_unbox = TRUE)
    if (verbose) print(body_json)  # Only print when verbose=TRUE
    log_debug(sprintf("Request body: %s", body_json))

    tryCatch({
      log_info("Sending request to Ollama /api/chat endpoint...")

      # Only show progress bar when verbose=TRUE
      response <- if (verbose) {
        with_progress({
          httr::POST(
            url   = paste0(.askai_env$API_ENDPOINT, "/chat"),
            body  = body_json,
            encode = "json",
            httr::content_type("application/json")
          )
        })
      } else {
        # No progress bar in non-verbose mode
        httr::POST(
          url   = paste0(.askai_env$API_ENDPOINT, "/chat"),
          body  = body_json,
          encode = "json",
          httr::content_type("application/json")
        )
      }

      # Check for errors
      if (httr::http_error(response)) {
        log_error(sprintf("API request failed: %s", httr::http_status(response)$message))
        stop("API request failed: ", httr::http_status(response)$message)
      }

      # Parse JSON response
      content <- httr::content(response, "text", encoding = "UTF-8")
      parsed  <- jsonlite::fromJSON(content)

      # Log the parsed response for debugging
      log_debug(sprintf("Parsed JSON: %s", jsonlite::toJSON(parsed, auto_unbox = TRUE)))

      log_info("Successfully received response from Ollama API")

      # Check if 'response' exists and is non-empty
      if (!is.null(parsed$response) && nchar(parsed$response) > 0) {
        return(parsed$response)
      } else if (!is.null(parsed$message$content) && nchar(parsed$message$content) > 0) {
        return(parsed$message$content)
      } else {
        log_warn("Received empty response from model.")
        return(NA_character_)
      }

    }, error = function(e) {
      handle_ollama_error(e, model)
    })
  }
}


## IDEAS FOR FURTHER IMPROVEMENTS:
## 8. Streaming Response Handling:
##    - Allow users to pass a callback function to process tokens as they are received.
##    - Explore asynchronous processing (e.g., with the later package) for non-blocking updates.
##    - Implement retry logic for transient connection issues.

