
<!-- README.md is generated from README.Rmd. Please edit that file -->

# AskOllama

<!-- badges: start -->

![R](https://img.shields.io/badge/Made%20with-R-blue?style=flat-square&logo=R)
![Ollama](https://img.shields.io/badge/Powered%20by-Ollama-ff69b4?style=flat-square)
![License:
MIT](https://img.shields.io/badge/License-MIT-green.svg?style=flat-square)
![GitHub
stars](https://img.shields.io/github/stars/ertamaki/AskOllama.svg?style=social)

<!-- badges: end -->

## Authors

- **Eduardo Tamaki** (<eduardo@tamaki.ai>)
- **Levente Littvay** (<levi@littvay.com>)

**AskOllama** provides a streamlined R interface to interact with
Ollama’s Chat Completion API. It simplifies querying models hosted on
our dedicated server (*Monster*) and supports single-turn, few-shot, and
multi-turn conversational prompts, as well as optional web-based context
enrichment, allowing users to finely tune conversational AI tasks
directly from R.

## Installation

You can install the development version of AskOllama from GitHub using:

``` r
# install.packages("devtools")
devtools::install_github("ertamaki/AskOllama")
```

Make sure to replace `yourusername` with your actual GitHub username.

## Usage

### Checking Available Models

To see which models are available on the Monster server, use:

``` r
library(AskOllama)

list_models()
```

Note that available models can change as new models are added or old
ones removed. We encourage regularly checking via list_models().

### Single-turn conversation

Here’s a simple example of how to query the Ollama API using AskOllama:

``` r
library(AskOllama)

response <- ask_ollama(
  system_content = "You are a helpful assistant.",
  user_content = "Tell me a short story",
  model = "mistral-large:123b-instruct-2411-q8_0"
)

cat(response)
```

### Multi-turn conversations

AskOllama also supports few-shot or multi-turn prompts:

``` r
conversation <- list(
  list(role = "system", content = "You are a helpful assistant."),
  list(role = "user", content = "What is the capital of France?"),
  list(role = "assistant", content = "The capital of France is Paris."),
  list(role = "user", content = "What about Germany?")
)

response <- ask_ollama(
  messages = conversation,
  model = "mistral-large:123b-instruct-2411-q8_0"
)

cat(response)
```

### Advanced Parameters

#### Temperature, top_p, and top_k

You can adjust parameters such as `temperature`, `top_p`, and `top_k` to
control randomness and sampling behavior:

- **Higher temperature** will make outputs more random and diverse.
- **Lower top-p values** reduce diversity and focus on more probable
  tokens.
- **Lower top-k** also concentrates sampling on the highest probability
  tokens for each step.

Thus, `temperature` increases variety, while `top_p` and `top_k` reduce
variety and focus samples on the model’s top predictions. You must
balance diversity and relevance when tuning these parameters for
different applications. Some models recommend only altering either
`temperature` or `top_p` from their defaults. Different models might
have different optimal values for these parameters.

``` r
response <- ask_ollama(
  user_content = "Suggest a creative name for a new coffee brand.",
  temperature = 0.9,
  top_p = 0.95,
  top_k = 50
)

cat(response)
```

#### stop

Use the `stop` parameter to indicate strings that should halt the
model’s output:

``` r
response <- ask_ollama(
  user_content = "List some famous scientists:\n1.",
  stop = c("\n5.") # Stops before printing "5."
)

cat(response)
```

#### Setting a Seed

You can set a random seed for reproducibility. However, please note that
the `seed` parameter may behave inconsistently with certain models:

``` r
response <- ask_ollama(
  user_content = "List some famous scientists:\n1.",
  seed = 12345
)

cat(response)
```

#### Token Limit Control

You can specify how many tokens you want the model to generate with
`num_predict`. However, due to occasional inconsistencies in enforcing
token limits, we introduced `strict_token_limit` which ensures strict
adherence by using streaming (`stream = TRUE`) and interrupting
generation once the token count is reached:

``` r
response <- ask_ollama(
  user_content = "Explain the concept of photosynthesis briefly.",
  num_predict = 50,
  strict_token_limit = TRUE
)

print(response)
```

### Web-enhanced queries

AskOllama can optionally fetch and include web content in your prompts:

``` r
response <- ask_ollama(
  user_content = "Summarize the key points of this webpage.",
  model = "mistral-large:123b-instruct-2411-q8_0",
  web = TRUE,
  web_url = "https://en.wikipedia.org/wiki/Artificial_intelligence"
)

cat(response)
```

## Error Handling & Logging

**AskOllama** provides robust error handling and logging mechanisms to
help diagnose issues:

- **Common Errors**:

  - `MODEL_NOT_FOUND`: Occurs when an invalid model name is provided.
    Use `list_models()` to see available models.
  - `SERVER_ERROR`: Indicates a failure to connect to the Ollama server.
    Verify the server is running, you’re connected via ZeroTier, and
    that nothing blocks this connection.
  - **Parameter Validation Errors**: These errors occur when numeric
    parameters fall outside acceptable ranges.

- **Logging**:

  - **Log Location**: Logs are stored by default in
    `~/.askai/logs/askai.log`. You can quickly access recent logs within
    R:

    ``` r
    logs <- readLines("~/.askai/logs/askai.log")

    print(tail(logs, 5))
    ```

  - **Debug Mode**: Enable or disable debugging easily:

    ``` r
    # Enable detailed debug logging
    .askai_env$DEBUG <- TRUE
    log_threshold(DEBUG)

    # Your query here...

    # Disable debug logging after debugging
    .askai_env$DEBUG <- FALSE
    log_threshold(INFO)
    ```

  - **Custom Log Layout:** Logs include the username and timestamp for
    easier troubleshooting.

## Contributing

Contributions are welcome! Please submit issues or pull requests through
GitHub.

## License

This project is licensed under the MIT License. See the
[LICENSE](LICENSE) file for details.
