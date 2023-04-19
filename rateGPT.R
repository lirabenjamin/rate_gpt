rate_gpt <- function(input_text, prompt, openai_token, number = T) {
  require(httr)

  # Set up API call
  api_url <- "https://api.openai.com/v1/chat/completions"
  headers <- add_headers(
    "Content-Type" = "application/json",
    "Authorization" = paste("Bearer", openai_token)
  )
  
  # Prepare request data
  request_data <- list(
    "model" = "gpt-3.5-turbo-0301",
    "messages" = list(
      list(
        "role" = "system",
        "content" = prompt),
      list(
        "role" = "user",
        "content" = input_text
      )
      ),
    "temperature" = 0.7
    #"max_tokens" = 50, # You can change the number of tokens depending on your needs
    #"n" = 1,
    #"stop" = NULL,
  )
  
  # Send POST request
  response <- POST(api_url, headers, body = jsonlite::toJSON(request_data, auto_unbox = TRUE), encode = "json")
  
  # Check if the request was successful
  if (response$status_code == 200) {
    # Extract completion text
    response_content <- content(response, "parsed")
    completion_text <- response_content$choices[[1]]$message$content
    if (number) {completion_text = readr::parse_number(completion_text)}
    return(completion_text)
  } else {
    # Return error message
    return(paste("Error: HTTP status code", response$status_code))
  }
}

# Example (single text)
input_text <- "Througout my college career I had to work so hard to achieve what I've wanted. I never gave up and finally was able to graduate. It wasn't easy, but I truly believe that overcoming those obstacles has made me a stronger candidate."
prompt <- "Please rate the text on a scale from 0 to 100, where 0 means no perseverance, and 100 means high perseverance. You can use any number between 0 and 100, Just respond with the number, you don't need to give explanations."
openai_token <- "your_token_here"
openai_token <- read_lines("secret")

result <- chat_gpt(input_text, prompt, openai_token)
cat("GPT-3.5-turbo completion:", result)

# Example (dataframe)
enframe(input_text) |>  
  rename(id = name, text = value) |>  
  mutate(perseverance = map_dbl(text, ~chat_gpt(., prompt, openai_token) ))
