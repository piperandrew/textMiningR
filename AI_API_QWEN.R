# ============================================================#
# Qwen (DashScope) API Batch Processing Script                #
# Reads a CSV with a "text" column, sends each row to the API,#
# and stores the response in a new "result" column.           #
# ============================================================#

library(httr)
library(jsonlite)

# --- 1. Get API key dynamically (prompts you at runtime) ---
api_key <- readline(prompt = "Enter your DashScope API key: ")

# --- 2. Set your parameters ---
model       <- "qwen3-14b"
temperature <- 0.7
prompt      <- "Summarize the following text in one sentence:"

# --- 3. Read input CSV ---
input_file  <- "input.csv"          # change as needed
output_file <- "output.csv"         # change as needed

df <- read.csv(input_file, stringsAsFactors = FALSE)

if (!"text" %in% colnames(df)) {
  stop("CSV must contain a column named 'text'.")
}

# --- 4. Define a function to call the API for one text ---
call_qwen <- function(text, api_key, model, temperature, prompt) {
  
  response <- POST(
    url = "https://dashscope-intl.aliyuncs.com/compatible-mode/v1/chat/completions",
    add_headers(
      "Authorization" = paste("Bearer", api_key),
      "Content-Type"  = "application/json"
    ),
    body = toJSON(list(
      model       = model,
      temperature = temperature,
      messages    = list(
        list(role = "system", content = prompt),
        list(role = "user",   content = text)
      )
    ), auto_unbox = TRUE),
    encode = "raw"
  )
  
  parsed <- content(response, as = "parsed", type = "application/json")
  
  if (!is.null(parsed$error)) {
    warning("API error: ", parsed$error$message)
    return(NA)
  }
  
  return(parsed$choices[[1]]$message$content)
}

# --- 5. Loop through each row and call the API ---
cat("Processing", nrow(df), "rows...\n")

df$result <- NA_character_

for (i in seq_len(nrow(df))) {
  cat(sprintf("  Row %d / %d\n", i, nrow(df)))
  
  df$result[i] <- call_qwen(
    text        = df$text[i],
    api_key     = api_key,
    model       = model,
    temperature = temperature,
    prompt      = prompt
  )
  
  Sys.sleep(0.5)
}

# --- 6. Save the result ---
write.csv(df, output_file, row.names = FALSE)
cat("Done. Output saved to:", output_file, "\n")