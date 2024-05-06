---
# Feel free to add content and custom Front Matter to this file.
# To modify the layout, see https://jekyllrb.com/docs/themes/#overriding-theme-defaults

layout: home
---
# Define the function to extract price table
extract_price_table <- function(selector) {

  url <- "https://openai.com/pricing"
  webpage <- rvest::read_html(url)

  table_list <- webpage |> rvest::html_nodes(selector) |> rvest::html_table()

  table <- table_list[[1]]
  colnames(table) <- table[1, ]
  table <- table[-1, ]

  token_per_dollars_in <- dplyr::if_else(
    stringr::str_detect(table$Input, "M tok"), 
    1e6, 
    NA_real_
  )
  
  token_per_dollars_out <- dplyr::if_else(
    stringr::str_detect(table$Output, "M tok"), 
    1e6, 
    NA_real_
  )

  price_per_token_in <- as.numeric(
    stringr::str_extract(table$Input, "\\d+\\.\\d+")
  ) / token_per_dollars_in
  
  price_per_token_out <- as.numeric(
    stringr::str_extract(table$Output, "\\d+\\.\\d+")
  ) / token_per_dollars_out

  tibble::tibble(
    model = table$Model, 
    input = price_per_token_in, 
    output = price_per_token_out
  )
}

# Define the function to get price table
price_table <- function() {

  selectors <- c(
    "#\33 XFAaez1CV0JBJZ3NiDOIT > div > div:nth-child(2)",
    "#\32 tRwpgdShF89S1pkT6BcF7 > div > div:nth-child(2)",
    "#\37 yiqzOlcsASQPmt9ZLTPmu > div > div:nth-child(2)",
    "#\35 57kU5n8Lk4Z68C3SZgoXy > div > div:nth-child(2)",
    "#\37 clpBPSObqMEpfZfXRsUDT > div > div:nth-child(2)",
    "#\33 J4UPBmZal9ZbPbmp8qGTX > div > div:nth-child(2)",
    "#\35 QyVtGC5kdsIQn3mq3AOTU > div > div:nth-child(2)",
    "#\35 emhrWxawQTAowc12znS31 > div > div:nth-child(2)",
    "#\35 Jq5vTfdg6COsq2f3bAuOt > div > div:nth-child(2)",
    "#\37 segjff4Fx1MwUSYdf9Q7K > div > div:nth-child(2)",
    "#\36 TtzyaV4InyuYZHQEQipBx > div > div:nth-child(2)"
  )

  price_tab <- purrr::map(
    selectors, 
    ~ extract_price_table(.x)
  ) |> purrr::list_rbind()
  
  price_tab
}

# Define the function to get price input
price_input <- function(x) {
  models_and_prices_040424 <- price_table()
  models_and_prices_040424 |> dplyr::filter(model == x) |> dplyr::pull(input)
}

# Define the function to get price output
price_output <- function(x) {
  models_and_prices_040424 <- price_table()
  models_and_prices_040424 |> dplyr::filter(model == x) |> dplyr::pull(output)
}

# Define the function to get available models
models_func <- function() {
  models_and_prices_040424 <- price_table()
  models_and_prices_040424$model
}

current_models <- models_func()
