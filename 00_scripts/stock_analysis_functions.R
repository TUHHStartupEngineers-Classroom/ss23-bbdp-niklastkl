get_stock_list <-
function(stock_index = "DAX") {
  
  # Control for upper and lower case
  index_lower <- str_to_lower(stock_index)
  # Control if user input is valid
  index_valid <- c("dax", "sp500", "dow", "nasdaq")
  if (!index_lower %in% index_valid) {stop(paste0("x must be a character string in the form of a valid exchange.",
                                                  " The following are valid options:\n",
                                                  stringr::str_c(str_to_upper(index_valid), collapse = ", ")))
  }
  
  # Control for different currencies and different column namings in wiki
  vars <- switch(index_lower,
                 dax    = list(wiki     = "DAX", 
                               columns  = c("Ticker", "Company")),  # changed here "Ticker symbol" to "Ticker"
                 sp500  = list(wiki     = "List_of_S%26P_500_companies", 
                               columns  = c("Symbol", "Security")),
                 dow    = list(wiki     = "Dow_Jones_Industrial_Average",
                               columns  = c("Symbol", "Company")),
                 nasdaq = list(wiki     = "NASDAQ-100",
                               columns  = c("Ticker", "Company"))
  )
  
  # Extract stock list depending on user input
  read_html(glue("https://en.wikipedia.org/wiki/{vars$wiki}")) %>% 
    
    # Extract table from wiki
    html_nodes(css = "#constituents") %>% 
    html_table() %>% 
    dplyr::first() %>% 
    as_tibble(.name_repair = "minimal") %>% 
    # Select desired columns (different for each article)
    dplyr::select(vars$columns) %>% 
    # Make naming identical
    set_names(c("symbol", "company")) %>% 
    
    # Clean (just relevant for DOW)
    mutate(symbol = str_remove(symbol, "NYSE\\:[[:space:]]")) %>% 
    
    # Sort
    arrange(symbol) %>%
    # Create the label for the dropdown list (Symbol + company name)
    mutate(label = str_c(symbol, company, sep = ", ")) %>%
    dplyr::select(label)
  
}
get_symbol_from_user_input <-
function(user_input){
  user_input %>% stringr::str_split(pattern=", ") %>% purrr::pluck(1, 1)
}
get_stock_data <-
function(stock_symbol, 
                           from = today() - days(180), 
                           to   = today(), 
                           mavg_short = 20, mavg_long = 50) {
  stock_symbol %>% quantmod::getSymbols(
  src         = "yahoo", 
  from        = from, 
  to          = to, 
  auto.assign = FALSE) %>% 
  
  # Convert to tibble
  timetk::tk_tbl(preserve_index = T, 
                 silent         = T) %>% 
  
  # Add currency column (based on symbol)
  mutate(currency = case_when(
    str_detect(names(.) %>% last(), ".DE") ~ "EUR",
    TRUE                                   ~ "USD")) %>% 
  
  # Modify tibble 
  set_names(c("date", "open", "high", "low", "close", "volume", "adjusted", "currency")) %>% 
  drop_na() %>%
  
  # Convert the date column to a date object (I suggest a lubridate function)
  dplyr::mutate(date = lubridate::ymd(date)) %>% 
  
  # Add the moving averages
  # name the columns mavg_short and mavg_long
  dplyr::mutate(mavg_short = rollmean(adjusted, mavg_short,  fill = NA, align = "right")) %>% 
  dplyr::mutate(mavg_long = rollmean(adjusted, mavg_long,  fill = NA, align = "right")) %>% 
  
  # Select the date and the adjusted column
  dplyr::select(date, adjusted, mavg_short, mavg_long, currency)
}
plot_stock_data <-
function(stock_data){
  g <- stock_data %>% 
  
  # convert to long format
  pivot_longer(cols = c(adjusted, mavg_short, mavg_long), 
               names_to    = "legend", 
               values_to    = "value", 
               names_ptypes = list(legend = factor())) %>% 
  
  # ggplot
  ggplot(aes(x=date, y = value, group = legend)) +
  geom_line(aes(linetype = legend, color=legend)) +
  scale_y_continuous(labels = stock_data %>% pull(currency) %>% first() %>% currency_format()) +
  labs(y = "Adjusted Share Price", x = "")
  ggplotly(g)
}
currency_format <-
function(currency) {
  if (currency == "USD") 
  { x <- scales::dollar_format(largest_with_cents = 10) }
  if (currency == "EUR")   
  { x <- scales::dollar_format(prefix = "", suffix = " €",
                               big.mark = ".", decimal.mark = ",",
                               largest_with_cents = 10)}
  return(x)
}
generate_commentary <-
function(data, user_input) {
  warning_signal <- data %>%
    tail(1) %>% # Get last value
    mutate(mavg_warning_flag = mavg_short < mavg_long) %>% # insert the logical expression
    pull(mavg_warning_flag)
  
  n_short <- data %>% pull(mavg_short) %>% is.na() %>% sum() + 1
  n_long  <- data %>% pull(mavg_long) %>% is.na() %>% sum() + 1
  
  if (warning_signal) {
    str_glue("In reviewing the stock prices of {user_input}, the {n_short}-day moving average is below the {n_long}-day moving average, indicating negative trends")
  } else {
    str_glue("In reviewing the stock prices of {user_input}, the {n_short}-day moving average is above the {n_long}-day moving average, indicating positive trends")
  }
}
