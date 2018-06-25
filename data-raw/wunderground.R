# install.packages("tidyverse") # installs ~21 of the most useful packages you'll ever find
library(lubridate) # simplify date/time handling
# install.packages("rvest")
library(rvest)  # web-scraping framework
library(tidyverse) # loads core tidyverse: ggplot2, dplyr, tidyr, readr, purrr, tibble, stringr, forcats




urls <- seq.Date(as.Date("2004-08-16"), Sys.Date(), by = "1 day") %>% 
  as.character(format = "%Y/%m/%d") %>% 
  paste0("https://www.wunderground.com/history/airport/KBVN/",., "/DailyHistory.html")

urls_len <- length(urls)
counter <- 1
build_df <- safely(function(url){
  message(counter, " of ", urls_len)
  counter <<- counter + 1
  
  date <- url %>% 
    str_extract("\\d{4}/\\d{2}/\\d{2}") %>% 
    str_replace_all("/", "-") %>% 
    as.Date()
  
  url %>% 
    read_html() %>% 
    html_nodes("table") %>% 
    .[[5]] %>% 
    html_table() %>% 
    as_tibble() %>% 
    mutate(date = date, url = url)
})

start <- Sys.time()
####################################
raw_dfs <- urls %>%
  map(build_df)
####################################
end <- Sys.time()
end - start
write_rds(raw_dfs, "data/raw_dfs.rds")

# test_clean <- raw_dfs %>% 
  # map_df("result")

clean_names <- function(col_name){
  col_name %>% 
    str_replace_all(" ", "_") %>% 
    str_remove_all("\\.|\\(|\\)") %>% 
    str_to_lower()
}

raw_df <- raw_dfs %>% 
  map_df("result") %>% 
  select(date, everything()) %>% 
  rename_all(clean_names) %>% 
  mutate(time = case_when(
    !is.na(time_cst) ~ time_cst,
    !is.na(time_cdt) ~ time_cdt
    ) %>%
      str_pad(width = 8, side = "left", pad = "0")
    ) %>%
  mutate(am_pm = str_extract(time, "(A|P)M")) %>%
  mutate(minute = as.numeric(str_extract(time, "(?<=:)\\d{2}"))) %>%
  mutate(hour = case_when(
    am_pm == "AM" ~ as.numeric(str_extract(time, "\\d{2}(?=:)")),
    am_pm == "PM" ~ as.numeric(str_extract(time, "\\d{2}(?=:)")) + 12
    ) %>% 
      as.character()
      ) %>%
  mutate(hour = if_else(hour == "12" & am_pm == "AM", "00", hour) %>% 
           as.numeric()
           ) %>% 
  mutate(date_time = str_glue("{date} {hour}:{minute}") %>%
           lubridate::ymd_hm(tz = "US/Central")
         ) %>%
  select(date, date_time, everything(), -am_pm, -hour, -minute, -time,
         -time_cdt, -time_cst)

raw_df %>% filter(wind_speed %in% c("Calm", "-")) %>% nrow() 
raw_df %>% filter(temp == "-") %>% nrow()
clean_df %>% filter(is.na(temp_fahr)) %>% nrow()

clean_df <- raw_df %>%
  mutate(wind_speed = if_else(wind_speed %in% c("Calm", "-"), # 60701 wind_speed vals are "Calm" or "-"... treating as 0mph
                              "0", wind_speed)) %>%
  mutate(wind_speed = wind_speed %>% 
           str_remove("mph") %>% 
           str_trim() %>% 
           as.numeric()
           ) %>% 
  mutate(wind_dir = if_else(wind_speed %in% c("Calm", "-"), NA_character_, wind_dir)) %>% 
  mutate(temp = if_else(temp == "-", NA_character_, temp)) %>% # 1192 temp vals are "-" treating as NA
  mutate(temp = temp %>% 
           str_remove("°F") %>% 
           str_trim() %>% 
           as.numeric()
         ) %>% 
  select(date, date_time, wind_speed, wind_dir, temp) %>% 
  rename(wind_speed_mph = wind_speed, temp_fahr = temp)

%>% 
  mutate(temp = )
  # mutate(wind_speed = if_else(wind_speed == "Calm", "0", wind_speed)) %>%
  mutate_if(is.character, na_if, "-") %>% 
  mutate_if(is.character, na_if, "N/A") %>% 
  # mutate_if(is.character, na_if, "") %>% 
  mutate(wind_dir = if_else(wind_dir == "Calm", NA_character_, wind_dir)) %>%
  mutate_at(vars(temp, heat_index, dew_point, humidity, pressure,
                 visibility, wind_speed, gust_speed, precip, windchill),
            funs(as.numeric(str_trim(str_extract(., "-?\\d+(\\.\\d+?)"))))) %>% 
  rename(temp_fahr = temp, windchil_fahr = windchill, heat_index_fahr = heat_index,
         dew_point_fahr = dew_point, pressure_in = pressure, visibility_mi = visibility,
         wind_speed_mph = wind_speed, gust_speed_mph = gust_speed, precip_in = precip,
         humidity_percent = humidity)

write_rds(clean_df, "data/clean_df.rds")

target_vars_df <- clean_df %>%
  select(date_time, wind_speed_mph, wind_dir, temp_fahr)

write_rds(target_vars_df, "data/target_vars_df.rds")



target_vars_df %>% 
  # mutate(date = as.Date(date_time)) %>% 
  # group_by(date) %>% 
  # summarise(wind_speed_mph = mean(wind_speed_mph, na.rm = TRUE),
            # temp_fahr = mean(temp_fahr, na.rm = TRUE)) %>% 
  gather(var, val, -date_time, -wind_dir) %>% 
  ggplot(aes(date_time, val, color = var)) +
  geom_point(show.legend = FALSE) +
  facet_wrap(~ var, nrow = 2, scales = "free_y") +
  theme_minimal()

target_vars_df %>% 
  ggplot(aes(date_time, wind_speed_mph)) +
  geom_point()

library(forecast)

target_vars_df %>% 
  mutate(date = as.Date(date_time)) %>% 
  group_by(date) %>% 
  summarise(wind_speed_mph = mean(wind_speed_mph, na.rm = TRUE),
            temp_fahr = mean(temp_fahr, na.rm = TRUE)) %>% 
  ggplot(aes(date, wind_speed_mph)) +
  geom_point(show.legend = FALSE) +
  theme_minimal()

library(prophet)  
prophecy <- target_vars_df %>% 
  mutate(date = as.Date(date_time)) %>% 
  group_by(date) %>% 
  summarise(wind_speed_mph = mean(wind_speed_mph, na.rm = TRUE),
            temp_fahr = mean(temp_fahr, na.rm = TRUE)) %>% 
  rename(ds = date, y = wind_speed_mph) %>% 
  select(ds, y) %>% 
  prophet(weekly.seasonality = FALSE, mcmc.samples = 1000)

future <- make_future_dataframe(prophecy, periods = 365 * 2, freq = "day")

forecast <- prophecy %>% 
  predict(future)
  
plot(prophecy, forecast) +
  labs(x = NULL, y = "Wind Speed (mph)") +
  theme_minimal()

prophecy %>% 
  prophet_plot_components(forecast, uncertainty = TRUE, plot_cap = TRUE)
