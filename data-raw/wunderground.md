Wind Speed: Data Carpentry
================
Brendan Knapp
June 25, 2018

Package Dependencies
====================

You probably don't have these installed yet, but you an get them by running the following:

``` r
install.packages("tidyverse")
install.packages("rvest")

# Not necessary... I'm only using it for nice table outputs :
devtools::install_github("yihui/knitr")
```

{`tidyverse`} installs ~21 packages of pure unadulterated magic. These include {`ggplot2`}, {`dplyr`}, {`tibble`}, {`stringr`}, and {`tidyr`} which are probably the biggest perks of using R.

{`rvest`} is web scraping framework that makes pulling data from webpages super easy. It's essentially a port of Python's BeautifulSoup, but R's `%>%`, used to create pipelines of functions, makes scraping most websites easier.

Then, just load the packages by calling `library()`.

``` r
library(knitr) # Not necessary... I'm only using it for nice table outputs
library(lubridate) # simplify date/time handling
library(rvest)  # web-scraping framework
library(tidyverse) # core tidyverse: ggplot2, dplyr, tidyr, readr, purrr, tibble, stringr, forcats
```

This is a function that I'm only using to `prettify()` table outputs.

``` r
prettify <- function(df) {
  df %>% 
    head() %>% 
    kable()
}
```

Scrape Data
===========

Dissect the URL Format
----------------------

We're going to "visit" the daily weather page for Albion, NE for every available day. Like most websites, the URLs use an easy to follow system to organize each web page, similar to a file directory. Here, we use:

-   `"https://www.wunderground.com"`: the URL stem, root, base, homepage, etc.
-   `"/history"`: for historical data
-   `"/airport"`: I'm assuming the type/location of weather station
-   `"/KBVN"`: the airport code
-   `"/YYYY/MM/DD"`: the date format used
-   `"DailyHistory.html"`: the "suffix" and file extension

So the pattern used is: `"https://www.wunderground.com/history/airport/KBVN/YYYY/MM/DD/DailyHistory.html"`

Build URLs to Visit
-------------------

With that in mind, we can figure out the URL for each daily web page by inserting the applicable date.

1.  Create a sequence of dates (`seq.Date()`) for the available days, `from` `"2004-08-16"` `to` today's date (`Sys.Date()`).
2.  Convert the dates to character strings using `as.character()`, specifying that we want to use the `"/"` `format` to separate the date components.
    -   `"%Y/%m/%d"` is `strftime`, which is one of the ways C language represents dates/times and is omnipresent in programming language. Each component starts with `%`, so `%Y` specifies a 4 digit year, `%m` specifies a 2 digit month and `%d` specifies a 2 digit day, which we are separating with each `/`.
3.  Insert everything before the URL's date before the date and the "suffix" after the date using `paste0()`.
    -   `paste()` "pastes" strings together, but you specify how to seperate and collapse each string, which defaults to a space. `paste0()` let's us collapse the components together with nothing between them.
    -   The `.` in the middle of `paste0()` let's use the values from the function preceeding it, in this case the results of `as.character()` in step 2.

We're going to assign the results to a vairable called `urls`.

``` r
urls <- seq.Date(from = as.Date("2004-08-16"), to = Sys.Date(), by = "1 day") %>% # 1.
  as.character(format = "%Y/%m/%d") %>% # 2.
  paste0("https://www.wunderground.com/history/airport/KBVN/",., "/DailyHistory.html") # 3.
```

And here are the first ten:

``` r
urls[1:10]
```

    ##  [1] "https://www.wunderground.com/history/airport/KBVN/2004/08/16/DailyHistory.html"
    ##  [2] "https://www.wunderground.com/history/airport/KBVN/2004/08/17/DailyHistory.html"
    ##  [3] "https://www.wunderground.com/history/airport/KBVN/2004/08/18/DailyHistory.html"
    ##  [4] "https://www.wunderground.com/history/airport/KBVN/2004/08/19/DailyHistory.html"
    ##  [5] "https://www.wunderground.com/history/airport/KBVN/2004/08/20/DailyHistory.html"
    ##  [6] "https://www.wunderground.com/history/airport/KBVN/2004/08/21/DailyHistory.html"
    ##  [7] "https://www.wunderground.com/history/airport/KBVN/2004/08/22/DailyHistory.html"
    ##  [8] "https://www.wunderground.com/history/airport/KBVN/2004/08/23/DailyHistory.html"
    ##  [9] "https://www.wunderground.com/history/airport/KBVN/2004/08/24/DailyHistory.html"
    ## [10] "https://www.wunderground.com/history/airport/KBVN/2004/08/25/DailyHistory.html"

Scraper Function
================

Each web page has a table of values set in 20 minute intervals, which is all we really want. The strategy here is use a function that extracts the table information and builds a data frame. We can then feed `urls` to that function `build_df()` in a "loop" and build a list of those data frames.

There's a lot going on here, but the algorithm is:

1.  Feed the url to the function.
2.  Extract the `date` portion of the URL, which we'll use at the end.
3.  Read the HTML code of the page to which the URL points.
4.  Extract the `"table"` nodes from the HTML.
5.  Grab the `[[5]]`th table.
6.  Make it `as_tibble()`, a sweet data frame format.
7.  `mutate()` the data frame to add a column specifying the date and the url (just in case we want it).

The whole function is wrapped in `safely()`, which returns a functions results as a list of `$results` and `$errors`. The Internet is a broken mess that is in a perpetual state of falling apart, so web scraping on any sort of scale almost inevitable results in an error from a broken URL or slow server. `safely()` prevents our code from stopping when this happens and notates the error message in `$errors`.

``` r
build_df <- safely(function(url){ 
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
```

Scrape It!
==========

With our `build_df()` function set up, we use `map()` to "map" it over `urls`. `map()` always returns a list object.

It is always recommended to avoid `for()` loops in your R code, which is strange if you've coded in practically any other language. `map()` and a whole family of similar function essentially execute a `for()` loop for you by iterating through the first argument (`.x`) and applying the second argument (`.f`) to it and combining the results into an object. This was obnoxious to adjust to when I started but now I *really* dig it.

This will take a while. A single core of your CPU is visiting several thousand web pages and extracting contents and increasing performance by taking advantage of your computer's multiple cores isn't worth the explanation here.

``` r
raw_dfs <- map(.x = urls, .f = build_df)
```

Inspect Scrape Results
======================

Here's the first element of `raw_dfs`:

``` r
raw_dfs[[1]]
```

    ## $result
    ## # A tibble: 38 x 15
    ##    `Time (CDT)` Temp.   `Heat Index` `Dew Point` Humidity Pressure
    ##    <chr>        <chr>   <chr>        <chr>       <chr>    <chr>   
    ##  1 10:30 AM     73.4 °F -            64.4 °F     73%      30.05 in
    ##  2 11:10 AM     75.2 °F -            64.4 °F     69%      30.05 in
    ##  3 11:30 AM     77.0 °F -            64.4 °F     65%      30.05 in
    ##  4 11:50 AM     78.8 °F -            64.4 °F     61%      30.05 in
    ##  5 12:10 PM     78.8 °F -            62.6 °F     57%      30.05 in
    ##  6 12:30 PM     78.8 °F -            62.6 °F     57%      30.04 in
    ##  7 12:50 PM     80.6 °F 81.8 °F      62.6 °F     54%      30.03 in
    ##  8 1:10 PM      80.6 °F 81.8 °F      62.6 °F     54%      30.04 in
    ##  9 1:30 PM      82.4 °F 83.4 °F      62.6 °F     51%      30.03 in
    ## 10 2:10 PM      82.4 °F 83.4 °F      62.6 °F     51%      30.03 in
    ## # ... with 28 more rows, and 9 more variables: Visibility <chr>, `Wind
    ## #   Dir` <chr>, `Wind Speed` <chr>, `Gust Speed` <chr>, Precip <chr>,
    ## #   Events <lgl>, Conditions <chr>, date <date>, url <chr>
    ## 
    ## $error
    ## NULL

We can see the work `safely()` did as we have a `$result`, which is the data frame constructed from the table we were after, and `$error`, which is `NULL` as there wasn't an error for this URL.

We can check if there were *any* errors by `map()`ping `raw_dfs` and grabbing the `$error` portion. `compact()` will remove any `NULL` values, and `length()` will tell us how long the list of final results is.

``` r
raw_dfs %>% 
  map("error") %>% 
  compact() %>% 
  length()
```

    ## [1] 0

0 errors!

Clean Data
==========

We don't really want a list of data frames; we want a single data frame that we can use for our analysis. With that in mind, we need to grab only the data frames in each `$result` and combine them row-wise.

Before we get to that, we want to be able to clean the column names of the data frame.

Clean Column Names
------------------

The `col_name`s currently present are great and all, but they are meant to display data, not interact with data. So we're going to build a function that:

1.  replaces all `" "` with `"_"`: `str_replace_all()`
2.  removes `"."`s, `"("`s, and `")"`s: `str_remove_all()`
3.  converts the letters to lower case: `str_to_lower()`

Dealing with text is complicated as you need to use regular expressions to do anything truly useful, so step 2 may seem confusing. `"\\"` "escapes" whatever is following it, which we need to do for `"."`, `"("`, and `")"` because those are special characters that otherwise mean something else in a regular expression. That's a whole other beast to explore at some later point.

With an algorithm in mind, we can wrap it up in a function that we'll call `clean_names()`:

``` r
clean_names <- function(col_name){
  col_name %>% 
    str_replace_all(" ", "_") %>%      # 1.
    str_remove_all("\\.|\\(|\\)") %>%  # 2.
    str_to_lower()                     # 3.
}
```

Similar to `map()`, which always returns results as a list, we can use `map_df()`, which will always return the results as a data frame.

So let's:

1.  Take `raw_dfs` and `map_df`, calling `"result"` to grab only the `$result` data frames.
2.  `select()` will let us rearrange the columns how we want (and omit any we don't).
    -   It's usually simplest to ensure that `date` is your first column, so we can use that as the "first" argument, then use `everything()` as the "second" argument to grab all of the other columns.
3.  Pass our `clean_names()` function to `rename_all()` to clean all the column names.

``` r
raw_df_clean_names <- raw_dfs %>% 
  map_df("result") %>%            # 1.
  select(date, everything()) %>%  # 2.
  rename_all(clean_names)         # 3.
```

Here's our data frame:

``` r
raw_df_clean_names %>% 
  select(-url) %>%  # save some screen space
  prettify()
```

| date       | time\_cdt | temp    | heat\_index | dew\_point | humidity | pressure | visibility | wind\_dir | wind\_speed | gust\_speed | precip | events | conditions | windchill | time\_cst |
|:-----------|:----------|:--------|:------------|:-----------|:---------|:---------|:-----------|:----------|:------------|:------------|:-------|:-------|:-----------|:----------|:----------|
| 2004-08-16 | 10:30 AM  | 73.4 °F | -           | 64.4 °F    | 73%      | 30.05 in | 10.0 mi    | WSW       | 6.9 mph     | -           | N/A    | NA     | Clear      | NA        | NA        |
| 2004-08-16 | 11:10 AM  | 75.2 °F | -           | 64.4 °F    | 69%      | 30.05 in | 10.0 mi    | WSW       | 8.1 mph     | -           | N/A    | NA     | Clear      | NA        | NA        |
| 2004-08-16 | 11:30 AM  | 77.0 °F | -           | 64.4 °F    | 65%      | 30.05 in | 10.0 mi    | SW        | 5.8 mph     | -           | N/A    | NA     | Clear      | NA        | NA        |
| 2004-08-16 | 11:50 AM  | 78.8 °F | -           | 64.4 °F    | 61%      | 30.05 in | 10.0 mi    | WSW       | 6.9 mph     | -           | N/A    | NA     | Clear      | NA        | NA        |
| 2004-08-16 | 12:10 PM  | 78.8 °F | -           | 62.6 °F    | 57%      | 30.05 in | 10.0 mi    | WSW       | 8.1 mph     | -           | N/A    | NA     | Clear      | NA        | NA        |
| 2004-08-16 | 12:30 PM  | 78.8 °F | -           | 62.6 °F    | 57%      | 30.04 in | 10.0 mi    | West      | 8.1 mph     | -           | N/A    | NA     | Clear      | NA        | NA        |

Now we can diagnose the rest of our data cleaning tasks.

Clean Times
-----------

Notice that there are `time_cdt` and `time_cst` columns. CDT and CST time zones refer to he same thing (Central Time Zone), so we need create a single column to fix this. You'll also notice times are annotated in civilian time (12 hours + AM or PM). Computer don't know what this is (because \*spoiler alert\*: it's dumb), so we need to fix that as well.

Let's start by creating a single time column by combining the (identical) time zones.

### Clean Time Zones

It's easiest to use `case_when()`, which is derived from the similarly-named SQL operator. `case_when()` let's us use logic without nesting a bunch of `if()`s, `else()`s, and/or `base::ifelse()`/`dplyr::if_else()`s. The `~` is used to create a formula, where the left side is whatever we're testing, which needs to return `TRUE` or `FALSE` and the right side is what to do with the value if it's `TRUE`.

We want to make our `time` variable `time_cst` when it's not missing (`NA`) and `time_cdt` when it's not `NA`. We can test this with `is.na()`, but, since that would return `TRUE` if it is missing, we need to negate it with `!`. To recap, `!is.na(value)` is read as `value` is not `NA`.

To operate on the values of each variable/column, we use `mutate()`. The basic syntax is `mutate(my_variable = my_function(my_variable))`, which will transform every value of the `my_variable` column. We also use it to create new columns, which we'll do here to create our `time` column.

We also need to pad the times that are missing a leading `"0"` where appropriate. Notice that if the hour portion of the time is a single digit, the value is of a different length than those of doube digits. If we don't fix this, we might get caught with our pants down later on.

With that in mind, we'll take `raw_df_clean_names` and:

1.  `mutate()`, setting `time` equal to a `case_when()`
    -   when `time_cst` isn't missing (`!is.na(time_cst)`), set it to (`~`) `time_cst`
    -   when `time_cdt` isn't missing (`!is.na(time_cdt)`), set it to (`~`) `time_cdt`
2.  Pad the resulting numbers with `"0"` using `str_pad()`.
    -   The format should be `"HH:MM AM"`, meaning we want all of our times to have a `width` of `8` characters (spaces count), which we accomplish by `pad`ding `"0"` on the `"left"` hand `side`.

``` r
raw_df_clean_tz <- raw_df_clean_names %>% 
  mutate(time = case_when(                          # 1.
    !is.na(time_cst) ~ time_cst,
    !is.na(time_cdt) ~ time_cdt
    ) %>%
      str_pad(width = 8, side = "left", pad = "0")  # 2.
    ) 
```

We can take a look at our `time` column like so:

``` r
raw_df_clean_tz %>% 
  select(time)
```

    ## # A tibble: 354,367 x 1
    ##    time    
    ##    <chr>   
    ##  1 10:30 AM
    ##  2 11:10 AM
    ##  3 11:30 AM
    ##  4 11:50 AM
    ##  5 12:10 PM
    ##  6 12:30 PM
    ##  7 12:50 PM
    ##  8 01:10 PM
    ##  9 01:30 PM
    ## 10 02:10 PM
    ## # ... with 354,357 more rows

Now we need to drop to use the `"AM"` and `"PM"` information in `time` to convert them to 24-hour/military time so our computers can actually understand them.

There's a lot going on here, but it breaks down like this:

1.  `mutate()` to create an `am_pm` column by `str_extract()`ing the AM/PM portion of `time` with a regular expression.
2.  `mutate()` to create a `minute` column by `str_extract()`ing the minute portion of `time` with a regular expression and converting to a number using `as.numeric()`.
3.  `mutate()` to create a `hour` column by `str_extract()`ing the hour portion of `time` with a regular expression and converting to a number using `as.numeric()`.
    -   Here, we use `case_when()`, because we want to add `12` to rows where our `am_pm` variable is `"PM"`.
4.  `mutate()` to fix those `hour`s that are equal (`==`) to `12`, and (`&`) `am_pm` is `"AM"` using `if_else()`.
    -   This is because 12:00 AM - 12:59 AM are really 00:00 - 00:59 in 24-hour time.
    -   In `if_else()`, your first argument is your test, the second is what to use when the test returns `TRUE`, and the third argument is what to use when the the test returns `FALSE`.
5.  `mutate()` to create a `date_time` column that creates a proper temporal variable so we know can know exactly when we are.
    -   `str_glue()` is some Pythonista secret-sauce for handling strings. We we can refer to our `{date}`, `{hour}`, and `{minute}` columns and plug them in how we want as `{}` will be interpreted as code inside the string.
    -   We can then take the resulting strings and convert them to a date time objects with `ymd_hm()`. I also specified the timezone (`tz`) here.
6.  `select()` the columns in the order desired. Here we also drop the redundant `am_pm`, `hour`, `minute`, `time`, `time_cdt`, and `time_cst` columns by "subtracting" them with `-`.

``` r
raw_df_clean_time <- raw_df_clean_tz %>%
  mutate(am_pm = str_extract(time, "(A|P)M")) %>%                        # 1.
  mutate(minute = as.numeric(str_extract(time, "(?<=:)\\d{2}"))) %>%     # 2.
  mutate(hour = case_when(                                               # 3.
    am_pm == "AM" ~ as.numeric(str_extract(time, "\\d{2}(?=:)")),
    am_pm == "PM" ~ as.numeric(str_extract(time, "\\d{2}(?=:)")) + 12
    )) %>%
  mutate(hour = if_else(hour == 12 & am_pm == "AM", 0, hour)) %>%        # 4.
  mutate(date_time = str_glue("{date} {hour}:{minute}") %>%              # 5.
           ymd_hm(tz = "US/Central")
         ) %>%
  select(date, date_time, everything(), -am_pm, -hour, -minute, -time,   # 6.
         -time_cdt, -time_cst)
```

And here's the result:

``` r
raw_df_clean_time %>% 
  select(-url) %>%  # save some screen space
  prettify()
```

| date       | date\_time          | temp    | heat\_index | dew\_point | humidity | pressure | visibility | wind\_dir | wind\_speed | gust\_speed | precip | events | conditions | windchill |
|:-----------|:--------------------|:--------|:------------|:-----------|:---------|:---------|:-----------|:----------|:------------|:------------|:-------|:-------|:-----------|:----------|
| 2004-08-16 | 2004-08-16 10:30:00 | 73.4 °F | -           | 64.4 °F    | 73%      | 30.05 in | 10.0 mi    | WSW       | 6.9 mph     | -           | N/A    | NA     | Clear      | NA        |
| 2004-08-16 | 2004-08-16 11:10:00 | 75.2 °F | -           | 64.4 °F    | 69%      | 30.05 in | 10.0 mi    | WSW       | 8.1 mph     | -           | N/A    | NA     | Clear      | NA        |
| 2004-08-16 | 2004-08-16 11:30:00 | 77.0 °F | -           | 64.4 °F    | 65%      | 30.05 in | 10.0 mi    | SW        | 5.8 mph     | -           | N/A    | NA     | Clear      | NA        |
| 2004-08-16 | 2004-08-16 11:50:00 | 78.8 °F | -           | 64.4 °F    | 61%      | 30.05 in | 10.0 mi    | WSW       | 6.9 mph     | -           | N/A    | NA     | Clear      | NA        |
| 2004-08-16 | 2004-08-17 00:10:00 | 78.8 °F | -           | 62.6 °F    | 57%      | 30.05 in | 10.0 mi    | WSW       | 8.1 mph     | -           | N/A    | NA     | Clear      | NA        |
| 2004-08-16 | 2004-08-17 00:30:00 | 78.8 °F | -           | 62.6 °F    | 57%      | 30.04 in | 10.0 mi    | West      | 8.1 mph     | -           | N/A    | NA     | Clear      | NA        |

Clean Target Variables
----------------------

### Diagnose "Odd" Values

After doing some exploration of the data, we can see that we need take care of some values that are "missing" or "0", but aren't noted as such. To prevent any surprises later, let's figure out how many "odd" values we're going to need to handle.

`wind_speed`s are numbers, in miles per hour, `"Calm"`, or `"-"`. We can count up the `"Calm"`s and `"-"`s by `filter()`ing the rows for only `wind_speed` values that are `%in%` `"Calm"` or `"-"` and looking at how many rows remain with `nrow()`.

`wind_dir`s will indicate direction as a string, or be `"Calm"` or `"-"`.

``` r
raw_df_clean_time %>% 
  filter(wind_speed %in% c("Calm", "-")) %>% 
  nrow() 
```

    ## [1] 60701

``` r
raw_df_clean_time %>% 
  filter(wind_dir %in% c("Calm", "-")) %>% 
  nrow() 
```

    ## [1] 60614

`temp`eratures are numbers, in °F, or `"-"`. Using the same technique as with `wind_speed`, we can count those up.

``` r
raw_df_clean_time %>% 
  filter(temp == "-") %>% 
  nrow()
```

    ## [1] 1192

With that in mind, let's clean up the `wind_speed`s and `wind_dir`s. It's not clear as to what `"-"` actually refers, but I'm suspecting it's 0 (or at least ~ 0), which I'm certain is the case with`"Calm"`.

Let's turn those values into actual `0`s by:

1.  `mutate()`ing wind\_speed with `if_else()`
    -   if `wind_speed` is `"Calm"` or `"-"`, make it `"0"`, or else leave it as the value it already is (`wind_speed`)
2.  Do the same to `wind_dir`, except we're going to set our "missing" values to `NA` of type `character` with `NA_character_`
3.  `mutate()` again and
    -   strip the `"mph"` from the text with `str_remove()`
    -   strip the the whitespace on either side of the text with `str_trim()`
    -   convert the `wind_speed` to actualy numbers with `as.numeric()`

``` r
raw_df_clean_wind <- raw_df_clean_time %>%
  mutate(wind_speed = if_else(wind_speed %in% c("Calm", "-"),                            # 1.
                              "0", wind_speed)) %>%
  mutate(wind_dir = if_else(wind_dir %in% c("Calm", "-"), NA_character_, wind_dir)) %>%  # 2.
  mutate(wind_speed = wind_speed %>%                                                     # 3.
           str_remove("mph") %>%                                                         # remove "mph"
           str_trim() %>%                                                                # trim white space
           as.numeric()                                                                  # convert to numeric
           ) 
```

And here are the resulting values:

``` r
raw_df_clean_wind %>% 
  select(wind_speed, wind_dir)
```

    ## # A tibble: 354,367 x 2
    ##    wind_speed wind_dir
    ##         <dbl> <chr>   
    ##  1        6.9 WSW     
    ##  2        8.1 WSW     
    ##  3        5.8 SW      
    ##  4        6.9 WSW     
    ##  5        8.1 WSW     
    ##  6        8.1 West    
    ##  7        5.8 WSW     
    ##  8        6.9 WSW     
    ##  9        4.6 WSW     
    ## 10        5.8 SW      
    ## # ... with 354,357 more rows

Using the same workflow, let's clean up the `temp` column by:

1.  `mutate()`ing `temp` to be `NA_character_` (it's not likely that missing temperatures are 0) if it's `"-"`
2.  `str_remove()`ing `"°F"`
3.  `str_trimming` the white space
4.  making the values `as.numeric()`.

``` r
raw_df_clean_temp <- raw_df_clean_wind %>% 
  mutate(temp = if_else(temp == "-", NA_character_, temp)) %>%
  mutate(temp = temp %>% 
           str_remove("°F") %>% 
           str_trim() %>% 
           as.numeric()
         )
```

And here are the resulting values:

``` r
raw_df_clean_temp %>% 
  select(temp)
```

    ## # A tibble: 354,367 x 1
    ##     temp
    ##    <dbl>
    ##  1  73.4
    ##  2  75.2
    ##  3  77  
    ##  4  78.8
    ##  5  78.8
    ##  6  78.8
    ##  7  80.6
    ##  8  80.6
    ##  9  82.4
    ## 10  82.4
    ## # ... with 354,357 more rows

`clean_df`
----------

With our target variables cleaned up, let's make our final, `clean_df` by:

1.  `select()`ing our target variables
    -   we'll keep both `date` and `date_time` for now
2.  `rename()`ing our target variables just to keep track of their units of measurement

``` r
clean_df <- raw_df_clean_temp %>% 
  select(date, date_time, wind_speed, wind_dir, temp) %>% # 1.
  rename(wind_speed_mph = wind_speed, temp_fahr = temp)   # 2.
```

And here it is:

``` r
clean_df %>% 
  prettify()
```

| date       | date\_time          |  wind\_speed\_mph| wind\_dir |  temp\_fahr|
|:-----------|:--------------------|-----------------:|:----------|-----------:|
| 2004-08-16 | 2004-08-16 10:30:00 |               6.9| WSW       |        73.4|
| 2004-08-16 | 2004-08-16 11:10:00 |               8.1| WSW       |        75.2|
| 2004-08-16 | 2004-08-16 11:30:00 |               5.8| SW        |        77.0|
| 2004-08-16 | 2004-08-16 11:50:00 |               6.9| WSW       |        78.8|
| 2004-08-16 | 2004-08-17 00:10:00 |               8.1| WSW       |        78.8|
| 2004-08-16 | 2004-08-17 00:30:00 |               8.1| West      |        78.8|

And then we can save it to `"data/clean_df.rds"` like so:

``` r
write_rds(clean_df, "data/clean_df.rds")
```

Which can be read back in like so:

``` r
clean_df.rds <- read_rds("data/clean_df.rds")
```
