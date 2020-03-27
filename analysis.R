library(tidyverse)
library(rvest)

d <- read_csv('district-data.csv')

d <- d %>% filter(`website-url` != "NA")

d <- rename(d, district = "district-name",
            nces_id = "nces-id",
            website_url = "website-url")

access_site <- function(name, state, id, url) {
  
  t <- url %>% 
    read_html() %>% 
    html_text()
  
  candidate_site <- str_detect(tolower(t), "clos*") | str_detect(tolower(t), "corona*") | str_detect(tolower(t), "covid*")
  
  if (candidate_site) {
    tibble(district_name = name, state = state, nces_id = id, url = url, candidate_site = TRUE, scraping_failed = FALSE, content = t)
  } else {
    tibble(district_name = name, state = state, nces_id = id, url = url, candidate_site = FALSE, scraping_failed = FALSE, content = NA)
  }
}

d <- d[1:5, ]

output <- pmap(list(name = d$district, state = d$state, id = d$nces_id, url = d$website_url), 
     possibly(access_site, 
              otherwise = tibble(district_name = NA, 
                                 state = NA, 
                                 nces_id = NA, 
                                 url = NA, 
                                 candidate_site = FALSE, 
                                 scraping_failed = TRUE, 
                                 content = NA)))

output <- map_df(output, ~.)
