library(tidyverse)
library(rvest)

d <- read_csv('district-data.csv')

d <- d %>% filter(`website-url` != "NA")

d <- rename(d, district = "district-name",
            nces_id = "nces-id",
            website_url = "website-url")

d <- d %>% 
  arrange(state, district)

access_site <- function(name, state, id, url) {
  
  h <- url %>% 
    read_html()
  
  t <- h %>% 
    html_text()
  
  write_xml(h, str_c("xml-data/", state, "-", name, "-", id, "-", url, ".xml"))
  
  closed <- str_detect(tolower(t), "clos*")
  corona <- str_detect(tolower(t), "corona*")
  covid <- str_detect(tolower(t), "covid*")
  
  tibble(district_name = name, state = state, nces_id = id, url = url, closed = closed, corona = corona, covid = covid, scraping_failed = FALSE, content = t)
  print(str_c("Processed ", name, " (", state, ") --- closed = ", closed, "; corona = ", corona, "; covid = ", covid))
}

# access_site(NA, NA, NA, 'https://denairusd.org')

output <- pmap(list(name = d$district, state = d$state, id = d$nces_id, url = d$website_url), 
               possibly(access_site, 
                        otherwise = tibble(district_name = NA, 
                                           state = NA, 
                                           nces_id = NA, 
                                           url = NA, 
                                           closed = NA,
                                           corona = NA,
                                           covid = NA,
                                           scraping_failed = TRUE, 
                                           content = NA)))

# output <- map_df(output, ~.)