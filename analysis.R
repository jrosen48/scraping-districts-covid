library(tidyverse)
library(rvest)

d <- read_csv('district-data.csv')

d <- d %>% filter(`website-url` != "NA")

d <- rename(d, district = "district-name",
            nces_id = "nces-id",
            website_url = "website-url")

d <- d %>% 
  arrange(state, district)

detector <- function(x) {
    str_detect(x, "coron*") |
    str_detect(x, "covid*")
}

access_site <- function(name, state, id, url) {
  
  h <- url %>% 
    read_html()
  
  write_xml(h, str_c("xml-data/", state, "-", name, "-", id, "-", Sys.Date(), ".xml"))
  
  t <- h %>% 
    html_text()
  
  links <- h %>% 
    html_nodes("a")
  
  corona <- str_detect(tolower(t), "corona*")
  covid <- str_detect(tolower(t), "covid*")
  
  link_found <- links %>% 
    html_text() %>% 
    tolower() %>% 
    detector() %>% 
    any()
  
  link_logical_to_index <- links %>% 
    html_text() %>%
    tolower() %>%
    detector()

  link_urls <- links %>%
    rvest::html_attr("href") %>% 
    str_c(url, .)

  link_urls <- link_urls[link_logical_to_index]
  
  print(str_c("Processed ", name, " (", state, ") --- corona = ", corona, "; covid = ", covid, "; LINK FOUND: ", link_found))
  #print(link_urls)
  
  tibble(district_name = name, state = state, nces_id = id, url = url, corona = corona, covid = covid, scraping_failed = FALSE, 
         link_found = link_found, link = list(link_urls))
  
}

output <- pmap(list(name = d$district, state = d$state, id = d$nces_id, url = d$website_url), 
               possibly(access_site, 
                        otherwise = tibble(district_name = NA, 
                                           state = NA, 
                                           nces_id = NA, 
                                           url = NA, 
                                           covid = NA,
                                           scraping_failed = TRUE, 
                                           link_found = NA,
                                           link = NA)))

output_df <- map_df(output, ~.)
