library(tidyverse)
library(rvest)
library(drake)

read_data <- function(f) {
  
  d <- read_csv(f)
  
  d <- d %>% filter(`website-url` != "NA")
  
  d <- rename(d, district = "district-name",
              nces_id = "nces-id",
              website_url = "website-url")
  
  d <- d %>% 
    arrange(state, district)

  d
}

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

scrape_and_process_sites <- function(list_of_args) {
  
output <- pmap(list(name = list_of_args[[1]], state = list_of_args[[2]],
                    id = list_of_args[[3]], url = list_of_args[[4]]), 
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

return(output_df)

}