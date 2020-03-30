read_data <- function(f) {
  
  d <- read_csv(f)
  
  d <- d %>% filter(`website-url` != "NA")
  
  d <- rename(d, 
              district = "district-name",
              nces_id = "nces-id",
              website_url = "website-url")
  
  d <- d %>% 
    arrange(state, district)
  
  d
}

check_if_up_to_date <- function (state, name, id, f) {
  string <- str_c(state, "-", name, "-", id, "-", Sys.Date(), ".xml")
  string %in% f
}

detector <- function(x) {
  str_detect(x, "coron*") |
    str_detect(x, "covid*")
}

access_site <- function(name, state, id, url, f) {
  
  if (check_if_up_to_date(state, name, id, f)) {
    #print(str_c("already accessed; skipping", name, " (", state, ")"))
    return(tibble(district_name = name,
                  state = state,
                  nces_id = id, 
                  url = NA, 
                  corona = NA,
                  covid = NA, 
                  scraping_failed = NA, 
                  link_found = NA, 
                  link = NA,
                  already_accessed = TRUE))
  }
  
  h <- url %>% 
    read_html()
  
  file_name <- str_c("xml-data/", state, "-", name, "-", id, "-", Sys.Date(), ".xml")
  
  write_xml(h, file_out(file_name))
  
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
         link_found = link_found, link = list(link_urls), already_accessed = FALSE)
  
}

scrape_and_process_sites <- function(list_of_args) {
  
  f <- list.files("xml-data")
  
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
                                             link = NA)),
                 f = f)
  
  output_df <- map_df(output, ~.)
  
  return(output_df)
  
}

proc_xml <- function(f) {
  
  #f <- f[1]
  
  h <- f %>%
    read_html()
  
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
  
  link_text <- links %>% 
    html_text() %>%
    tolower()
  
  link_logical_to_index <- link_text %>% 
    detector()
  
  link_urls <- links %>%
    rvest::html_attr("href") %>% 
    as.character()
  
  link_urls <- link_urls[link_logical_to_index]

  tibble(district_name = f,
         corona = corona, covid = covid,
         link_found = link_found, link = list(link_urls))
  
}