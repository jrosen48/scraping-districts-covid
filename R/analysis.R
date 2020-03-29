library(tidyverse)
library(rvest)
library(osfr)

# 3-27 data
# analysis_plan <- osf_retrieve_file("7kpyn") %>%
#   osf_download()

f <- list.files("xml-data", full.names = T)

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
  #print(str_c("Processed ", f, " --- corona = ", corona, "; covid = ", covid, "; LINK FOUND: ", link_found))
  
  tibble(district_name = f,
         corona = corona, covid = covid,
         link_found = link_found, link = list(link_urls))
  
}

detector <- function(x) {
  str_detect(x, "coron*") |
    str_detect(x, "covid*")
}

otherwise <- tibble(district_name = NA,
                    corona = NA, covid = NA,
                    link_found = NA, link = NA)

## PROCESSING

l <- map(f, possibly(proc_xml, otherwise))

proc_data <- map_df(l, ~.)

proc_data <- proc_data %>% 
  mutate(present = if_else(corona | covid, TRUE, FALSE))

o <- pull(proc_data, link)
proc_data$n_links <- map_int(o, length)

x <- proc_data %>% 
  mutate(state = str_sub(district_name, start = 10),
         state = str_split(state, "-")) %>% 
  pull(state)

proc_data$state <- map_chr(x, ~.[[1]]) %>% tolower() %>% tools::toTitleCase()

out_data <- proc_data %>% 
  janitor::tabyl(state, present) %>% 
  rename(not_present = `FALSE`,
         present = `TRUE`) %>% 
  mutate(prop_present = present / (present + not_present)) %>% 
  arrange(prop_present) %>% filter(state != "<NA>")

mean_n_links <- proc_data %>% 
  group_by(state) %>% 
  summarize(mean_n_links = mean(n_links))

to_plot <- out_data %>% 
  left_join(mean_n_links) %>% 
  rename(ID = state) %>% 
  mutate(ID = tolower(ID))

node <- osfr::osf_retrieve_node("https://osf.io/2txjy/")

write_csv(proc_data, "2020-03-28-processed-data.csv")
osfr::osf_upload(node, "2020-03-28-processed-data.csv")

## PLOT by state

library(sf)  
US <- st_as_sf(maps::map("state", plot = FALSE, fill = TRUE))

tp <- US %>% 
  left_join(to_plot)

p <- ggplot(tp) +
  geom_sf(aes(fill = prop_present)) +
  coord_sf(crs = st_crs(3347), default =TRUE) +
  theme_void() +
  theme(legend.position = "bottom", text = element_text(family = "Times", size = 18)) +
  scale_fill_continuous("Proportion of districts' websites referencing COVID-19 or coronavirus")

p

ggsave("img/adoption-over-time.png", width = 11, height = 11)

states
}