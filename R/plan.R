plan = drake_plan(
  d = read_data(file_in('district-data.csv')),
  table_of_output = scrape_and_process_sites(list(d$district, d$state, d$nces_id, d$website_url))
)