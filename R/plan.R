plan = drake_plan(
  processed_data = read_data(file_in('district-data.csv')),
  
  # filtered_data = check_files_for_download(processed_data$state, 
  #                                          processed_data$district, 
  #                                          processed_data$nces_id,
  #                                          processed_data),

  table_of_output = scrape_and_process_sites(
    list(processed_data$district, 
         processed_data$state, 
         processed_data$nces_id, 
         processed_data$website_url
    ))
)