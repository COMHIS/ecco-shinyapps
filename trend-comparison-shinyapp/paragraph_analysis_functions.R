

add_publication_year <- function(processed_jsearch_query_results, 
                                 estc_data) {
  title_year_df <- processed_jsearch_query_results
  title_year_df$year <- estc_data[match(title_year_df$id, estc_data$id),
                                  "publication_year"]
  return(title_year_df)
}


summarize_hits_per_year <- function(title_year_df, years = list(1705, 1799)) {
  yearly_hits_summary <- aggregate(title_year_df$freq,
                                  by = list(title_year_df$year),
                                  FUN = sum)
  years_range <- years[[1]]:years[[2]]
  years_df <- data.frame(year = years_range, hits = rep(0, length(years_range)))
  names(yearly_hits_summary) <- c("year", "hits")
  years_df$hits <- yearly_hits_summary[match(years_df$year, 
                                             yearly_hits_summary$year),
                                       "hits"]
  return(years_df)
}


get_hits_relative_frequency_yearly <- function(hits_subset_yearly, hits_all_yearly) {
  averages_yearly <- hits_subset_yearly
  averages_yearly["total_hits"] <-
    hits_all_yearly[match(averages_yearly$year, hits_all_yearly$year), "hits"]
  averages_yearly["frequency"] <-
    averages_yearly["hits"] / averages_yearly["total_hits"]
  averages_yearly <- averages_yearly[c("year", "frequency")]
  return(averages_yearly)
}


get_hits_yearly_for_api_query <- function(api_query, years = list(1705, 1799)) {
  # query_results_df <- octavoapi_get_jsearch_query_results_df(api_query)
  query_results_df <- octavoapi_get_stats_jsearch_query_results_df(api_query)
  # query_counts <- octavoapi_get_query_counts(query_results_df)
  # query_counts <- add_publication_year(query_counts, dataset)
  yearly_hits_summary <- query_results_df[, c("fields.publication_year", "stats.docFreq")]
  names(yearly_hits_summary) <- c("year", "hits")
  years_range <- years[[1]]:years[[2]]
  query_hits_yearly <- data.frame(year = years_range, hits = rep(0, length(years_range)))
  query_hits_yearly$hits <- yearly_hits_summary[match(query_hits_yearly$year, 
                                                yearly_hits_summary$year),
                                               "hits"]
  # query_hits_yearly <- years_df
  return(query_hits_yearly)
}


get_total_ecco_titles_yearly <- function(dataset, years = list(1705, 1799)) {
  year_list <- years[[1]]:years[[2]]
  subset_ids <- readRDS(paste0("../data/", "ecco_dump_ids", ".Rds"))
  data_subset <- dataset[dataset$id %in% subset_ids$id, ]
  yearly_titles <- plyr::count(data_subset, 'publication_year')
  yearly_title_results <- data.frame(year = year_list)
  yearly_title_results$titles <-
    yearly_titles[match(yearly_title_results$year, yearly_titles$publication_year), "freq"]
  names(yearly_title_results) <- c("year", "hits")
  return(yearly_title_results)
}


get_total_summary_yearly <- function(input_json, total_count = "totalParagraphs", years = list(1705, 1799)) {
  # totalTitles totalParagraphs documentLength
  json_data <- jsonlite::fromJSON(input_json)
  json_data$publication_year <- as.integer(substr(json_data$pubDateStart, 1, 4))
  json_data <- subset(json_data, publication_year >= years[1] & publication_year <= years[2])
  if (total_count == "totalTitles") {
    yearly_variable_summary <- plyr::count(json_data, 'publication_year')
    # should take in account number of unique estcid or not?
    # uniikki <- unique(json_data$ESTCID)
    # subbed <- gsub("([A-Z]{1})0*", "\\1", uniikki)
  } else {
  yearly_variable_summary <- aggregate(json_data[, total_count],
                                       by = list(json_data$publication_year),
                                       FUN = sum)
  }
  names(yearly_variable_summary) <- c("year", "hits")
  return(yearly_variable_summary)
}


get_yearly_paragraph_frequencies_list <- function(blank_total,
                                                  paragraph_query_set,
                                                  dataset,
                                                  subcorpus_filter = FALSE) {
  
  base_set <- paragraph_query_set$base_query_set
  print("querying api for base term")
  if (base_set$term == "" && subcorpus_filter == TRUE) {
    base_query_hits_yearly <- get_total_ecco_titles_yearly(dataset, years = list(1705, 1799))
    print("base term blank, subcorpus filter applied")
  } else if (base_set$term == "") {
    print("base term blank")
    # base_query_hits_yearly <- readRDS("./ecco_titles_yearly.Rds")
    base_query_hits_yearly <- switch(blank_total,
                                     titles = readRDS("./ecco_titles_yearly.Rds"),
                                     paragraphs = readRDS("./ecco_total_paragraphs.Rds"),
                                     words = readRDS("./ecco_total_tokens.Rds"))
  } else {
    base_query_hits_yearly <- get_hits_yearly_for_api_query(base_set$query,
                                                            # dataset,
                                                            years = list(1705, 1799))
  }
  
  comparable_sets <- paragraph_query_set$comparable_query_sets
  comparable_results <- vector("list", length(comparable_sets))
    
  i <- 1
  for (query_set in comparable_sets) {
    print(paste0("querying api for comparable term: ", query_set$term))
    comparable_hits_yearly <-
      get_hits_yearly_for_api_query(query_set$query,
                                    years = list(1705, 1799))
    comparable_yearly_relative_freq <-
      get_hits_relative_frequency_yearly(comparable_hits_yearly, 
                                         base_query_hits_yearly)
    comparable_result_set <- list(term = query_set$term,
                                  data = comparable_yearly_relative_freq)
    comparable_results[[i]] <- comparable_result_set
    i <- i + 1
  }
  return(comparable_results)
}
