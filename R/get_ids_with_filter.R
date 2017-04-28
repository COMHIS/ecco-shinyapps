

get_ids_with_filter <- function(dataset, filter_value, filter_field) {
  filtered_data <- dataset[which(dataset[, filter_field] == filter_value), ]
  return(filtered_data)
}


get_data_subset_ids_csv <- function(dataset, csv_name) {
  subset_ids <- read.csv(paste0("../output/saved_query_ids/", csv_name, ".csv"))
  data_subset <- dataset[dataset$id %in% subset_ids$id, ]
  return(data_subset)
}


get_data_subset_id_df_filter <- function(dataset_augmented, ecco_dump_ids, filter_method) {
  if (filter_method == "none") {
    data_subset <- dataset_augmented
  } else if (filter_method == "and") {
    data_subset <- dataset_augmented[dataset_augmented$id %in% ecco_dump_ids$id, ]
  } else if (filter_method == "not") {
    data_subset <- dataset_augmented[!(dataset_augmented$id %in% ecco_dump_ids$id), ]
  }
  return(data_subset)
}


get_data_subset_with_filter <- function(dataset, filter_value, filter_field, case_sensitive = TRUE) {
  if (case_sensitive) {
    filtered_data <- dataset[which(grepl(filter_value, dataset[, filter_field])), ]
  } else {
    filtered_data <- dataset[which(grepl(tolower(filter_value), tolower(dataset[, filter_field]))), ]
  }
  return(filtered_data)
}
