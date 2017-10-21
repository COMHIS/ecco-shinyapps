library(RCurl)
library(jsonlite)
library(httr)

get_eccoapi_url_base <- function() {
  return("https://vm0824.kaj.pouta.csc.fi/octavo/ecco/")
}


encode_url_request <- function(request) {
  request <- gsub(": \\+", ":", request)
  request <- gsub(" ", "%20", request)
  request <- gsub("\\|", "%7C", request)
  request <- gsub("\\+", "%2B", request)
  request <- gsub("'", "%27", request)
  request <- gsub(":", "%3A", request)
  request <- gsub("<", "%3C", request)
  request <- gsub(">", "%3E", request)
  request <- gsub("§", "%C2%A7", request)
  request <- gsub('"', "%22", request)
  request <- gsub('\\(', "%28", request)
  request <- gsub('\\)', "%29", request)
  return(request)
}


sanitize_term <- function(term){
  term <- as.character(term)
  term <- tolower(term)
  term <- gsub("[^a-zA-Z0-9 ]", "", term)
  term <- trimws(term)
  return(term)  
}

sanity_check_term_query <- function(term){
  if (nchar(term) < 5) {
    return(FALSE)
  }
  return(TRUE)
}


get_query_terms <- function(terms_dataframe) {
  top50terms <- head(terms_dataframe, 50)
  top50terms_list <- as.character(top50terms[, 1])
  top50terms_list <- gsub("[^a-z0-9%']", "", top50terms_list)
  top50merged <- paste0("%22", top50terms_list, "%22", collapse = "%20OR%20")
  top50merged <- gsub(" ", "%20", top50merged)
  return(top50merged)
}


validate_json <- function(jsondata) {
  conversion_results <- fromJSON(jsondata)
  if (length(conversion_results) > 0) {
    return(TRUE)
  }
  return(FALSE)
}


octavoapi_get_fields_from_input <- function(selected_fields) {
  switch(selected_fields,
         "contents_headings_all" = {
           return("<document§")
         },
         "contents_titlepage" = {
           return("<documentPart§+documentPartType:titlePage")
         },
         "headings_all" = {
           return("<section§+heading:")
         },
         "contents_headings_frontmatter" = {
           return("<documentPart§+documentPartType:frontmatter")
         },
         "contents_headings_backmatter" = {
           return("<documentPart§+documentPartType:backmatter")
         },
         return("<document§")
  )
}


octavoapi_get_query_url <- function(query_string,
                                    fields = list("ESTCID", "totalPages")) {
  url_base <- get_eccoapi_url_base()
  query_url <- paste0(url_base, "search?query=")
  fields_string <- ""
  for (field in fields) {
    fields_string <- paste0(fields_string, "&field=", field)
  }
  query_string_encoded <- encode_url_request(query_string)
  query_url <- paste0(query_url, query_string_encoded, fields_string, "&timeout=600")
  return(query_url)
}


octavoapi_get_terms <- function(api_url = "https://vm0824.kaj.pouta.csc.fi/octavo/ecco/",
                                    term,
                                    terms_conf = "&minCommonPrefix=1&maxEditDistance=1"){
  # eg. https://vm0824.kaj.pouta.csc.fi/octavo/ecco/terms?term=public
  terms_url <- paste0(api_url, "terms")
  term <- encode_url_request(sanitize_term(term))
  query_url <- paste0(terms_url, "?term=", term, terms_conf)
  terms_result <- toJSON(content(httr::GET(query_url)))
  return(terms_result)
}


octavoapi_get_search_results <- function(api_url = "https://vm0824.kaj.pouta.csc.fi/octavo/ecco/",
                                    query_terms,
                                    fields,
                                    min_score = "&minScore=1") {
  results_url <- paste0(api_url, "search")
  request_start <- paste0(results_url, "?query=")
  request_end <- paste0(fields,
                             " +",
                             "%28",
                             query_terms,
                             "%29",
                             "§document>",
                             "&field=ESTCID",
                             min_score,
                             "&limit=-1")
  request_end <- encode_url_request(request_end)
  api_request <- paste0(request_start, request_end)
  # request_results <- getURL(api_request)
  request_results <- toJSON(content(httr::GET(api_request)))
  results <- fromJSON(request_results)$results$docs
  return(results)
}


octavoapi_enrich_rest_query_results <- function(query_results) {
  names(query_results) <- c("id", "freq")
  query_results$id <- gsub("\\,$", "", as.character(query_results$id))
  query_results$freq <- as.numeric(as.character(query_results$freq))
  # rest_query_results$length <- as.numeric(as.character(rest_query_results$length))
  query_results$id <- apply(cbind(substr(query_results$id, 1, 1),
                                       gsub("^[A-Z]0*", "", query_results$id)),
                                 1, function(x) {paste(x, collapse = "")})
  formatted_ids <- query_results
  return(formatted_ids)
}


octavoapi_get_query_results <- function(term,
                                        api_url = "https://vm0824.kaj.pouta.csc.fi/octavo/ecco/",
                                        terms_conf = "&minCommonPrefix=1&maxEditDistance=1",
                                        fields,
                                        min_freq = "&minScore=1") {
  terms_json <- octavoapi_get_terms(api_url, term, terms_conf)
  if (validate_json(terms_json) == FALSE) {
    return(NULL) 
  } else {
    terms_df <- fromJSON(terms_json)$results
    terms_top50 <- get_query_terms(terms_df)
    query_results <- octavoapi_get_search_results(api_url,
                                             query_terms = terms_top50,
                                             fields,
                                             min_score = min_freq)
    return(query_results)
  }
}


octavoapi_get_query_ids <- function(input,
                                    api_url = "https://vm0824.kaj.pouta.csc.fi/octavo/ecco/",
                                    terms_conf,
                                    fields,
                                    min_freq = 1) {
  search_term <- tolower(as.character(input$search_term))
  min_score <- paste0("&minScore=", min_freq)
  query_results <- octavoapi_get_query_results(search_term,
                                               api_url,
                                               terms_conf,
                                               fields,
                                               min_score)
  if (is.null(query_results)) {
    return(NULL)
  }
  enriched_query_results <- octavoapi_enrich_rest_query_results(query_results)
  return(enriched_query_results)
}


octavoapi_sum_estcid_hits <- function(query_results) {
  # as one ecco id corresponds to multiple estcids, we need to summarize those:
  summarised_results <- aggregate(query_results$freq, by=list(query_results$id), FUN = sum)
  colnames(summarised_results) <- c("id", "freq")
  return(summarised_results)
}


octavoapi_get_termquery_string <- function(term = "religion",
                                           api_url = NULL,
                                           api_return_fields = "&field=ESTCID",
                                           level = "paragraph") {
  if (is.null(api_url)) {
    api_url <- get_eccoapi_url_base()
  }
  
  api_query_start <- "search?query="
  api_query_level_start <- paste0("<" , level, "§")
  api_query_text <- term
  api_query_level_end <- paste0("§" , level, ">")
  api_query_mid <- encode_url_request(paste0(api_query_level_start, api_query_text, api_query_level_end))
  api_params <- "&limit=-1&timeout=1000"
  api_query <- paste0(api_url, api_query_start, api_query_mid, api_return_fields, api_params)
  return(api_query)
}


octavoapi_get_query_set <- function(base_term, comparable_terms, query_level) {
  if (base_term == "") {
    base_set <- list(term = "", query = NA)
    base_q <- ""
  } else {
    # base_q <- paste0('"', base_term, '"')
    base_set <- list(term = base_term, query = octavoapi_get_termquery_string(term = base_term, level = query_level))
  }
  
  comparable_query_sets <-  vector("list", length(comparable_terms))
  i <- 1
  for (comparable_term in comparable_terms) {
    if (base_term != "") {
      query_terms <- paste0(base_term, " AND (", comparable_term, ")")
    } else {
      query_terms <- comparable_term
    }
    comparable_set <-
      list(term = comparable_term, query = octavoapi_get_termquery_string(term = query_terms, level = query_level))
    comparable_query_sets[[i]] <- comparable_set
    i <- i + 1
  }
  api2_query_set <- list(base_query_set = base_set,
                         comparable_query_sets = comparable_query_sets)
  return(api2_query_set)
}


octavoapi_format_jsearch_query_results <- function(jsearch_query_results, format_freq = TRUE) {
  if (format_freq) {
    jsearch_query_results$freq <- as.numeric(jsearch_query_results$freq)
  }
  jsearch_query_results$id <- gsub("\\,$", "", as.character(jsearch_query_results$id))
  # Takes first character of id, removes zeroes from start of numeric part,
  # adds first char back again.
  jsearch_query_results$id <- apply(cbind(substr(jsearch_query_results$id, 1, 1),
                                          gsub("^[A-Z]0*", "", jsearch_query_results$id)),
                                    1, function(x) {paste(x, collapse = "")})
  formatted_ids <- jsearch_query_results
  return(formatted_ids)
}


octavoapi_get_jsearch_query_results_df <- function(query_url, column_names = NA) {
  # returns df with optional column names
  results <- jsonlite::fromJSON(query_url, flatten = TRUE)$results$docs
  results_df <- data.frame(results)
  if (!all(is.na(column_names))) {
    names(results_df) <- column_names
  }
  return(results_df)
}


octavoapi_get_query_counts <- function(query_results_df) {
  names(query_results_df) <- c("estcid", "count")
  query_results_df$count <- as.numeric(query_results_df$count)
  results_df_summary <- plyr::count(query_results_df, vars = c("estcid"))
  names(results_df_summary) <- c("id", "freq")
  results_df_summary <- octavoapi_format_jsearch_query_results(results_df_summary)
  return(results_df_summary)
}


query_verify_sanity <- function(search_terms) {
  if (nchar(search_terms) > 5) {
    return(TRUE)
  }
  return(FALSE)
}

# 
# 
# format_api2_ids <- function(df_with_ids, id_field = "id") {
#   if (id_field != "id") {
#     df_with_ids$id <- df_with_ids[, id_field]
#   }
#   df_with_ids
#   
#   df_with_ids$id <- gsub("\\,$", "", as.character(df_with_ids$id))
#   # Takes first character of id, removes zeroes from start of numeric part,
#   # adds first char back again.
#   df_with_ids$id <- apply(cbind(substr(df_with_ids$id, 1, 1),
#                                 gsub("^[A-Z]0*", "", df_with_ids$id)),
#                           1, function(x) {paste(x, collapse = "")})
#   formatted_ids <- df_with_ids
#   return(formatted_ids)
# }

