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
  summarised_results <- aggregate(query_results$freq, by=list(query_results$id), FUN=sum)
  colnames(summarised_results) <- c("id", "freq")
  return(summarised_results)
}

