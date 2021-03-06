
library(shiny)
library(devtools)
load_all("../bibliographica")
load_all("../estc")
load_all()
source("resultsdata_functions.R")

dataroot <- get_dataroot()
basedata <- readRDS(paste0(dataroot, "data/enriched_and_streamlined_data.Rds"))


# Define server logic required to draw a histogram
shinyServer(function(input, output) {

  api2_query_sanity <- reactive({
    query_verify_sanity(input$api2_query)
  })
  
  api2_query_results_df <- eventReactive(input$api_button, {
    if (api2_query_sanity()) {
      query_url <- octavoapi_get_query_url(query_string = input$api2_query,
                                           fields = list("ESTCID","totalPages"))
      print(query_url)
      query_results_df <- octavoapi_get_jsearch_query_results_df(query_url, column_names = c("id", "pages_ecco", "hits"))
      query_results_df <- octavoapi_format_jsearch_query_results(query_results_df, format_freq = FALSE)
      print(paste0("query_results_df rows: ", nrow(query_results_df)))
      query_counts <- octavoapi_get_query_counts(query_results_df)
      print(paste0("query_counts length: ", nrow(query_counts)))
      resultsdata <- resultsdata_add_query_hits(basedata, query_counts)
      resultsdata <- resultsdata_add_ecco_pages(resultsdata, query_results_df)
      resultsdata <- resultsdata_add_hits_per_page(resultsdata, pages_as_integers = TRUE, ecco_pages = TRUE)
      return(resultsdata)
    } else {
      empty_df <- data.frame(nodata = c("query did not validate"))
      return(empty_df)
    }
  })
  
  
  # api_url_results_df <- eventReactive(input$api_url_button, {
  #   print("foo!")
  # })
  
  # observers
  
  observeEvent(input$save_button, {
    # print("Button clicked!")
    ids_label <- input$ids_identifier
    query_res_df <- api2_query_results_df()
    query_res_ids <- query_res_df$id
    query_res_ids_df <- data.frame(id = query_res_ids)
    filename <- paste0("../output/saved_query_ids/", ids_label, ".csv")
    write.csv(query_res_ids_df, file = filename)
    showNotification(paste0("Query saved with identifier: ", ids_label), duration = 10, type = "message")
  })
  
     
  output$query_results <- renderDataTable({
    api2_query_results_df()
  }, options = list(orderClasses = TRUE)
  )

  output$helper_text <- renderText({
    "Query must be at least 5 characters long to avoid freezing the API.
    Example query: +america +rebellion"
  })
})

