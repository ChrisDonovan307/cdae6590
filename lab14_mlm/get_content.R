# get_content
# 2023.12.17

# Pulling all the get_content functions for each portal into this script.
# They will be separated into get_socrata, get_ckan, and get_esri.
# Setting limits to 50,000 for now, not sure how high they need to go though.



# Socrata -----------------------------------------------------------------


# Original
get_socrata <- function(dm = "data.auburnwa.gov", limit = 15000) {
    "http://api.us.socrata.com/api/catalog/v1?domains=${dm}&limit=${limit}" %>%
        str_interp() %>%
        httr::GET() %>%
        content("text") %>%
        fromJSON(flatten = TRUE)
}

# With scroll_id
# NOTE - outdated
scroll_socrata <- function(domain,
                           limit = 15000,
                           scroll_id = NULL) {
    "http://api.us.socrata.com/api/catalog/v1?domains=${domain}&scroll_id=${scroll_id}&limit=${limit}" %>%
        str_interp() %>%
        httr::GET() %>%
        content("text") %>%
        fromJSON(flatten = TRUE)
}

get_socrata_type <- function(domain,
                             limit = 15000,
                             type = type) {
    "http://api.us.socrata.com/api/catalog/v1?domains=${domain}&only=${type}&limit=${limit}" %>%
        str_interp() %>%
        httr::GET() %>%
        content("text") %>%
        fromJSON(flatten = TRUE)
}



# CKAN --------------------------------------------------------------------


# Original function from Bev and Cong
get_ckan <- function(dm = "http://data.ci.newark.nj.us") {
    ckanr_setup(dm)
    result = tryCatch({
        package_search(q = '*:*', 
                       rows = 50000, 
                       as = 'table')
    }, error = function(e) {
        list(0)
    })
    return(result$results)
}


# CKAN with pagination
get_ckan_pag <- function(dm = "http://data.ci.newark.nj.us",
                         start = 0,
                         rows = 1000) {
    ckanr_setup(dm)
    result = tryCatch({
        package_search(q = '*:*', 
                       rows = rows,
                       start = start,
                       as = 'table')
    }, error = function(e) {
        list(0)
    })
    return(result$results)
}



# ESRI --------------------------------------------------------------------


get_esri <- function(dm) {
    result = tryCatch({
        "${dm}/data.json" %>%
            str_interp() %>%
            httr::GET() %>%
            content("text") %>%
            fromJSON(flatten = TRUE)
    }, error = function(e) {
        list(0)
    })
    return(result$dataset)
}
# Socrata Variations ------------------------------------------------------


# ### Socrata (Page two)
# get_socrata_page2 <- function(dm = "data.auburnwa.gov", 
#                               limit = 10000,
#                               offset = 10000) {
#     "http://api.us.socrata.com/api/catalog/v1?domains=${dm}&limit=${limit}&offset=${offset}" %>%
#         str_interp() %>%
#         httr::GET() %>%
#         content("text") %>%
#         fromJSON(flatten = TRUE)
# }
# 
# ### Socrata (pagination)
# 
# get_socrata_pag <- function(dm = "data.auburnwa.gov", limit = 50000) {
#     base_url <- "https://api.us.socrata.com/api/catalog/v1"
#     
#     # Initialize variables
#     offset <- 0
#     all_datasets <- list()
#     
#     repeat {
#         # Construct the API request URL with the current offset and limit
#         url <- sprintf("%s?domains=%s&limit=%s&offset=%s", base_url, dm, limit, offset)
#         
#         # Make the API request
#         response <- httr::GET(url)
#         
#         # Check if the request was successful
#         if (httr::status_code(response) == 200) {
#             # Parse the JSON content
#             datasets <- content(response, "text") %>% fromJSON(flatten = TRUE)
#             
#             # Append the current datasets to the list
#             all_datasets <- c(all_datasets, datasets)
#             
#             # Check if there are more datasets to retrieve
#             if (length(datasets) == limit) {
#                 # Increment the offset for the next request
#                 offset <- offset + limit
#             } else {
#                 # Break the loop if there are no more datasets
#                 break
#             }
#         } else {
#             # Print an error message and break the loop if the request was not successful
#             cat("Error:", httr::status_code(response), "\n")
#             break
#         }
#     }
#     
#     return(all_datasets)
# }
# 
# ### Socrata with httr2 and pagination
# get_socrata_httr_pag <- function(dm = "data.auburnwa.gov", limit = 50000) {
#     base_url <- "http://api.us.socrata.com/api/catalog/v1"
#     
#     req <- req_paginate(
#         url = paste0(base_url, "?domains=", dm, "&limit=", limit),
#         page_parser = list(content_type("application/json")),  # Move this part outside of list()
#         parse = json_parser(flatten = TRUE)  # Include this line for parsing JSON
#     )
#     
#     response <- run(req)
#     datasets <- response$content
#     
#     return(datasets)
# }
# 
# 
# ### Socrata by category?
# 
# get_socrata_na <- function(dm = "data.auburnwa.gov", limit = 50000) {
#     "http://api.us.socrata.com/api/catalog/v1?domains=${dm}&limit=${limit}&$where=classification.domain_category
# =Health" %>%
#         str_interp() %>%
#         httr::GET() %>%
#         content("text") %>%
#         fromJSON(flatten = TRUE)
# }


