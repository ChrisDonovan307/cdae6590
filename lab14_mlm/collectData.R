# collectData
# 2023.12.16

# Edited example script from Bev and Cong for pulling from state portals
# note: get_content functions are moved into '3_functions/get_content.R

# Deleted the section using the RSocrata package. I couldn't get it to work
# (could have tried harder though), and it doesn't seem necessary because 
# the individual portal strategy works fine



# Housekeeping ------------------------------------------------------------


# load state data portal links and clean it up a bit
raw <- read_excel("1_rawData/Open Data Websites_Jan2024.xlsx",
                  range = cell_cols(c("A:D"))) |> 
    janitor::clean_names()

# NOTE - urls MUST be formatted without https:// or trailing /
# also removing states without urls
state_portals <- raw |> 
    filter(!is.na(url)) |>
    mutate(domain = sapply(str_split(url, '//'), \(x) x[[2]])) |> 
    mutate(domain = str_replace(domain, "/$", ""),
           state = str_trim(state, side = 'right'))

# save this
saveRDS(state_portals, '2_cleanData/state_portals.RDS')



# Socrata Portals ---------------------------------------------------------


# # Get Socrata data
# # includes if statement for pagination and accounts for null results
# socrata_df <- state_portals %>%
#     filter(platform %in% "Socrata") %>%
#     mutate(fields = lapply(.$domain, function(x) {
#         list <- get_socrata(x)
#         
#         # Check if results exist
#         if (length(list$results) <= 0) {
#             return(NULL)
#             
#         } else if (nrow(list$results) < 10000) {
#             # If not capped, we're good
#             return(list$results)
#             
#         } else {
#             # If capped, start over with scroll function
#             # first page
#             output <- data.frame()
#             page <- scroll_socrata(x,
#                                    scroll_id = first(sort(list$results$resource.id)))
#             output <- bind_rows(output, page$results)
#             
#             # more pages if necessary
#             while (nrow(page$results) == 10000) {
#                 page <- scroll_socrata(x,
#                                        scroll_id = last(page$results$resource.id))
#                 output <- bind_rows(output, page$results)
#             }
#             return(output)
#         }
#     }))



## Type Method -------------------------------------------------------------


# socrata_df <- state_portals %>%
#     filter(platform %in% "Socrata") %>%
#     mutate(fields = lapply(.$domain, function(x) {
#         list <- get_socrata(x)
#         
#         # Check if results exist
#         if (length(list$results) <= 0) {
#             return(NULL)
#             
#         } else if (nrow(list$results) < 10000) {
#             # If not capped, we're good
#             return(list$results)
#             
#         } else {
#             # If capped, start over function by type
#             output <- map(types, ~ {
#                 type_output <- get_socrata_type(x,
#                                                 type = .)
#                 type_output[[1]]
#             })
#             output <- bind_rows(output)
#             return(output)
#         }
#     }))
# 
# # Check it
# get_str(socrata_df)
# get_str(socrata_df$fields, 1)
# # looks good so far, 17499
# 
# # NOTE: this works, but I'm investigating whether I should actually be 
# # querying all data types. If so, I should do it for all portals, because
# # it gets more results than querying without the 'only' parameter. 



# New Scroll Method -------------------------------------------------------


socrata_df <- state_portals %>%
    filter(platform %in% "Socrata") %>%
    mutate(fields = lapply(.$domain, function(x) {
        
        output <- data.frame()
        page <- scroll_socrata(x, 
                               limit = 15000,
                               scroll_id = '0')
        output <- bind_rows(output, page$results)
        
        # Check for null
        if (is.null(page$results) | length(page$results) <= 0) {
            
            warning(cat('No records found for domain:', x),
                    call. = FALSE)
            return(NULL)
            
        } else {
            # more pages if necessary
            while (nrow(page$results) == 10000) {
                page <- scroll_socrata(x,
                                       limit = 15000,
                                       scroll_id = last(page$results$resource.id))
                output <- bind_rows(output, page$results)
            }
            
            return(output)
        }}))

# Extract attributes
data_soc <- socrata_df[lapply(socrata_df$fields, length) > 0, ] %>%
    mutate(data =
               map(
                   fields,
                   ~ .x %>% select(
                       resource.name,
                       resource.description,
                       classification.domain_category,
                       classification.domain_tags,
                       resource.type,
                       resource.attribution,
                       resource.download_count,
                       resource.page_views.page_views_last_week,
                       resource.page_views.page_views_total,
                       owner.display_name,
                       resource.updatedAt
                   )
               )) %>%
    select(state, data) %>%
    unnest(cols = c(data))


# check it
get_str(data_soc)

# length by state
table(data_soc$state)

# save this
saveRDS(data_soc, file = "1_rawData/data_soc.rds")



# CKAN Portals ------------------------------------------------------------


# Using pagination function to re-query as necessary
ckan_df <- state_portals %>% filter(platform %in% "CKAN") %>%
    mutate(fields = lapply(.$domain, \(x) {
        
        # Pull first page,
        page <- get_ckan_pag(dm = x,
                             start = 0,
                             rows = 1000)
        
        # Check if results exist
        if (nrow(page) <= 0) {
            
            warning(cat('No records found for domain:', x),
                    call. = FALSE)
            return(NULL)
            
        # If not capped, it's fine
        } else if (nrow(page) < 1000) {
            
            return(page)
            
        # Otherwise it is 1000 and we run again
        } else {
            output <- page
            
            while (nrow(page) == 1000) {
                start <- nrow(output)
                page <- get_ckan_pag(dm = x,
                                     start = start,
                                     rows = 1000)
                output <- bind_rows(output, page)
            }
            return(output)
        }
    }))


# pull relevant attributes
data_ckan <- ckan_df[lapply(ckan_df$fields, length) > 0, ] %>%
    mutate(
        title = map(fields, ~ .x %>% select(title)),
        groups = map(fields, ~ .x %>% select(groups)),
        tags = map(fields, ~ .x %>% select(tags)),
        description = map(fields, ~ .x %>% select(notes)),
        organization = map(fields, ~ .x |> select(organization)),
    ) %>%
    mutate(list = map(fields, ~ .x %>% select(resources))) %>%
    mutate(resources = lapply(list, "[[", "resources")) %>%
    select(-c(domain, platform, fields, list)) |> 
    unnest(cols = c(title, groups, tags, description, organization, resources)) |> 
    mutate(contributor = organization$title)

# check
get_str(data_ckan)
table(data_ckan$state)

saveRDS(data_ckan, file = "1_rawData/data_ckan.rds")



# ArcGIS Portals ----------------------------------------------------------


# get content from esri
esri_df <- state_portals %>% filter(platform %in% "Esri") %>%
    mutate(fields = lapply(.$domain, get_esri))

get_str(esri_df)
get_str(esri_df$fields, 1)
get_str(esri_df$fields[[1]], 1)
head(esri_df$fields[[1]]$publisher.name, 10)
length(esri_df$fields[[1]]$publisher.name)
# this all looks fine


###
data_esri <- esri_df[lapply(esri_df$fields, length) > 0, ] %>%
    mutate(data1 = map(fields, ~ .x %>% select(title,
                                               description,
                                               keyword,
                                               modified))) %>%
    mutate(data2 = map(fields, ~ .x %>% select(distribution))) %>%
    mutate(data3 = lapply(fields, "[[", "publisher.name")) %>%
    select(state, data1, data2, data3) 

get_str(data_esri)
get_str(data_esri$data3, 1)
print(data_esri$data3[[6]])
# this all looks fine except for a couple NULLs

# how many NULLs
sum(unlist(map(data_esri$data3, is.null)))
# 2 NULLs

###

# There are some datasets (most) with no contributor information.
# replace the NULL cell with NA values
# CD: it looks to me like these are actually 100% filled?
#   Description, on the other hand, is mostly empty
num = which(lapply(data_esri$data3, length) == 0)

for (i in 1:length(num)) {
    data_esri$data3[num[i]] <-
        list(rep("NA", nrow(as.data.frame(
            data_esri$data2[num[i]]
        ))))
}

names(data_esri)[4] <- "publisher.name"

data_esri <- unnest(data_esri, cols = c(data1, data2, publisher.name))
table(data_esri$state)

# save this
saveRDS(data_esri, file = "1_rawData/data_esri.rds")



# Remove objects ----------------------------------------------------------


remove_data_objects()
