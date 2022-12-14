# Libraries ====================================================================
library(tidyverse)
library(here)
library(assertthat)
library(Radlibrary)

# Functions ====================================================================
fb_short <- function(id, token,
                     ## https://www.facebook.com/ads/library/api/releasenotes
                     ## Still v12.0 for now: use ?adlib_build_query
                     ## Full fields give error:
                     ## Please reduce the amount of data you're asking for,
                     ## then retry your request
                     fields,
                     # fields = c(
                     #   "ad_creation_time",
                     #   "ad_creative_body",
                     #   "ad_creative_link_caption",
                     #   "ad_creative_link_description",
                     #   "ad_creative_link_title",
                     #   "ad_delivery_start_time",
                     #   "ad_delivery_stop_time",
                     #   "ad_snapshot_url",
                     #   "currency",
                     #   "demographic_distribution",
                     #   "funding_entity",
                     #   "impressions",
                     #   "page_id",
                     #   "page_name",
                     #   "potential_reach",
                     #   "publisher_platforms",
                     #   "region_distribution",
                     #   "spend"
                     # ),
                     max_date = "2022-12-31",
                     min_date = "2021-01-01",
                     limit = 5000,
                     ...) {
  if (!(fields %in% c("ad_data", "demographic_data", "region_data"))) {
    stop("Use the specified set of columns for now.")
  }
  
  query <- adlib_build_query(
    ad_reached_countries = "US",
    ad_active_status = "ALL",
    ## ad_type = "POLITICAL_AND_ISSUE_ADS", ---> this is implicit
    search_page_ids = id,
    ad_delivery_date_max = max_date,
    ad_delivery_date_min = min_date,
    limit = limit,
    fields = fields,
    ...
  )
  resp <- adlib_get(params = query, token = token)
  
  if (fields == "ad_data") {
    if (length(resp$data) > 0) {
      out <- as_tibble(resp, type = "ad")
    } else {
      out <- NULL
    }
  } else if (fields == "demographic_data") {
    ## later, perform
    ## unnest(out, cols = c(demographic_distribution))
    out <- parse_response(resp, list_cols = "demographic_distribution")
  } else if (fields == "region_data") {
    out <- parse_response(resp, list_cols = "region_distribution")
  }
  
  ## Return all query, response object itself, and tibble
  return(list(query = query, resp = resp, tbl = out))
}


