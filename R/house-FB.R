# Setup ========================================================================
source(here::here("R", "house.R"))
token <- readline()

# Deduplicate ==================================================================
house_list <- cand_house_2022 %>%
  filter(!is.na(facebook_ID)) %>%
  rename(id = facebook_ID) %>%
  rowwise() %>%
  mutate(candidate = paste(first_name, last_name)) %>%
  ungroup() %>%
  group_by(id) %>%
  slice(1) %>%
  group_split() %>%
  `names<-`({
    .
  } %>% map(~ .x$candidate[1]) %>% unlist())

# Run FB Graph API =============================================================
fname1 <- here("data", "raw", "fb", "fb-house-raw-ad-data-2022.Rda")
fname2 <- here("data", "raw", "fb", "fb-house-raw-demo-data-2022.Rda")
fname3 <- here("data", "raw", "fb", "fb-house-raw-region-data-2022.Rda")
if (file.exists(fname1)) {
  load(fname1)
  load(fname2)
  load(fname3)
} else {
  ad_house <- vector("list", length(house_list))
  names(ad_house) <- names(house_list)
  region_house <- demo_house <- ad_house
}

for (i in seq(length(house_list))) {
  idx <- house_list[[i]]$id
  cand <- house_list[[i]]$candidate
  message(paste0("Queued for ", cand, ", ", house_list[[i]]$state_cd, "."))
  Sys.sleep(3)

  if (!grepl("e", tolower(idx))) {
    if (is.null(ad_house[[cand]])) {
      ## Three chunks of preset groups of data
      ad_house[[cand]] <-
        fb_short(id = idx, token = token, fields = "ad_data")
      assert_that(!is.null(ad_house[[cand]]))
      Sys.sleep(3)

      demo_house[[cand]] <-
        fb_short(id = idx, token = token, fields = "demographic_data")
      assert_that(!is.null(demo_house[[cand]]))
      Sys.sleep(3)

      region_house[[cand]] <-
        fb_short(id = idx, token = token, fields = "region_data")
      assert_that(!is.null(region_house[[cand]]))
      Sys.sleep(3)

      print(head(ad_house[[cand]]$tbl))
    }
  }

  save(ad_house, file = fname1)
  save(demo_house, file = fname2)
  save(region_house, file = fname3)

  message(paste0("Finished for ", cand, ", ", house_list[[i]]$state_cd, "."))
  message(paste0("Row ", i, " out of ", length(house_list), "."))
}
