# Get spatial catch data, based on gfdata::get_cpue_spatial
library(here)

get_catch_spatial <- function(species) {
  .q <- readLines(here("NOAA_queries","get-catch-spatial.sql"))
  .q <- inject_filter("AND SP.SPECIES_CODE IN", species, sql_code = .q)
  .d <- run_sql("GFFOS", .q)
  .d$SPECIES_COMMON_NAME[.d$SPECIES_COMMON_NAME == "SPINY DOGFISH"] <-
    toupper("north pacific spiny dogfish") # to match GFBioSQL
  names(.d) <- tolower(names(.d))
  .d$species_common_name <- tolower(.d$species_common_name)
  .d$species_scientific_name <- tolower(.d$species_scientific_name)
  as_tibble(.d)
}

