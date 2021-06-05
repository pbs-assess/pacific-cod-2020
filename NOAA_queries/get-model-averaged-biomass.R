# Get survey and biomass time series

# !! 1. Run all chunks in report/00-load.Rmd !!
# This gets the objects avg.model.3cd and avg.model.5abcd

# Now run this code

source(here("NOAA_queries","get-model-averaged-biomass-func.R"))

# Plot the biomass
p <- b.plot(avg.model.3cd,
       base.model.3cd.name,
       depl = FALSE,
       add.hist.ref = FALSE,
       lrp = c(1986, 1986),
       usr = c(1956, 2004), french=french)
ggsave(here("NOAA_queries","figs","Model_averaged_biomass_pcod_3CD.png"))

p <- b.plot(avg.model.5abcd,
       base.model.3cd.name,
       depl = FALSE,
       add.hist.ref = FALSE,
       lrp = c(2000, 2000),
       usr = c(1956, 2004), french=french)
ggsave(here("NOAA_queries","figs","Model_averaged_biomass_pcod_5ABCD.png"))

# Just get the numbers
b <- b.plot(avg.model.3cd,
            base.model.3cd.name,
            depl = FALSE,
            add.hist.ref = FALSE,
            french=french)
glimpse(b)
