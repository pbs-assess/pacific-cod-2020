# Get survey and biomass time series
# For Steve Barbeaux (NOAA)
# Robyn Forrest. June 4 2020
message("STOP! Have you run the chunks in report/00-load.Rmd?")

# !! 1. Run all chunks in report/00-load.Rmd !!
# This gets the objects avg.model.3cd and avg.model.5abcd

# Now run this code
library(here)
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
b3cd <- b.csv(avg.model.3cd,
            "Model average",
            depl = FALSE,
            french=french)
glimpse(b3cd)

b5abcd <- b.csv(avg.model.5abcd,
              "Model average",
              depl = FALSE,
              french=french)
glimpse(b5abcd)

# Check
p <- b3cd %>%
  ggplot() +
  geom_ribbon(aes(x=Year, ymin=lowercv, ymax=uppercv), color="lightgray", alpha=0.5)+
  geom_line(aes(x=Year, y=median), lwd=2)
p

p <- b5abcd %>%
  ggplot() +
  geom_ribbon(aes(x=Year, ymin=lowercv, ymax=uppercv), color="lightgray", alpha=0.5)+
  geom_line(aes(x=Year, y=median), lwd=2)
p
