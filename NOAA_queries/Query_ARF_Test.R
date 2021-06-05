# Query to get spatial CPUE and Catch data for ARF (test)
# June 3, 2021
library(gfplot)
library(gfdata)
library(tidyverse)
library(here)

startyear <- 2007
endyear <- 2020
hexwidth <- 10
minvessels <- 1  # code returns cells with >= minvessels

# Source get-catch-spatial function.
# This is the same function as gfdata::get_cpue_spatial with a modified sql query
source(here("NOAA_queries","get-catch-spatial.R"))
source(here("NOAA_queries","plot-catch-spatial.R"))

# Get spatial catch and CPUE data
if(!file.exists(here("NOAA_queries","spatialCPUE_ARF.rda"))){
  message("Getting spatial cpue data ...")
  spatialCPUE <- gfdata::get_cpue_spatial("Arrowtooth Flounder") %>%
    dplyr::filter(year<=endyear)
  saveRDS(spatialCPUE, here("NOAA_queries","ARF","spatialCPUE_ARF.rda"))
  message("Done ...")
}else{
  spatialCPUE <- readRDS(here("NOAA_queries","ARF","spatialCPUE_ARF.rda"))
}
# glimpse(spatialCPUE)

if(!file.exists(here("NOAA_queries","spatialCatch_ARF.rda"))){
  message("Getting spatial catch data ...")
  spatialCatch <- get_catch_spatial("Arrowtooth Flounder")%>%
    dplyr::filter(year<=endyear)
  saveRDS(spatialCatch, here("NOAA_queries","ARF","spatialCatch_ARF.rda"))
  message("Done ...")
}else{
  spatialCatch <- readRDS(here("NOAA_queries","ARF","spatialCatch_ARF.rda"))
}
# glimpse(spatialCatch)
# unique(spatialCatch$year)

# ~~~~~ get binned data and plot ... All years combined ~~~~~~~~~~~~
# cpue
plotcpueDat <- gfplot::plot_cpue_spatial(spatialCPUE, n_minimum_vessels = minvessels,
                                         bin_width=hexwidth, start_year=startyear,
                                         show_historical=FALSE, return_data = TRUE) %>%
              select(x,y,value) %>%
              rename("Easting"=x, "Northing"=y, "Mean CPUE (kg/h)"=value)

glimpse(plotcpueDat)
write_csv(plotcpueDat, here("NOAA_queries","ARF","csvs",paste0("spatialCPUE_ARF_",startyear,"-",endyear,".csv")))

p <- gfplot::plot_cpue_spatial(spatialCPUE, n_minimum_vessels = minvessels,
                               bin_width=hexwidth, start_year=startyear,
                               show_historical=FALSE, return_data = FALSE,
                               fill_lab = paste(startyear,"-",endyear, "Mean CPUE (kg/h)"))
p
ggsave(here("NOAA_queries","ARF","figs",paste0("spatialCPUE_ARF_",startyear,"-",endyear,".png")))

# catch
plotcatchDat <- plot_catch_spatial(spatialCatch, n_minimum_vessels = minvessels,
                                   bin_width=hexwidth, start_year=startyear,
                                   show_historical=FALSE, return_data = TRUE) %>%
                select(x,y,value) %>%
                rename("Easting"=x, "Northing"=y, "Total Catch (t)"=value)

glimpse(plotcatchDat)
write_csv(plotcatchDat, here("NOAA_queries","ARF","csvs",paste0("spatialCatch_ARF_",startyear,"-",endyear,".csv")))

p <- plot_catch_spatial(spatialCatch, n_minimum_vessels = minvessels,
                        bin_width=hexwidth, start_year=startyear,
                        show_historical=FALSE, return_data = FALSE,
                        fill_lab = paste(startyear,"-",endyear, "Total Catch (t)"))
p
ggsave(here("NOAA_queries","ARF","figs",paste0("spatialCatch_ARF_",startyear,"-",endyear,".png")))

# ~~~~~ Make annual plots and csvs (Catch only) ~~~~~~~~~~~~
years <- startyear:endyear
nyears <- length(years)

for(i in 1:nyears){
  message(paste("Plotting catch for year",years[i], "..."))

  dat <- spatialCatch %>%
    dplyr::filter(year==years[i])
  message("nrows=", nrow(dat))

  plotDat <- plot_catch_spatial(dat, n_minimum_vessels = minvessels,
                                     bin_width=hexwidth, start_year=years[i],
                                     show_historical=FALSE, return_data = TRUE) %>%
    mutate(year=years[i]) %>%
    select(year,x,y,value) %>%
    rename("Easting"=x, "Northing"=y, "Total Catch (t)"=value)

  totalCatch <- c(years[i], sum(plotDat$`Total Catch (t)`))

  p <- plot_catch_spatial(dat, n_minimum_vessels = minvessels,
                          bin_width=hexwidth, start_year=years[i],
                          show_historical=FALSE, return_data = FALSE,
                          fill_lab = paste(years[i], "Catch (t)"))
  ggsave(here("NOAA_queries","ARF","figs",paste0("spatialcatch_ARF_",years[i], ".png")))

  if(i==1){
    plotDatByYear <- plotDat
    totalCatchByYear <- totalCatch
  }else{
    plotDatByYear <- rbind(plotDatByYear,plotDat)
    totalCatchByYear <- rbind(totalCatchByYear,totalCatch)
  }


}
totalCatchByYear <- as.data.frame(totalCatchByYear[,1:2])
colnames(totalCatchByYear) <- c("Year", "Total Catch (t)")

write_csv(plotDatByYear, here("NOAA_queries","ARF","csvs",paste0("spatialcatch_ARF_by_year.csv")))
write.csv(totalCatchByYear, here("NOAA_queries","ARF","csvs",paste0("totalcatch_ARF_by_year.csv")), row.names = F)
