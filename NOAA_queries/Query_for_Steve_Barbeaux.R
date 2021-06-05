# Query to get spatial CPUE and Catch data for P cod Steve Barbeaux (NOAA)
# June 3, 2021
library(gfplot)
library(gfdata)
library(tidyverse)
library(here)

startyear <- 2010
endyear <- 2020
hexwidth <- 10
minvessels <- 3  # code returns cells with >= minvessels

# Source get-catch-spatial function.
# This is the same function as gfdata::get_cpue_spatial with a modified sql query
source(here("NOAA_queries","get-catch-spatial.R"))
source(here("NOAA_queries","plot-catch-spatial.R"))

# Get spatial catch and CPUE data
if(!file.exists(here("NOAA_queries","spatialCPUE_pcod.rda"))){
  message("Getting spatial cpue data ...")
  spatialCPUE <- gfdata::get_cpue_spatial("Pacific cod") %>%
    dplyr::filter(year<=endyear)
  saveRDS(spatialCPUE, here("NOAA_queries","spatialCPUE_pcod.rda"))
  message("Done ...")
}else{
  spatialCPUE <- readRDS(here("NOAA_queries","spatialCPUE_pcod.rda"))
}
# glimpse(spatialCPUE)

if(!file.exists(here("NOAA_queries","spatialCatch_pcod.rda"))){
  message("Getting spatial catch data ...")
  spatialCatch <- get_catch_spatial("Pacific cod")%>%
    dplyr::filter(year<=endyear)
  saveRDS(spatialCatch, here("NOAA_queries","spatialCatch_pcod.rda"))
  message("Done ...")
}else{
  spatialCatch <- readRDS(here("NOAA_queries","spatialCatch_pcod.rda"))
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
write_csv(plotcpueDat, here("NOAA_queries","csvs",paste0("spatialCPUE_pcod_",startyear,"-",endyear,".csv")))

p <- gfplot::plot_cpue_spatial(spatialCPUE, n_minimum_vessels = minvessels,
                               bin_width=hexwidth, start_year=startyear,
                               show_historical=FALSE, return_data = FALSE,
                               fill_lab = paste(startyear,"-",endyear, "Mean CPUE (kg/h)"))
p
ggsave(here("NOAA_queries","figs",paste0("spatialCPUE_pcod_",startyear,"-",endyear,".png")))

# catch
plotcatchDat <- plot_catch_spatial(spatialCatch, n_minimum_vessels = minvessels,
                                   bin_width=hexwidth, start_year=startyear,
                                   show_historical=FALSE, return_data = TRUE) %>%
                select(x,y,value) %>%
                rename("Easting"=x, "Northing"=y, "Total Catch (t)"=value)

glimpse(plotcatchDat)
write_csv(plotcatchDat, here("NOAA_queries","csvs",paste0("spatialCatch_pcod_",startyear,"-",endyear,".csv")))

p <- plot_catch_spatial(spatialCatch, n_minimum_vessels = minvessels,
                        bin_width=hexwidth, start_year=startyear,
                        show_historical=FALSE, return_data = FALSE,
                        fill_lab = paste(startyear,"-",endyear, "Total Catch (t)"))
p
ggsave(here("NOAA_queries","figs",paste0("spatialCatch_pcod_",startyear,"-",endyear,".png")))

# Total catch (test)
totalCatch_t <- sum(plotcatchDat$`Total Catch (t)`)
totalCatch_t

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
  ggsave(here("NOAA_queries","figs",paste0("spatialcatch_pcod_",years[i], ".png")))

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

write_csv(plotDatByYear, here("NOAA_queries","csvs",paste0("spatialCatch_pcod_by_year.csv")))
write.csv(totalCatchByYear, here("NOAA_queries","csvs",paste0("totalCatch_pcod_by_year.csv")), row.names = F)


# ~~~~~ Make annual plots and csvs (CPUE only) ~~~~~~~~~~~~
for(i in 1:nyears){
  message(paste("Plotting cpue for year",years[i], "..."))

  dat <- spatialCPUE %>%
    dplyr::filter(year==years[i])
  message("nrows=", nrow(dat))

  plotDatCPUE <- gfplot::plot_cpue_spatial(dat, n_minimum_vessels = minvessels,
                                bin_width=hexwidth, start_year=years[i],
                                show_historical=FALSE, return_data = TRUE) %>%
    mutate(year=years[i]) %>%
    select(year,x,y,value) %>%
    rename("Easting"=x, "Northing"=y, "Mean CPUE (kg/h)"=value)

  meanCPUE <- c(years[i], mean(plotDatCPUE$`Mean CPUE (kg/h)`))

  p <- gfplot::plot_cpue_spatial(dat, n_minimum_vessels = minvessels,
                          bin_width=hexwidth, start_year=years[i],
                          show_historical=FALSE, return_data = FALSE,
                          fill_lab = paste(years[i], "CPUE (kg/h)"))
  ggsave(here("NOAA_queries","figs",paste0("spatialcpue_pcod_",years[i], ".png")))

  if(i==1){
    plotCPUEDatByYear <- plotDatCPUE
    meanCPUEByYear <- meanCPUE
  }else{
    plotCPUEDatByYear <- rbind(plotCPUEDatByYear,plotDatCPUE)
    meanCPUEByYear <- rbind(meanCPUEByYear,meanCPUE)
  }
}
meanCPUEByYear <- as.data.frame(meanCPUEByYear[,1:2])
colnames(meanCPUEByYear) <- c("Year", "CPUE (kg/h)")

write_csv(plotCPUEDatByYear, here("NOAA_queries","csvs",paste0("spatialCPUE_pcod_by_year.csv")))
write.csv(meanCPUEByYear, here("NOAA_queries","csvs",paste0("meanCPUE_pcod_by_year.csv")), row.names = F)

