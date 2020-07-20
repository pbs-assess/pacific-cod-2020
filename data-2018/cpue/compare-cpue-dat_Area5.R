#Code by Sean Anderson and Robyn Forrest.
#Original in C:\GitHub\pcod-scenarios-2018\data\read-cpue-dat.r
#see also C:\GitHub\pcod-scenarios-2018\data\read-cpue-dat.Rmd
#Modified by Robyn Forrest June 27 2018
# graphics.off()
# rm(list=ls(all=TRUE))
library(gfplot)
library(tidyverse)
library(readr)
# library(rstan)
library(lubridate)
# library(psych)
library(reshape2)

geometric.mean <- psych::geometric.mean

setwd("C:/pbs-pcod/0_15-2018_Assessment/Data/CPUE_MeanWeight_CombineSurveys/")
outDir <- "C:/pbs-pcod/0_15-2018_Assessment/Data/CPUE_MeanWeight_CombineSurveys/Results/"

#get csv files for comparing outputs
Starr2013 <- read_csv("Starr_2013_CPUE.csv")
Anderson201804 <- read_csv("CPUE_April2018_KeyLocal.csv") #Earlier extraction by Sean Anderson
#Paul Starr codes for settings
StarrCodes <- c("AA","AB","AC","AD","GA","GD")
StarrLabels <- c("Arith All Eff","Arith Pos Eff1","Arith Pos Eff2","Arith Key Local","Geom All Eff","Geom Key Local")

useLocal <- TRUE #set to TRUE if you want to work offline and have recently pulled the data from the network

source("get-data.R")
if(useLocal==F) extract.data(species = "pacific cod",cache.dir = "pcod-cache",unsorted_only = FALSE)

load.data(cache.dir = "pcod-cache") #load data function is in get-data.r. It loads the pcod-cpue.rds file

d <- d_cpue

names(d) <- tolower(names(d))
d <- rename(d, total = totcatch_kg, minor_stat_area_code = min)
d$hours_fished <- as.numeric(as.character(d$hours_fished))
d$database_name <- tolower(d$database_name)
d$gear <- tolower(d$gear)
d$locality_description <- tolower(d$locality_description)

areas <- c("3[CD]+", "5[AB]+", "5[CD]+")
d$area <- NA
for (i in seq_along(areas)) {
  d[grepl(areas[[i]], d$major_stat_area_description), "area"] <-
    gsub("\\[|\\]|\\+", "", areas[[i]])
}
specific_areas <- c("3C", "3D", "5A", "5B", "5C", "5D")
d$specific_area <- NA
for (i in seq_along(specific_areas)) {
  d[grepl(specific_areas[[i]], d$major_stat_area_description), "specific_area"] <-
    specific_areas[[i]]
}

#Get the locality codes
#Paul Starr's locality codes
keyLocality <- c("cape scott spit","mexicana","topknot","ne goose","se goose","nw goose","sw goose", "reef island","west horseshoe","east horseshoe","two peaks","butterworth","white rocks","shell ground")
x <- d %>%
  subset(select=c(specific_area,locality_code,locality_description)) %>%
  distinct()

x <- x[with(x, order(x$specific_area,x$locality_code)),]
Starr_key_locality <- is.element(x$locality_description,keyLocality)
x <- cbind(x,Starr_key_locality)
write_csv(x,paste0(outDir,"Locality_Codes.csv"))

#Robyn re-wrote this (see object x above for key)
dsum <- d %>%
  mutate(key_locality =
           specific_area == "5A" & locality_description %in% c("cape scott spit", "mexicana","topknot") |
           specific_area == "5B" & locality_description %in% c("ne goose","se goose","nw goose","sw goose") |
           specific_area == "5C" & locality_description %in% c("reef island","west horseshoe","east horseshoe") |
           specific_area == "5D" & locality_description %in% c("two peaks","butterworth","white rocks","shell ground")
  ) %>%
  ## filter(hours_fished < 2000) %>%
  filter(species_code == "222") %>%
  filter(!is.na(hours_fished), !is.na(total), total > 0, hours_fished > 0)

#Deprecated. NOT WORKING WELL FOR 5CD
#if(FALSE){
#  dsum <- d %>%
 #   mutate(key_locality =
#      specific_area == "5A" & locality_code %in% c(1, 2,3,4) |
#      specific_area == "5B" & locality_code %in% c(1, 2, 3, 4) |
#      specific_area == "5C" & minor_stat_area_code == 2 & locality_code %in% c(1, 2, 3) |
#      specific_area == "5C" & minor_stat_area_code == 6 & locality_code %in% c(1, 2, 10) |
#      specific_area == "5D" & minor_stat_area_code == 1 & locality_code %in% c(5) |
#      specific_area == "5D" & minor_stat_area_code == 4 & locality_code %in% c(1, 2) |
#      specific_area == "5D" & minor_stat_area_code == 5 & locality_code %in% c(1, 3)
#    ) %>%
    ## filter(hours_fished < 2000) %>%
#    filter(species_code == "222") %>%
#   filter(!is.na(hours_fished), !is.na(total), total > 0, hours_fished > 0)
    # filter(!is.na(hours_fished), !is.na(total), hours_fished > 0) %>%
    # filter(key_locality) %>%
    # filter(best_depth_m > 0, best_depth_m < 150) %>%
#}

#Ratio estimator 1: sum of(catch / sum of effort by area and fyear  (FOLLY)
#Ratio estimator 2: mean(catchi/efforti)

keylocal <- dsum %>%
    filter(key_locality) %>%
    group_by(area, fyear) %>%
    summarise(
      catch = sum(total, na.rm = TRUE),
      sum_hours_fished = sum(hours_fished, na.rm = TRUE),
      arith_cpue = sum(total, na.rm = TRUE) / sum(hours_fished, na.rm = TRUE),
      geo_cpue = exp(mean(log(total / hours_fished), na.rm = TRUE))
    )

nonkeylocal <- dsum %>%
    group_by(area, fyear) %>%
    summarise(
      catch = sum(total, na.rm = TRUE),
      sum_hours_fished = sum(hours_fished, na.rm = TRUE),
      arith_cpue = sum(total, na.rm = TRUE) / sum(hours_fished, na.rm = TRUE),
      geo_cpue = exp(mean(log(total / hours_fished), na.rm = TRUE))
    )

write_csv(as.data.frame(dsum),paste0(outDir, "pcod-cpue-data.csv"))
write_csv(as.data.frame(keylocal),paste0(outDir, "pcod-cpue-trends-keylocalities.csv"))
write_csv(as.data.frame(nonkeylocal),paste0(outDir, "pcod-cpue-trends-nonkeylocalities.csv"))

#Non-key locations
#dsum <- dsum %>%
#  mutate(arith_cpue = arith_cpue / exp(mean(log(arith_cpue)))) %>%
#  mutate(geo_cpue = geo_cpue / exp(mean(log(geo_cpue))))

################################################################
#COMPARISONS
#Plot Starr Data - see Appendix B of 2013 P cod assessment

####################################################################################################################
#5AB
####################################################################################################################
#New CPUE does not have full sequence of years before 1960 for 5AB
df <- filter(Starr2013,Area=="5AB", !is.element(Analysis,c("GA","GD")), is.element(FYear,1965:2012))
#get relative CPUE
#1. get geometric mean for each arithmentic analysis type (AA,AB,AC or AD), for standardisation to match Paul's plots
meancpue <- df %>%
  group_by(Analysis) %>%
  summarise(gmean = geometric.mean(CPUE))

#2. Now standardise each time series by geometric mean and bind back to data
for(i in 1:4){
  An <- StarrCodes[i]
  dfa <- filter(df,Analysis==An)
  x <- dfa$CPUE/meancpue$gmean[i]
  if(i==1) relativeCPUE <- x
  if(i>1) relativeCPUE <- c(relativeCPUE,x)
}
#print(geometric.mean(relativeCPUE)) - should = 1
df <- cbind(df, relativeCPUE)

#3. Now add new key locality CPUE calcs (see above), and those done in April, to Paul's
dfjuly <- filter(keylocal, is.element(fyear,1965:2012),area=="5AB")
dfapril <- filter(Anderson201804, is.element(fyear,1965:2012),area=="5AB")

#make a new dataframe (dfnew) based on dfjuly to put comparisons
dfnew <- dfjuly %>%
  select(-c(geo_cpue, catch, sum_hours_fished)) %>%
  rename(arith_CPUE_july = arith_cpue)

#Append other CPUE series (Anderson's April calcs and Starr 2013) to dfnew
dfnew$arith_CPUE_april = dfapril$arith_cpue
dfnew$arith_CPUE_Starr = df$CPUE[which(df$Analysis=="AD")]
dfnew$relative_arith_CPUE_july = dfjuly$arith_cpue/geometric.mean(dfjuly$arith_cpue)
dfnew$relative_arith_CPUE_april = dfapril$arith_cpue/geometric.mean(dfapril$arith_cpue)
dfnew$relative_arith_CPUE_Starr = df$relativeCPUE[which(df$Analysis=="AD")]

#reshape the data for easier plotting -- this is cool!
dfnew2 <- dfnew %>%
  subset(select=c(fyear, relative_arith_CPUE_Starr,relative_arith_CPUE_july,relative_arith_CPUE_april)) %>%
  melt(id=c("fyear"))

dfnew3 <- dfnew %>%
  subset(select=c(fyear, arith_CPUE_Starr,arith_CPUE_july,arith_CPUE_april)) %>%
  melt(id=c("fyear"))

write_csv(as.data.frame(dfnew),paste0(outDir, "pcod-All_cpue-keylocalities-5AB.csv")) #pre-melted version
write_csv(as.data.frame(dfnew2),paste0(outDir, "pcod-relativecpue-keylocalities-5AB.csv"))
write_csv(as.data.frame(dfnew3),paste0(outDir, "pcod-cpue-keylocalities-5AB.csv"))

#Plot Starr data on its own first. Arithmetic means standardised to geometric mean
#Absolute CPUE
ggplot(data=df, aes(x=FYear,y=CPUE,Group=Analysis, colour=Analysis)) +
  geom_line(lwd=1) +
  scale_colour_manual(breaks = c("AA","AB","AC","AD"),
                    values=c("red","blue","darkgreen","black"),
                    labels=c("Arithmetic A (all effort)", "Arithmetic B (+ve effort 1)", "Arithmetic C (+ve effort 2)", "Arithmetic D (key areas)"))+
  theme(plot.title=element_text(size=14,face="bold",hjust=0.5),
        axis.text=element_text(size=14),
        axis.title=element_text(size=14,face="bold")) +
  labs(x= "Fishing Year", y = "CPUE (Kg/h)", title="Area 5AB")
ggsave(file.path(outDir, "pcod-Starr-CPUE_5AB_Arithmetic.png"), width = 8, height = 3)

#Relative CPUE
ggplot(data=df, aes(x=FYear,y=relativeCPUE,Group=Analysis, colour=Analysis)) +
  geom_line(lwd=1) +
  scale_colour_manual(breaks = c("AA","AB","AC","AD"),
                      values=c("red","blue","darkgreen","black"),
                      labels=c("Arithmetic A (all effort)", "Arithmetic B (+ve effort 1)", "Arithmetic C (+ve effort 2)", "Arithmetic D (key areas)"))+
  theme(plot.title=element_text(size=14,face="bold",hjust=0.5),
        axis.text=element_text(size=14),
        axis.title=element_text(size=14,face="bold")) +
  labs(x= "Fishing Year", y = "Relative CPUE (Kg/h)", title="Area 5AB")
ggsave(file.path(outDir, "pcod-Starr-RelativeCPUE_5AB_Arithmetic.png"), width = 8, height = 3)

#Compare Starr and new
#Relative
ggplot(data=dfnew2, aes(x=fyear,y=value,Group=variable, colour=variable)) +
  geom_line(lwd=0.5, aes(linetype=variable)) +
  scale_colour_manual(breaks = c("relative_arith_CPUE_Starr","relative_arith_CPUE_april","relative_arith_CPUE_july"),
                      values=c("blue","red","black"),
                      labels=c("Arithmetic, Key areas: Starr","Arithmetic, Key areas: April","Arithmetic, Key areas: July"))+
  theme(plot.title=element_text(size=14,face="bold",hjust=0.5),
        axis.text=element_text(size=14),
        axis.title=element_text(size=14,face="bold")) +
  labs(x= "Fishing Year", y = "Relative CPUE (Kg/h)", title="Area 5AB")
ggsave(file.path(outDir, "pcod-Compare-RelativeCPUE_5AB_Arithmetic.png"), width = 8, height = 3)

#absolute
ggplot(data=dfnew3, aes(x=fyear,y=value,Group=variable, colour=variable)) +
  geom_line(lwd=0.5, aes(linetype=variable)) +
  scale_colour_manual(breaks = c("arith_CPUE_Starr","arith_CPUE_april","arith_CPUE_july"),
                      values=c("blue","red","black"),
                      labels=c("Arithmetic, Key areas: Starr","Arithmetic, Key areas: April","Arithmetic, Key areas: July"))+
  theme(plot.title=element_text(size=14,face="bold",hjust=0.5),
        axis.text=element_text(size=14),
        axis.title=element_text(size=14,face="bold")) +
  labs(x= "Fishing Year", y = "CPUE (Kg/h)", title="Area 5AB")
ggsave(file.path(outDir, "pcod-Compare-CPUE_5AB_Arithmetic.png"), width = 8, height = 3)

####################################################################################################################
#5CD
####################################################################################################################
df <- filter(Starr2013,Area=="5CD", !is.element(Analysis,c("GA","GD")))
#get relative CPUE
#get relative CPUE
#1. get geometric mean for each arithmentic analysis type (AA,AB,AC or AD), for standardisation to match Paul's plots
meancpue <- df %>%
  group_by(Analysis) %>%
  summarise(gmean = geometric.mean(CPUE))
#2. Now standardise each time series by geometric mean and bind back to data
for(i in 1:4){
  An <- StarrCodes[i]
  dfa <- filter(df,Analysis==An)
  x <- dfa$CPUE/meancpue$gmean[i]
  if(i==1) relativeCPUE <- x
  if(i>1) relativeCPUE <- c(relativeCPUE,x)
}
df <- cbind(df, relativeCPUE)

#3. Now add new key locality CPUE calcs (see above) to Paul's
dfjuly <- filter(keylocal, is.element(fyear,1956:2012),area=="5CD")
dfapril <- filter(Anderson201804, is.element(fyear,1956:2012),area=="5CD")

#make a new dataframe (dfnew) based on dfjuly to put comparisons
dfnew <- dfjuly %>%
  select(-c(geo_cpue, catch, sum_hours_fished)) %>%
  rename(arith_CPUE_july = arith_cpue)

#Append other CPUE series (Anderson's April calcs and Starr 2013) to dfnew
dfnew$arith_CPUE_april = dfapril$arith_cpue
dfnew$arith_CPUE_Starr = df$CPUE[which(df$Analysis=="AD")]
dfnew$relative_arith_CPUE_july = dfjuly$arith_cpue/geometric.mean(dfjuly$arith_cpue)
dfnew$relative_arith_CPUE_april = dfapril$arith_cpue/geometric.mean(dfapril$arith_cpue)
dfnew$relative_arith_CPUE_Starr = df$relativeCPUE[which(df$Analysis=="AD")]

#reshape the data for easier plotting -- this is cool!
dfnew2 <- dfnew %>%
  subset(select=c(fyear, relative_arith_CPUE_Starr,relative_arith_CPUE_july,relative_arith_CPUE_april)) %>%
  melt(id=c("fyear"))

dfnew3 <- dfnew %>%
  subset(select=c(fyear, arith_CPUE_Starr,arith_CPUE_july,arith_CPUE_april)) %>%
  melt(id=c("fyear"))

write_csv(as.data.frame(dfnew),paste0(outDir, "pcod-All_cpue-keylocalities-5CD.csv")) #pre-melted version
write_csv(as.data.frame(dfnew2),paste0(outDir, "pcod-relativecpue-keylocalities-5CD.csv"))
write_csv(as.data.frame(dfnew3),paste0(outDir, "pcod-cpue-keylocalities-5CD.csv"))

ggplot(data=df, aes(x=FYear,y=CPUE,Group=Analysis, colour=Analysis)) +
  geom_line(lwd=1) +
  scale_colour_manual(breaks = c("AA","AB","AC","AD"),
                      values=c("red","blue","darkgreen","black"),
                      labels=c("Arithmetic A (all effort)", "Arithmetic B (+ve effort 1)", "Arithmetic C (+ve effort 2)", "Arithmetic D (key areas)"))+
  theme(plot.title=element_text(size=14,face="bold",hjust=0.5),
        axis.text=element_text(size=14),
        axis.title=element_text(size=14,face="bold")) +
  labs(x= "Fishing Year", y = "CPUE (Kg/h)", title="Area 5CD")
ggsave(file.path(outDir, "pcod-Starr-CPUE_5CD_Arithmetic.png"), width = 8, height = 3)

ggplot(data=df, aes(x=FYear,y=relativeCPUE,Group=Analysis, colour=Analysis)) +
  geom_line(lwd=1) +
  scale_colour_manual(breaks = c("AA","AB","AC","AD"),
                      values=c("red","blue","darkgreen","black"),
                      labels=c("Arithmetic A (all effort)", "Arithmetic B (+ve effort 1)", "Arithmetic C (+ve effort 2)", "Arithmetic D (key areas)"))+
  theme(plot.title=element_text(size=14,face="bold",hjust=0.5),
        axis.text=element_text(size=14),
        axis.title=element_text(size=14,face="bold")) +
  labs(x= "Fishing Year", y = "Relative CPUE (Kg/h)", title="Area 5CD")
ggsave(file.path(outDir, "pcod-Starr-RelativeCPUE_5CD_Arithmetic.png"), width = 8, height = 3)

#Compare Starr and new
#Relative
ggplot(data=dfnew2, aes(x=fyear,y=value,Group=variable, colour=variable)) +
  geom_line(lwd=0.5, aes(linetype=variable)) +
  scale_colour_manual(breaks = c("relative_arith_CPUE_Starr","relative_arith_CPUE_april","relative_arith_CPUE_july"),
                      values=c("blue","red","black"),
                      labels=c("Arithmetic, Key areas: Starr","Arithmetic, Key areas: April","Arithmetic, Key areas: July"))+
  theme(plot.title=element_text(size=14,face="bold",hjust=0.5),
        axis.text=element_text(size=14),
        axis.title=element_text(size=14,face="bold")) +
  labs(x= "Fishing Year", y = "CPUE (Kg/h)", title="Area 5CD")
ggsave(file.path(outDir, "pcod-Compare-RelativeCPUE_5CD_Arithmetic.png"), width = 8, height = 3)
#Absolute
ggplot(data=dfnew3, aes(x=fyear,y=value,Group=variable, colour=variable)) +
  geom_line(lwd=0.5, aes(linetype=variable)) +
  scale_colour_manual(breaks = c("arith_CPUE_Starr","arith_CPUE_april","arith_CPUE_july"),
                      values=c("blue","red","black"),
                      labels=c("Arithmetic, Key areas: Starr","Arithmetic, Key areas: April","Arithmetic, Key areas: July"))+
  theme(plot.title=element_text(size=14,face="bold",hjust=0.5),
        axis.text=element_text(size=14),
        axis.title=element_text(size=14,face="bold")) +
  labs(x= "Fishing Year", y = "CPUE (Kg/h)", title="Area 5CD")
ggsave(file.path(outDir, "pcod-Compare-CPUE_5CD_Arithmetic.png"), width = 8, height = 3)

#######################################################################################################################
#plot July 2018 analyses
#######################################################################################################################
ggplot(nonkeylocal, aes(fyear, arith_cpue)) +
  geom_line(lty = 2) +
  geom_line(aes(y = geo_cpue), lty = 1) +
  geom_vline(xintercept = 1996, lty = 3) +
  facet_wrap(~area, scales = "free_y")
ggsave(file.path(outDir, "pcod-cpue-trends-nonkeylocalities.png"), width = 8, height = 3)

ggplot(nonkeylocal, aes(fyear, catch)) +
  geom_line(lty = 1) +
  geom_vline(xintercept = 1996, lty = 3) +
  facet_wrap(~area, scales = "free_y")
ggsave(file.path(outDir, "pcod-catch-trends-nonkeylocalities.png"), width = 8, height = 3)

ggplot(nonkeylocal, aes(fyear, sum_hours_fished)) +
  geom_line(lty = 1) +
  geom_vline(xintercept = 1996, lty = 3) +
  facet_wrap(~area, scales = "free_y")
ggsave(file.path(outDir, "pcod-effort-trends-nonkeylocalities.png"), width = 8, height = 3)

#Key locations
ggplot(keylocal, aes(fyear, arith_cpue)) +
  geom_line(lty = 2) +
  geom_line(aes(y = geo_cpue), lty = 1) +
  geom_vline(xintercept = 1996, lty = 3) +
  facet_wrap(~area, scales = "free_y")
ggsave(file.path(outDir, "pcod-cpue-trends-keylocalities.png"), width = 8, height = 3)

ggplot(keylocal, aes(fyear, catch)) +
  geom_line(lty = 1) +
  geom_vline(xintercept = 1996, lty = 3) +
  facet_wrap(~area, scales = "free_y")
ggsave(file.path(outDir, "pcod-catch-trends-keylocalities.png"), width = 8, height = 3)

ggplot(keylocal, aes(fyear, sum_hours_fished)) +
  geom_line(lty = 1) +
  geom_vline(xintercept = 1996, lty = 3) +
  facet_wrap(~area, scales = "free_y")
ggsave(file.path(outDir, "pcod-effort-trends-keylocalities.png"), width = 8, height = 3)

#Alternative plot for Starr analyses (like Appendix B)
Starr2013_5AB <- read_csv("Starr_2013_CPUE_5AB.csv") #Paul Starr's analysis from 2013 assessment (5AB and 5CD only)
Starr2013_5CD <- read_csv("Starr_2013_CPUE_5CD.csv") #Paul Starr's analysis from 2013 assessment (5AB and 5CD only)

if(FALSE){
  ggplot() +
    geom_point(data=Starr2013_5AB, aes(FYear,AA), colour="red", cex=1.5, pch=19) +
    geom_point(data=Starr2013_5AB, aes(FYear,AB), colour="blue", cex=1.5, pch=17) +
    geom_point(data=Starr2013_5AB, aes(FYear,AC), colour="darkgreen", cex=1.5, pch=15) +
    geom_line(data=Starr2013_5AB, aes(FYear,AD), colour="black", lwd=1) +
    theme(axis.text=element_text(size=16),axis.title=element_text(size=16,face="bold")) +
    labs(x= "Fishing Year", y = "CPUE (Kg/h)") +
    scale_shape_discrete(name  ="Effort Measure",
                         labels=c("Arithmetic A (all effort)", "Arithmetic B (+ve effort 1)", "Arithmetic C (+ve effort 2)", "Arithmetic D (key areas)"))
  ggsave(file.path(outDir, "pcod-Starr-CPUE_5AB_Arithmetic1.png"), width = 8, height = 3)
}
