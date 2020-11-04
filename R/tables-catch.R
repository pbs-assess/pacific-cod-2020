catch.table <- function(dat,
                        dat.disc = NULL, # no longer used
                        area = "NA",
                        cap = "",
                        french = FALSE){
  ## dat is what comes out of data/get-data.R/total.catch.yr.qtr
  ## dat.disc is what comes out of data/get-data.R/total.catch.discards

  j <- dat %>%
    group_by(year) %>%
    summarize(Year = year[1],
      USA = sum(usa_catch),
      `landings` = sum(landed_canada),
      `released at sea` = sum(discarded_canada),
      `total` = sum(landed_canada + discarded_canada),
      `Total catch` = `total` + `USA`) %>%
    select(-year)

  j <- j[!is.na(j$`Total catch`),]

  j <- j[,c("Year", "landings", "released at sea", "total", "USA", "Total catch")]

  j[,-1] <- round(j[,-1], 0)

  # Extrapolate the final year's catch based on the average proportion taken by
  # end of Q2 in the past three years (See 2018 assessment doc, Section 2.2)

    # Use last three years only, not including the last year in the data
    last_year <- dat %>%
      tail(1) %>%
      pull(year)
    yrs <- (last_year - 3):(last_year - 1)
    catch_last3yrs_first2quarters <- dat %>%
      filter(year %in% yrs) %>%
      filter(quarter %in% 1:2) %>%
      group_by(year) %>%
      summarize(total_catch_first2_quarters = sum(total_catch))
    catch_last3yrs_all_quarters <- j %>%
      filter(Year %in% yrs) %>%
      select(Year, `Total catch`) %>%
      rename(year = Year, total_catch = `Total catch`)
    catch_last3yrs <- catch_last3yrs_first2quarters %>%
      left_join(catch_last3yrs_all_quarters, by = "year") %>%
      mutate(proportion = total_catch_first2_quarters / total_catch)
    catch_prop <<- catch_last3yrs$proportion #put in global space
    avg_prop <<- mean(catch_last3yrs$proportion) #put in global space

        j$landings[nrow(j)] <- j$`Total catch`[nrow(j)] / avg_prop
    j$total[nrow(j)] <- j$`Total catch`[nrow(j)] / avg_prop
    j$`Total catch`[nrow(j)] <- j$`Total catch`[nrow(j)] / avg_prop
    j$`released at sea`[nrow(j)] <- 0
    j$USA[nrow(j)] <- 0


  j[,c(2,3,4,5,6)] <- apply(j[,c(2,3,4,5,6)],
                            2,
                            function(x){
                              tmp <- as.numeric(x)
                              f(tmp)
                            })

  colnames(j) <- c(en2fr(colnames(j)[1], translate = french, allow_missing = TRUE),
                  en2fr(colnames(j)[2], translate = french, allow_missing = TRUE, case="lower"),
                  en2fr(colnames(j)[3], translate = french, allow_missing = TRUE, case="lower"),
                  en2fr(colnames(j)[4], translate = french, allow_missing = TRUE, case="lower"),
                  en2fr(colnames(j)[5], translate = french, allow_missing = TRUE),
                  en2fr(colnames(j)[6], translate = french, allow_missing = TRUE))

  #Add Canada to colnames for cols 2-4
  for(k in 2:4){
    colnames(j)[k] <- latex.mlc(c("Canada", colnames(j)[k]))
  }

  colnames(j) <- latex.bold(colnames(j))

  kable(j[4:nrow(j),],
        caption = cap,
        booktabs = TRUE,
        longtable = TRUE,
        linesep = "",
        escape = FALSE, format = "pandoc",
        align = c("l", "r", "r", "r", "r", "r")) %>%
    column_spec(c(2, 4, 5, 6), width = "2cm") %>%
    column_spec(3, width = "4cm") %>%
    kable_styling(latex_options = c("hold_position", "repeat_header"))

}

tac.table <- function(tac,
                      cap = "", french = FALSE){
  ## dat is what comes out of the csv file data/pcod-tac-1996-2018.csv

  names(tac) <- gsub("X", "", names(tac))
  names(tac) <- en2fr(names(tac), translate = french, allow_missing = TRUE)

  #Hardcode the translation for IFMP
  tac[1:13,6] <- "PGIP"

  tac[,c(2,3,4,5)] <- apply(tac[,c(2,3,4,5)],
                            2,
                            function(x){
                              tmp <- as.numeric(x)
                              f(tmp)
                            })
  tac[grep(" *NA", tac[,2]), 2] = "bycatch only"
  tac[grep(" *NA", tac[,3]), 3] = "bycatch only"
  tac[grep(" *NA", tac[,4]), 4] = "bycatch only"

  colnames(tac) <- latex.bold(colnames(tac))

  kable(tac, caption = cap,
        booktabs = TRUE,
        longtable = TRUE,
        linesep = "",
        escape = FALSE, format = "pandoc",
        align = c("l", "r", "r", "r", "r", "l")) %>%
    kable_styling(latex_options = c("hold_position", "repeat_header"))

}
