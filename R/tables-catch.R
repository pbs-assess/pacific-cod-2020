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

  # Extrapolate the final year's catch. Inflate the final year catch (Q1 and 2 only), based
  # on the average proportion taken by end of Q2 in the past three years.
  # two different ways depending on area (See 2018 assessment doc, Section 2.2)

    if(area == "5ABCD") {
    # First need to remove Q3 data from 2018 catch so that numbers match iscam file
      # At the time we only had data up to the end of Q2 (Sep 30)
      # This is a one-time hack because we are retroactively adding
      # the extrapolation to the table
      j18 <- dat %>%
        dplyr::filter(year==2018, quarter<3) %>%
        summarize(Year = year[1],
           USA = sum(usa_catch),
          `landings` = sum(landed_canada),
          `released at sea` = sum(discarded_canada),
          `total` = sum(landed_canada + discarded_canada),
          `Total catch` = `total` + `USA`)
      j18 <- j18[,c("Year", "landings", "released at sea", "total", "USA", "Total catch")]
      j18[,-1] <- round(j18[,-1], 0)
      #Replace last row of catch table
      j[nrow(j),]  <- j18

    # Now do the extrapolation based on the average proportion taken in the first 2 quarters
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
    avg_prop <- mean(catch_last3yrs$proportion)

    # Sadly, we need to hardwire the number this time, so the number here matches the iscam files
    # There must have been more catch added to Q2
    # after we made the iscam data files. The date on the pcod-cache/pacific-cod.rds file
    # is Oct 25 2018 so we would have pulled it again.
    # BUT this is the way to do it going forward!

    # j$landings[nrow(j)] <- j$`Total catch`[nrow(j)] / avg_prop
    # j$total[nrow(j)] <- j$`Total catch`[nrow(j)] / avg_prop
    # j$`Total catch`[nrow(j)] <- j$`Total catch`[nrow(j)] / avg_prop
    # j$`released at sea`[nrow(j)] <- 0
    # j$USA[nrow(j)] <- 0

    j$landings[nrow(j)] <- 230
    j$total[nrow(j)] <- 230
    j$`Total catch`[nrow(j)] <- 230
    j$`released at sea`[nrow(j)] <- 0
    j$USA[nrow(j)] <- 0
  }else if(area == "3CD") {
    # Use the last year's values
    j$landings[nrow(j)] <- j$`Total catch`[nrow(j) - 1]
    j$total[nrow(j)] <- j$`Total catch`[nrow(j) - 1]
    j$`Total catch`[nrow(j)] <- j$`Total catch`[nrow(j) - 1]
    j$`released at sea`[nrow(j)] <- 0
    j$USA[nrow(j)] <- 0
  }

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


  if (french) {
    for (i in seq_len(ncol(j))) {
      j[,i] <- gsub(",", " ", j[,i,drop=TRUE])
      j[,i] <- gsub("\\.", ",", j[,i,drop=TRUE])
    }
  }

  #cut off first three years
  kable(j[4:nrow(j),],
        caption = cap,
        booktabs = TRUE,
        longtable = TRUE,
        linesep = "",
        escape = FALSE, format = "latex",
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

   tac[,c(2,3,4,5)] <- apply(tac[,c(2,3,4,5)],
                            2,
                            function(x){
                              tmp <- as.numeric(x)
                              f(tmp)
                            })
  tac[grep(" *NA", tac[,2]), 2] = if (!french) "bycatch only" else "prise accessoire"
  tac[grep(" *NA", tac[,3]), 3] = if (!french) "bycatch only" else "prise accessoire"
  tac[grep(" *NA", tac[,4]), 4] = if (!french) "bycatch only" else "prise accessoire"

  #Hardcode the translation for IFMP and bycatch only
  if (french) {
    tac[1:13,6] <- "PGIP"
    tac[23,2:4] <- "prises accessoires"
    for (i in seq_len(ncol(tac))) {
      tac[,i] <- gsub(",", " ", tac[,i,drop=TRUE])
    }
  }

  colnames(tac) <- latex.bold(colnames(tac))

  kable(tac, caption = cap,
        booktabs = TRUE,
        longtable = TRUE,
        linesep = "",
        escape = FALSE, format = "latex",
        align = c("l", "r", "r", "r", "r", "l")) %>%
    kable_styling(latex_options = c("hold_position", "repeat_header"), font_size = 8.5)

}
