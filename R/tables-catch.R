catch.table <- function(dat,
                        dat.disc = NULL, # no longer used
                        area = "NA",
                        cap = ""){
  ## dat is what comes out of data/get-data.R/total.catch.yr.qtr
  ## dat.disc is what comes out of data/get-data.R/total.catch.discards

  j <- dat %>%
    group_by(year) %>%
    summarize(Year = year[1],
      USA = sum(usa_catch),
      `Canada landed` = sum(landed_canada),
      `Canada released at sea` = sum(discarded_canada),
      `Canada total` = sum(landed_canada + discarded_canada),
      `Total catch` = `Canada total` + `USA`) %>%
    select(-year)

  j <- j[!is.na(j$`Total catch`),]

  j <- j[,c("Year", "Canada landed", "Canada released at sea", "Canada total", "USA", "Total catch")]

  j[,-1] <- round(j[,-1], 0)

  j[,c(2,3,4,5,6)] <- apply(j[,c(2,3,4,5,6)],
                            2,
                            function(x){
                              tmp <- as.numeric(x)
                              f(tmp)
                            })

  colnames(j) <- latex.bold(colnames(j))
  #do not include 2018
  kable(j[1:(nrow(j)-1),],
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
                      cap = ""){
  ## dat is what comes out of the csv file data/pcod-tac-1996-2018.csv

  names(tac) <- gsub("X", "", names(tac))

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
