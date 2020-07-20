decision.table <- function(model,
                           caption = "",
                           make.table = TRUE,
                           format = "pandoc",
                           tac.vec = NA,
                           make.lt.gt = TRUE){
  ## make.lt.gt = add less than and greater than
  ## sybols in table. Changes those columns to character

  model <- model[[1]]

  dat <- as.data.frame(matrix(NA,
                              ncol = 6,
                              nrow = length(tac)))
  if(format == "html"){
    col.names <- c("2019 Catch (mt)",
                   "P(B2020 < B2019)",
                   "P(F2019 > F2018)",
                   "P(B2020 < LRP)",
                   "P(B2020 < USR)",
                   "P(F2019 > LRR)")

  }else{
    col.names <- c("$2019$ Catch (mt)",
                   "$P(B_{2020} < B_{2019})$",
                   "$P(F_{2019} > F_{2018})$",
                   "$P(B_{2020} < \\mathrm{LRP})$",
                   "$P(B_{2020} < \\mathrm{USR})$",
                   "$P(F_{2019} > \\mathrm{LRR})$")
  }
  tac <- model$proj$tac.vec
  if(!is.na(tac.vec[1])){
    tac <- tac.vec[tac.vec %in% tac]
  }
  for(t in seq_along(tac)){
    d <- as.data.frame(model$mcmccalcs$proj.dat)
    d <- d[d$TAC == tac[t],]
    dat[t, 1] <- f(tac[t], 0)
    dat[t, 2] <- f(mean(d$B2020B2019 < 1), 2)
    dat[t, 3] <- f(mean(d$F2019F2018 > 1), 2)
    dat[t, 4] <- f(mean(d$B2020Bmin < 1), 2)
    dat[t, 5] <- f(mean(d$B2020BAvgS < 1), 2)
    dat[t, 6] <- f(mean(d$F2019FAvgS > 1), 2)
  }

  if(make.lt.gt){
    dat <- mutate_at(dat, -1,
                     function(x) gsub('0.00', '<0.01', x))
    dat <- mutate_at(dat, -1,
                     function(x) gsub('1.00', '>0.99', x))
  }

  if(make.table){
    kable(dat,
          caption = caption,
          booktabs = TRUE,
          longtable = TRUE,
          linesep = "",
          escape = FALSE,
          format = format,
          col.names = col.names) %>%
      kable_styling(latex_options = c("hold_position", "repeat_header")) %>%
      kableExtra::column_spec(1, width = "2.7cm") %>%
      kableExtra::column_spec(2:6, width = "2.0cm")
  }else{
    dat
  }
}

suggested.ref.points <- function(){
  df <- data.frame(
    referencepoint = c("$B_{\t{Min}}$",
                       "$B_{\t{Avg}}$",
                       "$F_{\t{Avg}}$",
                       "$B_{\t{2018}}$",
                       "$F_{\t{2017}}$"),
    Definition = c(
      latex.mlc(c("Lowest estimated biomass agreed to be an",
                  "undesirable state to avoid ($B_{\t{2000}}$",
                  "in  5ABCD; $B_{\t{1986}}$ in 3CD)"),
                make.bold = FALSE),
      "Average biomass for the period 1956-2004",
      "Average fishing mortality for the period 1956-2004",
      "Biomass in 2018",
      "Fishing mortality in 2017"),
    Role = c("LRP",
             "USR",
             "LRR",
             "Benchmark",
             "Benchmark")) %>%
    rename("Reference point" = referencepoint)

  colnames(df) <- latex.bold(colnames(df))
  kable(df,
        caption = paste0("Reference points for the Reference Case ",
                         "5ABCD and 3CD models."),
        booktabs = TRUE,
        linesep = "",
        escape = FALSE,
        format = "pandoc",
        align = c("l", "l", "l")) %>%
    column_spec(2, width = "10cm") %>%
    kableExtra::kable_styling(latex_options = "hold_position")
}
