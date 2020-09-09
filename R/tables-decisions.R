decision.table <- function(model,
                           caption = "",
                           make.table = TRUE,
                           format = "pandoc",
                           tac.vec = NA,
                           make.lt.gt = TRUE,
                           french=FALSE){
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
    col.names <- c(latex.mlc(c("$2019$", "$\\mathrm{Prise (mt)}$")),
                   latex.mlc(c("$P(B_{2020} <$", "$B_{2019})$")),
                   latex.mlc(c("$P(F_{2019} >$", "$F_{2018})$")),
                   latex.mlc(c("$P(B_{2020} <$", "$\\mathrm{LRP})$")),
                   latex.mlc(c("$P(B_{2020} <$", "$\\mathrm{USR})$")),
                   latex.mlc(c("$P(F_{2019} >$", "$\\mathrm{LRR})$")))
  }
  if(french==TRUE){
    if(format == "html"){
      col.names <- c("2019 Prise (mt)",
                     "P(B2020 < B2019)",
                     "P(F2019 > F2018)",
                     "P(B2020 < PRL)",
                     "P(B2020 < RSS)",
                     "P(F2019 > TEL)")

    }else{
      col.names <- c(latex.mlc(c("$2019$", "$\\mathrm{Prise (mt)}$")),
                     latex.mlc(c("$P(B_{2020}<$", "$B_{2019})$")),
                     latex.mlc(c("$P(F_{2019} >$", "$F_{2018})$")),
                     latex.mlc(c("$P(B_{2020} <$", "$\\mathrm{PRL})$")),
                     latex.mlc(c("$P(B_{2020} <$", "$\\mathrm{RSS})$")),
                     latex.mlc(c("$P(F_{2019} >$", "$\\mathrm{TEL})$")))
    }
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

suggested.ref.points <- function(french=FALSE, definition_text="definition", caption_text="caption"){

    df <- data.frame(
    referencepoint = c("$B_{\t{Min}}$",
                       "$B_{\t{Avg}}$",
                       "$F_{\t{Avg}}$",
                       "$B_{\t{2018}}$",
                       "$F_{\t{2017}}$"),
    Definition = definition_text,
    Role = c(en2fr("LRP", translate=french, allow_missing=TRUE),
             en2fr("USR", translate=french, allow_missing=TRUE),
             en2fr("LRR", translate=french, allow_missing=TRUE),
             en2fr("Benchmark", translate=french, allow_missing=TRUE),
                   en2fr("Benchmark", translate=french, allow_missing=TRUE))) %>%
    rename("Reference point" = referencepoint)

  colnames(df) <- en2fr(colnames(df), translate = french, allow_missing = TRUE)
  colnames(df) <- latex.bold(colnames(df))
  kable(df,
        caption = caption_text,
        booktabs = TRUE,
        linesep = "",
        escape = FALSE,
        format = "pandoc",
        align = c("l", "l", "l")) %>%
    column_spec(2, width = "10cm") %>%
    kableExtra::kable_styling(latex_options = "hold_position")
}
