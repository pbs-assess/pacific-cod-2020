mcmc_monitor <- function(model) {
  x <- model$mcmccalcs$p.dat
  a <- array(NA, dim = c(nrow(x), 1L, ncol(x)))
  for (i in seq_len(ncol(x))) {
    a[,,i] <- x[,i]
  }
  dimnames(a)[3L] <- list(colnames(x))
  out <- as.data.frame(rstan::monitor(a, print = FALSE, warmup = 0))
  out$par <- row.names(out)
  out %>% select(par, n_eff, Rhat) %>%
    mutate(
      n_eff = round(n_eff, 0),
      Rhat = f(round(Rhat, 2), 2))
}

make.parameters.table <- function(model,
                                  caption = "default", omit_pars = NULL,
                                  omit_selectivity_pars = FALSE,
                                  format = "pandoc",
                                  french=FALSE){

  get.bounds <- function(ind){
    ## Return the bounds string for row ind of the parameters
    paste0("[",
      params$lb[ind],
      ", ",
      params$ub[ind],
      "]")
  }

  get.vals <- function(ind){
    ## Return a 3-element vector for the number estimated, bounds, and prior
    ##  dist mean and SD or "Uniform" for the row ind of the parameters
    ##
    ## vec is a vector of 4 values, the phase, prior type (0=uniform, 1=normal,
    ##  2=lognormal, 3=beta, 4=gamma), the first and second parameter
    ##  of the prior.
    vec <- as.numeric(params[ind, 4:7])
    if(vec[1] < 1){
      return(c(0,
        en2fr("Fixed",translate=french,allow_missing=TRUE),
        paste0("$", f(params[ind, 1], 3), "$")))
    }else if(vec[2] == 0){
      return(c(1,
        get.bounds(ind),
        "Uniform"))
    }else if(vec[2] == 1){
      return(c(1,
        get.bounds(ind),
        paste0("Normal($ln(",
          round(exp(vec[3]),3),
          "), ",
          vec[4],
          "$)")))
    }else if(vec[2] == 2){
      return(c(1,
        get.bounds(ind),
        paste0("Lognormal($",
          vec[3],
          ", ",
          vec[4],
          "$)")))
    }else if(vec[2] == 3){
      return(c(1,
        get.bounds(ind),
        paste0("Beta($\\alpha = ",
          vec[3],
          ", \\beta = ",
          vec[4],
          "$)")))
    }else if(vec[2] == 4){
      return(c(1,
        get.bounds(ind),
        paste0("Gamma($k = ",
          vec[3],
          "), \\theta = ",
          vec[4],
          "$)")))
    }
    invisible()
  }

  if(class(model) == model.lst.class){
    model <- model[[1]]
    if(class(model) != model.class){
      stop("The structure of the model list is incorrect.")
    }
  }

  ctl <- model$ctl

  params <- as.data.frame(ctl$params)
  stopifnot(row.names(ctl$params) ==
      c("log_ro", "steepness", "log_m", "log_avgrec", "log_recinit",
    "rho", "kappa"))

  tab <- data.frame(param = character(),
    num.est = character(),
    bounds = character(),
    prior = character(),
    stringsAsFactors = FALSE)

  #Need to do it this way due to different order of adjectives and nouns (e.g., mean recruitment)
if(french==TRUE){
  param.text <- c("Log recrutement ($ln(R_0)$)",
                  "La pente ($h$)",
                  paste("Log", en2fr("Natural mortality", translate=french, allow_missing=TRUE, case="lower" ), "($ln(M)$))"),
                  "Log recrutement moyen ($\\ln(\\overline{R})$)",
                  "Log recrutement initial ($\\ln(\\overline{R}_{init})$)",
                  "Rapport de variance ($\\rho$)",
                  "Variance totale inverse ($\\vartheta^2$)")
}else{
  param.text <- c("Log recruitment ($ln(R_0)$)",
                  "Steepness ($h$)",
                  "Natural mortality ($ln(M)$)",
                  "Log mean recruitment ($\\ln(\\overline{R})$)",
                  "Log initial recruitment ($\\ln(\\overline{R}_{init})$)",
                  "Variance ratio ($\\rho$)",
                  "Total inverse variance ($\\vartheta^2$)")

}
  param.vals <- do.call(rbind, lapply(1:nrow(params), get.vals))

  ## Selectivity parameters
  ## sel data frame has one column for each gear and 10 rows:
  ## 1  - selectivity type:
  ##       1) logistic selectivity parameters
  ##       2) selectivity coefficients
  ##       3) a constant cubic spline with age-nodes
  ##       4) a time varying cubic spline with age-nodes
  ##       5) a time varying bicubic spline with age & year nodes
  ##       6) fixed logistic (set isel_type=6, and estimation phase to -1)
  ##       7) logistic function of body weight.
  ##       8) logistic with weight deviations (3 parameters)
  ##       11) logistic selectivity with 2 parameters based on mean length
  ##       12) length-based selectivity coefficients with spline interpolation
  ## 2  - Age/length at 50% selectivity (logistic)
  ## 3  - STD at 50% selectivity (logistic)
  ## 4  - No. of age nodes for each gear (0=ignore)
  ## 5  - No. of year nodes for 2d spline(0=ignore)
  ## 6  - Phase of estimation (-1 for fixed) If neg number, it reflects a
  ##       mirroring of another gear's selectivity.
  ## 7  - Penalty wt for 2nd differences w=1/(2*sig^2)
  ## 8  - Penalty wt for dome-shaped w=1/(2*sig^2)
  ## 9  - Penalty wt for time-varying selectivity
  ## 10 - n_sel_blocks (number of selex blocks)

  sel <- ctl$sel
  dat <- model$dat
  indices <- dat$indices
  indices.df <- as.data.frame(do.call(rbind, indices))
  surv.gear.nums <- unique(indices.df$gear)
  surv.sel <- as.data.frame(sel[,surv.gear.nums])
  fish.sel <- as.data.frame(sel[,-surv.gear.nums])
  ## Get number estimated by looking at the phase row in the sel data frame
  surv.est <- surv.sel[6,]
  surv.est <- sum(surv.est > 0)
  fish.est <- fish.sel[6,]
  fish.est <- sum(fish.est > 0)
  ## Hardwired bounds of 0,1 for age-at-50% and 0,Inf for age-at-50% SD
  param.vals <- rbind(param.vals,
    c(surv.est,
      "[0, 1]",
      en2fr("None",translate=french,allow_missing=TRUE)),
    c(fish.est,
      "[0, 1]",
      en2fr("None",translate=french,allow_missing=TRUE)),
    c(surv.est,
      "[0, Inf)",
      en2fr("None",translate=french,allow_missing=TRUE)),
    c(fish.est,
      "[0, Inf)",
      en2fr("None",translate=french,allow_missing=TRUE)))

  param.text <- c(param.text,
    "Survey age at 50\\% selectivity ($\\hat{a}_k$)",
    "Fishery age at 50\\% selectivity ($\\hat{a}_k$)",
    "Survey SD of logistic selectivity ($\\hat{\\gamma}_k$)",
    "Fishery SD of logistic selectivity ($\\hat{\\gamma}_k$)")

  ## Catchability  parameters
  ## q is a data frame with 1 column for each survey and 3 rows:
  ## 1 - prior type:
  ##      0) Uniformative prior
  ##      1) normal prior density for log(q)
  ##      2) random walk in q
  ## 2 - prior log(mean)
  ## 3 - prior SD

  q <- ctl$surv.q
  num.inds <- ctl$num.indices
  param.vals <- rbind(param.vals,
    c(num.inds,
      en2fr("None",translate=french,allow_missing=TRUE),
      "Normal($0.5, 1$)"))

  param.text <- c(param.text,
    paste(en2fr("Survey catchability", translate=french, allow_missing=TRUE), "($q_k$)"))

  ## Fishing mortality and recruitment parameters
  ##
  par <- model$par
  num.f.params <- length(par$log_ft_pars)
  num.rec.params <- length(par$log_rec_devs)
  num.init.rec.params <- length(par$init_log_rec_devs)
  param.vals <- rbind(param.vals,
    c(num.f.params,
      "[-30, 3]",
      "[-30, 3]"),
    c(num.rec.params,
      en2fr("None",translate=french,allow_missing=TRUE),
      "Normal($0, 2$)"),
    c(num.init.rec.params,
      en2fr("None",translate=french,allow_missing=TRUE),
      "Normal($0, 2$)"))

  if(french==TRUE){
  param.text <- c(param.text,
    paste("Log", en2fr("Fishing mortality", translate=french,allow_missing=TRUE, case="lower"), "($\\Gamma_{k,t}$)"),
    paste(en2fr("Log recruitment deviations", translate=french,allow_missing=TRUE, case="sentence"), "($\\omega_t$)"),
    paste(en2fr("Log recruitment deviations", translate=french,allow_missing=TRUE, case="sentence"), "inital ($\\omega_{init,t}$)"))
  }else{
  param.text <- c(param.text,
    "Log fishing mortality values ($\\Gamma_{k,t}$)",
    "Log recruitment deviations ($\\omega_t$)",
    "Initial log recruitment deviations ($\\omega_{init,t}$)")
  }
  tab <- cbind(param.text, param.vals)

  tab <- as.data.frame(tab)
  if (!is.null(omit_pars)) {
    tab <- dplyr::filter(tab, !`param.text` %in% omit_pars)
    # tab <- dplyr::filter(tab, !`\\textbf{Parameter}` %in% omit_pars)
  }
  if (omit_selectivity_pars) {
    # tab <- dplyr::filter(tab, !grepl("selectivity", `\\textbf{Parameter}`))
    tab <- dplyr::filter(tab, !grepl("selectivity", `param.text`))
  }


  colnames(tab) <- c(latex.bold(en2fr("Parameter", translate = french, allow_missing = TRUE)),
                     latex.mlc(en2fr(c("Number",
                                 "estimated"), translate = french, allow_missing = TRUE)),
                     latex.mlc(en2fr(c("Bounds", "[low, high]"), translate = french, allow_missing = TRUE)),
                     latex.mlc(c("Prior (mean, SD)", "(single value = fixed)")))

  #Hardwire the last heading
  if(french==TRUE) {
    colnames(tab)[4] <- latex.bold(latex.mlc(c("Priori (moyenne, ET)", "(une seule valeur = fixe)")))
  }

  knitr::kable(tab,
    caption = caption, format = format,
    align = get.align(ncol(tab))[-1],
    booktabs = TRUE, linesep = "", escape = FALSE, row.names = FALSE) %>%
    kableExtra::kable_styling(latex_options = "hold_position")
}

make.parameters.est.table <- function(model,
                                      digits = 3,
                                      caption = "",
                                      omit_pars = NULL,
                                      format = "pandoc",
                                      french=FALSE){
  ## Returns an xtable in the proper format for parameter estimates and priors
  ##
  ## digits - number of decimal points on % columns
  ## xcaption - caption to appear in the calling document
  ## xlabel - the label used to reference the table in latex
  ## font.size - size of the font for the table
  ## space.size - size of the vertical spaces for the table
  ## placement - latex code for placement of the table in document


  if(class(model) == model.lst.class){
    model <- model[[1]]
    if(class(model) != model.class){
      stop("The structure of the model list is incorrect.")
    }
  }

  mc <- model$mcmccalcs
  p.quants <- mc$p.quants
  mcmc.names <- colnames(p.quants)

  ## Append MPD values
  mpd <- model$mpd
  mpd.names <- names(mpd)

  mpd.param.vals <- NULL
  for(pname in mcmc.names){
    ## This is hack code because iscam is not outputting the same parameter
    ##  names for MPD and MCMC runs
    if(pname == "h"){
      pname <- "steepness"
    }
    if(pname == "m1"){
      pname <- "m"
    }
    if(pname == "bo"){
      pname <- "sbo"
    }
    match.sel <- grep("sel[[:digit:]]+",
                      pname)
    match.sel.sd <- grep("selsd[[:digit:]]+",
                         pname)
    match.q <- grep("q[[:digit:]]+",
                    pname)
    ## Age value at 50%
    sel.pars <- mpd$sel_par[,3]
    ## Age SD at 50%
    sel.sd.pars <- mpd$sel_par[,4]
    q.pars <- mpd$q
    if(length(match.sel) > 0){
      ## The parameter starts with "sel"
      split.val <- strsplit(pname,
                            "[^[:digit:]]")[[1]]
      sel.num <- as.numeric(split.val[length(split.val)])
      this.par <- sel.pars[sel.num]
    }else if(length(match.sel.sd) > 0){
      ## The parameter starts with "selsd"
      split.val <- strsplit(pname,
                            "[^[:digit:]]")[[1]]
      sel.num <- as.numeric(split.val[length(split.val)])
      this.par <- sel.sd.pars[sel.num]
    }else if(length(match.q) > 0){
      ## The parameter starts with "q"
      split.val <- strsplit(pname,
                            "[^[:digit:]]")[[1]]
      q.num <- as.numeric(split.val[length(split.val)])
      this.par <- q.pars[q.num]
    }else{
      ## Match the mcmc name with the mpd name. Q and selectivity are special
      ##  cases, they must be extracted from vectors and matrices respectively
      this.par <- mpd[match(pname, mpd.names)]
    }
    j <- mpd.param.vals <- c(mpd.param.vals, this.par)
  }
  names(mpd.param.vals) <- mcmc.names
  tab <- rbind(p.quants, as.numeric(mpd.param.vals))
  row.n <- rownames(tab)
  row.n[length(row.n)] <- "MPD"
  rownames(tab) <- row.n

  tab <- t(tab)
  tab[row.names(tab) == "ro", ] <-       f(as.numeric(tab[row.names(tab) == "ro", ]), 0)
  tab[row.names(tab) == "h", ] <-        f(as.numeric(tab[row.names(tab) == "h", ]), digits)
  tab[row.names(tab) == "m", ] <-        f(as.numeric(tab[row.names(tab) == "m", ]), digits)
  tab[row.names(tab) == "rbar", ] <-     f(as.numeric(tab[row.names(tab) == "rbar", ]), 0)
  tab[row.names(tab) == "rinit", ] <-    f(as.numeric(tab[row.names(tab) == "rinit", ]), 0)
  tab[row.names(tab) == "vartheta", ] <- f(as.numeric(tab[row.names(tab) == "vartheta", ]), digits)
  tab[row.names(tab) == "sbo", ] <-      f(as.numeric(tab[row.names(tab) == "sbo", ]), 0)
  tab[row.names(tab) == "q1", ] <-       f(as.numeric(tab[row.names(tab) == "q1", ]), digits)
  tab[row.names(tab) == "q2", ] <-       f(as.numeric(tab[row.names(tab) == "q2", ]), digits)
  tab[row.names(tab) == "q3", ] <-       f(as.numeric(tab[row.names(tab) == "q3", ]), digits)
  tab[row.names(tab) == "q4", ] <-       f(as.numeric(tab[row.names(tab) == "q4", ]), digits)
  tab[row.names(tab) == "q5", ] <-       f(as.numeric(tab[row.names(tab) == "q5", ]), digits)
  tab[row.names(tab) == "q5", ] <-       f(as.numeric(tab[row.names(tab) == "q5", ]), digits)

  ## The next set of names only pertains to the ARF assessment, the q's
  ##  and sel's are modified to line up with each other.

  new.col <- rownames(tab)
  new.col <- gsub("^ro$", "$R_0$", new.col)
  new.col <- gsub("^h$", "$h$", new.col)
  new.col <- gsub("^m$", "$M$", new.col)
  new.col <- gsub("^rbar$", "$\\\\overline{R}$", new.col)
  new.col <- gsub("^rinit$", "$\\\\overline{R}_{init}$", new.col)
  new.col <- gsub("^vartheta$", "$\\\\vartheta$", new.col)
  new.col <- gsub("^sbo$", "$B_0$", new.col)
  new.col <- gsub("^q([0-9]+)$", "$q_\\1$", new.col)

  col.names <- colnames(tab)
  # col.names <- latex.bold(latex.perc(col.names))
  # col.names <- c(latex.bold("Parameter"), col.names)
  col.names <- c("Parameter", col.names)

  tab <- cbind(new.col, tab)
  colnames(tab) <- col.names

  tab <- as.data.frame(tab)
  tab$par <- row.names(tab)
  mon <- mcmc_monitor(model)
  tab <- left_join(tab, mon, by = "par")
  tab$par <- NULL
  tab$Rhat[tab$Rhat == " NaN"] <- "--"
  names(tab) <- gsub("n_eff", "$n_\\\\mathrm{eff}$", names(tab))
  names(tab) <- gsub("Rhat", "$\\\\hat{R}$", names(tab))

  if (!is.null(omit_pars)) {
    # tab <- dplyr::filter(tab, !`\\textbf{Parameter}` %in% omit_pars)
    tab <- dplyr::filter(tab, !`Parameter` %in% omit_pars)
  }

  colnames(tab) <- en2fr(colnames(tab), translate = french, allow_missing = TRUE)
  colnames(tab) <- latex.bold(latex.perc(colnames(tab)))

  knitr::kable(tab,
               caption = caption, format = format,
               align = get.align(ncol(tab))[-1], longtable = TRUE,
               booktabs = TRUE, linesep = "", escape = FALSE, row.names = FALSE) %>%
    kableExtra::kable_styling(latex_options = "hold_position")
}

make.ref.points.table <- function(models,
                                  add.hist.ref = TRUE,
                                  digits = 3,
                                  caption = "default",
                                  omit_msy = FALSE,
                                  lrp = NA,
                                  usr = NA,
                                  lower = 0.025,
                                  upper = 0.975,
                                  format = "pandoc",
                                  french=FALSE)
  {

  probs <- c(lower, 0.5, upper)

  model <- models[[1]]
  if(length(models) == 1){
    if(class(models) == model.lst.class){
      model <- models[[1]]
      if(class(model) != model.class){
        stop("The structure of the model list is incorrect.")
      }
    }
    tab <- model$mcmccalcs$r.quants
    tab[,-1] <- f(tab[,-1], digits)
    if (omit_msy) tab <- dplyr::filter(tab, !grepl("MSY", `\\textbf{Reference Point}`)) #RF moved the filtering to here
    col.names <- colnames(tab)
    col.names[1] <- latex.bold(en2fr("Reference point", translate = french, allow_missing = TRUE))
    colnames(tab) <- col.names
  }else{
    tab <- lapply(models,
                  function(x){
                    x$mcmccalcs$r.dat
                  })
    tab <- bind_rows(tab)
    tab <- t(apply(tab, 2, quantile, prob = probs))
    tab <- f(tab, digits)
    k <- names(tab[,1])
    yr.sbt.init <- sub("b", "", k[6])
    yr.sbt.end <- sub("b", "", k[7])
    yr.f.end <- sub("f", "", k[10])
    desc.col <- c(latex.subscr.ital("B", "0"),
                  latex.subscr.ital("B", "MSY"),
                  "MSY",
                  latex.subscr.ital("F", "MSY"),
                  latex.subscr.ital("U", "MSY"),
                  latex.subscr.ital("B", yr.sbt.init),
                  latex.subscr.ital("B", yr.sbt.end),
                  paste0(latex.subscr.ital("B", yr.sbt.end),
                         "/",
                         latex.subscr.ital("B", 0)),
                  paste0(latex.subscr.ital("B", yr.sbt.end),
                         "/",
                         latex.subscr.ital("B", yr.sbt.init)),
                  latex.subscr.ital("F", yr.f.end),
                  paste0("0.4",
                         latex.subscr.ital("B", "MSY")),
                  paste0("0.8",
                         latex.subscr.ital("B", "MSY")))
    tab <- cbind.data.frame(desc.col, tab)
    col.names <- colnames(tab)
    col.names <- latex.bold(latex.perc(col.names))
    if (omit_msy) tab <- dplyr::filter(tab, !grepl("MSY",  desc.col)) #RF moved the filtering to here
    col.names[1] <- latex.bold(en2fr("Reference point", translate = french, allow_missing = TRUE))
    colnames(tab) <- col.names
  }

  if(add.hist.ref){
    if(is.na(lrp) || is.na(usr)){
      cat0("Supply year ranges for both lrp and usr when add.hist.ref is TRUE")
    }else{
      ## bt <- model$mcmccalcs$sbt.quants
      bt <- lapply(models,
                   function(x){
                     x$mcmccalcs$sbt.dat
                   })
      bt <- bind_rows(bt)
      yrs <- colnames(bt)
      bt <- t(apply(bt, 2, quantile, prob = probs))
      bt <- cbind(yrs, bt)

      bt <- bt %>%
        as.tibble() %>%
        mutate(Year = yrs)

      cal <- bt %>%
        filter(Year >= lrp[1] & Year <= lrp[2])
      lrp.5 <- mean(as.numeric(cal$`2.5%`))
      lrp.50 <- mean(as.numeric(cal$`50%`))
      lrp.95 <- mean(as.numeric(cal$`97.5%`))

      cau <- bt %>%
        filter(Year >= usr[1] & Year <= usr[2])

      usr.5 <- mean(as.numeric(cau$`2.5%`))
      usr.50 <- mean(as.numeric(cau$`50%`))
      usr.95 <- mean(as.numeric(cau$`97.5%`))
      lrp.desc <- paste0(en2fr("LRP",translate=french,allow_missing=TRUE), " (",
                         ifelse(lrp[1] == lrp[2],
                                lrp[1],
                                paste0(lrp[1], "--", lrp[2])),
                         ")")
      usr.desc <- paste0(en2fr("USR",translate=french,allow_missing=TRUE), " (",
                         ifelse(usr[1] == usr[2],
                                usr[1],
                                paste0(usr[1], "--", usr[2])),
                         ")")
      col.names <- colnames(tab)
      tab <- data.frame(lapply(tab, as.character), stringsAsFactors = FALSE)
      tab <- rbind(tab,
                   c(lrp.desc,
                     f(lrp.5),
                     f(lrp.50),
                     f(lrp.95)))

      tab <- rbind(tab,
                   c(usr.desc,
                     f(usr.5),
                     f(usr.50),
                     f(usr.95)))
      colnames(tab) <- col.names
    }
  }

  knitr::kable(tab,
    caption = caption, format = format,
    align = get.align(ncol(tab))[-1],
    booktabs = TRUE, linesep = "", row.names = FALSE, escape = FALSE) %>%
    kableExtra::kable_styling(latex_options = "hold_position")
}

make.value.table <- function(model,
  type,
  digits = 3,
  caption = "default",
  french=FALSE
  ){

  if(class(model) == model.lst.class){
    model <- model[[1]]
    if(class(model) != model.class){
      stop("The structure of the model list is incorrect.")
    }
  }

  if(type == 1){
    out.dat <- model$mcmccalcs$sbt.quants
  }else if(type == 2){
    out.dat <- model$mcmccalcs$recr.quants
  }else if(type == 3){
    out.dat <- model$mcmccalcs$f.mort.quants[[1]]
  }else if(type == 4){
    out.dat <- model$mcmccalcs$u.mort.quants[[1]]
  }else if(type == 5){
    out.dat <- model$mcmccalcs$depl.quants
  }else{
    stop("Type ", type, " not implemented.")
  }

  tab <- f(t(out.dat), digits)
  tab <- cbind(rownames(tab), tab)
  colnames(tab)[1] <- "Year"
  colnames(tab) <- en2fr(colnames(tab), translate = french, allow_missing = TRUE)
  colnames(tab) <- latex.bold(latex.perc(colnames(tab)))

  knitr::kable(tab,
    caption = caption,
    longtable = TRUE, format = "pandoc",
    align = get.align(ncol(tab))[-1],
    booktabs = TRUE, linesep = "", escape = FALSE, row.names = FALSE) %>%
    kableExtra::kable_styling(latex_options = c("hold_position", "repeat_header"))
}


model.param.desc.table <- function(cap = "",
                                   font.size = 8){

  j <- tribble(
    ~Parameter,          ~Description,                                                                  ~`Value 5ABCD`,                   ~`Value 3CD`,
    "\\textbf{Indices}", "",                                                                            "",                               "",
    "$t$",               "Time (years)",                                                                paste0(start.yr5, "--", end.yr5), paste0(start.yr3, "--", end.yr3),
    "$j$",               "Gear (fishery or index of abundance)",                                        "",                               "",
    "$a$",               "Age (years) used for initializing numbers in first year",                     paste0(sage5, "--", nage5, " y"), paste0(sage3, "--", nage3, " y"),
    "$A$",               "Maximum age (years) used for initializing numbers in first year",             paste0(nage5, " y"),              paste0(nage3, " y"),
    "\\textbf{Fixed input parameters}", "", "", "",
    "$k$",               "Age at knife-edge recruitment",                                               paste0(sage5, " y"),              paste0(sage3, " y"),
    "$L_\\infty$",       "Theoretical maximum length",                                                  paste0(linf5, " cm"),             paste0(linf3, " cm"),
    "$K_{VB}$",          "von Bertalannfy growth rate",                                                 as.character(k5),                 as.character(k3),
    "$a_{LW}$",          "Scaling parameter of the length/weight relationship",                         as.character(lwscal5),            as.character(lwscal3),
    "$b_{LW}$",          "Exponent of the length/weight relationship",                                  as.character(lwpow5),             as.character(lwpow3),
    "$t_0$",             "Theoretical age at 0 cm",                                                     as.character(t05),                as.character(t03),
    "$\\alpha_g$",       "Intercept of the Ford-Walford plot, for all ages > $k$",                      as.character(alpha.g5),           as.character(alpha.g3),
    "$\\rho_g$",         "Slope of the Ford-Walford plot, for all ages > $k$",                          as.character(rho.g5),             as.character(rho.g3),
    "$W_k$",             "Weight at age of recruitment $k$",                                            as.character(wk5),                as.character(wk3),
    "\\textbf{Annual input data}", "", "", "",
    "$C_t$",             "Catch (metric tonnes)",                                                       "",                               "",
    "$W_t$",             "Mean weight of individuals in the population",                                "",                               "",
    "$I_{j,t}$",         "Index of abundance $j$ (Survey or commercail trawl CPUE)",                    "",                               "",
    "$CV_{j,t}$",        "Annual coefficients of variation in index of abundance observations",         "",                               "",
    "\\textbf{Time-invariant parameters}", "", "", "",
    "$R_0$",             "\\textbf{Equilibrium unfished age-0 recruits} $^a$",                          "",                               "",
    "$h$",               "\\textbf{Steepness of the stock-recruit relationship}",                       "",                               "",
    "$M$",               "\\textbf{Natural mortality} $^a$",                                            "",                               "",
    "$R_{AVG}$",         "\\textbf{Average annual recruitment} $^a$",                                   "",                               "",
    "$R_{AVG\\_init}$",  "\\textbf{Average annual recuitment for initializing the model} $^a$",         "",                               "",
    "$CR$",              "Recruitment compensation ratio",                                              "",                               "",
    "$a$",               "Slope of the stock-recruit function at the origin",                           "",                               "",
    "$b$",               "Scaling parameter of the stock-recruit function",                             "",                               "",
    "$N_0$",             "Equilibrium unfished numbers",                                                "",                               "",
    "$B_0$",             "Equilibrium unfished biomass",                                                "",                               "",
    "$S_0$",             "Equilibrium unfished survival rate",                                          "",                               "",
    "$\\bar{W}_0$",      "Equilibrium unfished mean weight",                                            "",                               "",
    "$c_j$",             "Additional process error in index of abundance observations for gear $j$",    "",                               "",
    "\\textbf{Time-varying parameters}", "", "", "",
    "$\\omega_t$",       "Log-recruitment deviations$^a$",                                              "",                               "",
    "$F_t$",             "Fishing mortality in the trawl fishery",                                      "",                               "",
    "$S_t$",             "Annual survival rate",                                                        "",                               "",
    "$N_t$",             "Numbers",                                                                     "",                               "",
    "$R_t$",             "Recruits",                                                                    "",                               "",
    "$B_t$",             "Biomass",                                                                     "",                               "",
    "$\\bar{W}_t$",      "Predicted mean weight",                                                       "",                               "",
    "\\textbf{Likelihood components}", "", "", "",
    "$\\sigma_R$",       "Standard deviation in log-recruitment residuals",                             "",                               "",
    "$\\sigma_O$",       "Overall standard deviation in observation residuals",                         "",                               "",
    "$\\sigma_{i,j}$",   "Annual standard deviation in observation residuals for each survey",          "",                               "",
    "$\\sigma_C$",       "Standard deviation in catch",                                                 "",                               "",
    "$\\sigma_W$",       "Standard deviation in mean weight",                                           "",                               "",
    "$\\vartheta^{-2}$", "\\textbf{Inverse of the total variance (total precision)}",                   "",                               "",
    "$\\rho$",           "\\textbf{Proportion of total variance due to observation error}",             "",                               "",
    "$\\tau$",           "\\textbf{Variance in age composition residuals} $^b$",                        "",                               "",
    "$q_j$",             "\\textbf{Constant of proportionality in indices of (catchability)} $^{a,b}$", "",                               "",
    "$d_{j,t}^2$",       "Residual log difference for $j$ indices of abundance",                        "",                               "",
    "$d_{C_t}^2$",       "Residual log difference for catch data",                                      "",                               "",
    "$d_{W_t}^2$",       "Residual log difference for mean weight data",                                "",                               "")

  csasdown::csas_table(j,
                       format = "latex",
                       caption = cap,
                       bold_header = TRUE,
                       col_names = c("Parameter", "Description", "Value 5ABCD", "Value 3CD")) %>%
    column_spec(2, width = "5cm") %>%
    footnote(alphabet = c("Estimated in log space",
                          "Conditional MPD estimates"))
}
