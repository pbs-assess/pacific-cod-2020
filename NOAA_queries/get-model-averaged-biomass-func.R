# Use the b.plot function and remove all plotting
b.csv <- function(models,
                   models.names,
                   depl = FALSE,
                   add.hist.ref = FALSE,
                   add.bo.ref = FALSE,
                   lrp = NA,
                   usr = NA,
                   proj_columns = NULL,
                   tac_vector = 0,
                   burnin = 1000,
                   thin = 1,
                   year_range = NULL,
                   probs = c(0.025, 0.975),
                   french=FALSE
){
  ## lrp usr are year ranges (2-element vectors) to take the mean of
  ## the biomass for the reference points

  ## Biomass or Depletion
  if(depl){
    bt.quants <- lapply(models,
                        function(x){
                          j <- x$mcmccalcs$depl.quants
                          rownames(j)[1] <- "lowercv"
                          rownames(j)[2] <- "median"
                          rownames(j)[3] <- "uppercv"
                          j})
  }else{
    bt.quants <- lapply(models,
                        function(x){
                          j <- x$mcmccalcs$sbt.quants
                          rownames(j)[1] <- "lowercv"
                          rownames(j)[2] <- "median"
                          rownames(j)[3] <- "uppercv"
                          j})
  }

  if (!is.null(proj_columns)) {
    m <- models[[1]]
    stopifnot(all(tac_vector %in% unique(m$mcmc$proj$TAC)))
    proj_dat <- filter(m$mcmccalcs$proj.dat, TAC %in% tac_vector)
    proj_dat <- proj_dat[,c("TAC", proj_columns),drop=FALSE]
    names(proj_dat) <- gsub("^B", "", names(proj_dat))
    proj_dat <- reshape2::melt(proj_dat, id.vars = "TAC") %>%
      mutate(year = as.numeric(as.character(variable))) %>%
      select(-variable) %>%
      rename(B = value) %>%
      as_tibble() %>%
      group_by(TAC, year) %>%
      summarise(
        q05 = quantile(B, probs = probs[[1]]),
        q50 = quantile(B, probs = 0.50),
        q95 = quantile(B, probs = probs[[2]])
      )
  }

  names(bt.quants) <- models.names
  bt.quants <- lapply(bt.quants,
                      function(x){
                        tmp <- as.data.frame(t(x))
                        tmp %>% mutate(Year = rownames(tmp))})
  bt <- bind_rows(bt.quants, .id = "Sensitivity") %>%
    as.tibble() %>%
    mutate(Year = as.numeric(Year)) %>%
    mutate(Sensitivity = forcats::fct_relevel(Sensitivity,
                                              models.names[1],
                                              after = 0)) %>%
    select(-MPD)


  bt
}
