# Use the b.plot function and remove all plotting
b.csv <- function(models,
                   models.names,
                   depl = FALSE,
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
