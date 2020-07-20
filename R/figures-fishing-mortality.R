scaleFUN <- function(x) sprintf("%.2f", x)

f.plot <- function(models,
                   models.names,
                   add.hist.ref = FALSE,
                   lrp = NA,
                   usr = NA){
  ## lrp usr are year ranges (2-element vectors) to take the mean of
  ## the F value for the reference points

  f.quants <- lapply(models,
                     function(x){
                       ## Assume only first gear has F's
                       j <- x$mcmccalcs$f.mort.quants[[1]]
                       rownames(j)[1] <- "lowercv"
                       rownames(j)[2] <- "median"
                       rownames(j)[3] <- "uppercv"
                       j})

  names(f.quants) <- models.names
  f.quants <- lapply(f.quants,
                      function(x){
                        tmp <- as.data.frame(t(x))
                        tmp %>% mutate(Year = rownames(tmp))})
  f <- bind_rows(f.quants, .id = "Sensitivity") %>%
    as.tibble() %>%
    rename(`Fishing mortality` = median) %>%
    mutate(Year = as.numeric(Year)) %>%
    mutate(Sensitivity = forcats::fct_relevel(Sensitivity,
                                              models.names[1],
                                              after = 0)) %>%
    select(-MPD)

  p <- ggplot(f, aes(x = Year,
                      y = `Fishing mortality`,
                      ymin = lowercv,
                      ymax = uppercv,
                      fill = Sensitivity)) +
    geom_ribbon(alpha = 0.2) +
    geom_line(aes(color = Sensitivity),
              size = 1) +
    theme(legend.position = c(1, 1),
          legend.justification = c(1, 1),
          legend.title = element_blank()) +
    scale_y_continuous(labels = scaleFUN,
                       limits = c(0, NA)) +
    coord_cartesian(expand = FALSE) +
    xlim(c(min(f$Year - 1), NA)) +
    scale_x_continuous(breaks = seq(0, 3000, 5))

  if(add.hist.ref){
    if(is.na(lrp) || is.na(usr)){
      cat0("Supply year ranges for both lrp and usr when add.hist.ref is TRUE")
    }else{
      cal <- f %>%
        filter(Year >= lrp[1] & Year <= lrp[2])
      lrp.val <- mean(cal$`Fishing mortality`)

      cau <- f %>%
        filter(Year >= usr[1] & Year <= usr[2])
      usr.val <- mean(cau$`Fishing mortality`)
      j <- data.frame("Intercept" = c(lrp.val, usr.val),
                      "Color" = c("red", "green"))
      p <- p +
        geom_hline(data = j,
                   aes(yintercept = Intercept),
                   color = j$Color,
                   linetype = "dashed",
                   size = 1,
                   show.guide = TRUE)
    }
  }

  if(length(models) == 1){
    p <- p + theme(legend.position = "none")
  }
  p
}
