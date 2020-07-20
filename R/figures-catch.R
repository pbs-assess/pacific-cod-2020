make.catches.plot <- function(dat,
                              every = 5,
                              last.yr = 2015){
  dat <- dat %>%
    select(-total_catch) %>%
    group_by(year) %>%
    summarize(usa_catch = sum(usa_catch),
              canada_catch = sum(canada_catch))
  dat <- melt(dat, id.vars = "year")
  p <- ggplot(dat) +
    aes(x = year, y = value, fill = variable) +
    geom_col() +
    coord_cartesian(expand = FALSE) +
    labs(x = "Year",
         y = "Catch (t)",
         fill = "") +
    scale_fill_brewer(labels = c("USA", "Canada"), palette = "Dark2") +
    scale_y_continuous(labels = comma,
                       limits = c(0, NA)) +
    scale_x_continuous(breaks = seq(0, last.yr, every)) +
    theme(legend.position = c(1, 1),
          legend.justification = c(1, 1),
          legend.title = element_blank())
  p
}

discards.plot <- function(dat){
  dat <- dat %>%
    group_by(year) %>%
    summarize(`Released at sea` = sum(discarded_canada) ,
              `Prop. released` = sum(discarded_canada) / sum(landed_canada + discarded_canada)) %>%
    rename(Year = year)

  g.bottom <- ggplot(dat) +
    aes(x = Year, y = `Released at sea`) +
    geom_col(fill = RColorBrewer::brewer.pal(3, "Dark2")[[2]]) +
    coord_cartesian(expand = FALSE) +
    labs(x = "Year",
         y = "Catch (t)",
         fill = "") +
    scale_x_continuous(breaks = seq(0, 2015, 5))

  g.top <- ggplot(dat) +
    aes(x = Year, y = `Prop. released`) +
    geom_line(color = "grey50",
              size = 1,
              alpha = 0.5) +
    scale_x_continuous(breaks = seq(0, 2015, 5))


  grid.arrange(g.top, g.bottom, heights = c(1/3, 2/3))

}

catch.fit.plot <- function(model,
                           every = 5,
                           last.yr = 2015){

  model <- model[[1]]
  obs <- model$dat$catch %>%
    as.tibble() %>%
    select(year, value)
  fit <- model$mpd$ct

  i <- as.tibble(cbind(obs, fit))
  names(i) <- c("Year", "Catch (t)", "Fit")

  p <- ggplot(i) +
    aes(x = Year,
        y = `Catch (t)`) +
    geom_point(size = 2) +
    geom_line(color = "red",
              y = i$Fit,
              size = 1) +
    scale_y_continuous(labels = comma,
                       limits = c(0, NA)) +
    scale_x_continuous(breaks = seq(0, last.yr, every))
  p
}
