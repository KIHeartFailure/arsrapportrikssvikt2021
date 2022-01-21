piefunc <- function(var, type = "Index",
                       startime = start, stoptime = stop,
                       data = rsdata) {
  tmp <- data %>%
    filter(
      d_DATE_FOR_ADMISSION >= startime &
        d_DATE_FOR_ADMISSION <= stoptime &
        ttype %in% type
    )

  levels(tmp[[var]]) <- c(levels(tmp[[var]]), labnams[4])

  riket <- tmp %>%
    count(!!sym(var)) %>%
    mutate(
      tot = sum(n),
      percent = n / tot * 100,
      !!sym(var) := replace_na(!!sym(var), labnams[4]),
      !!sym(var) := paste0(!!sym(var), "\n", fn(percent, 0), "%")
    )

  cols <- rev(global_colsblue[1:nrow(riket)])
  cols[str_detect(riket %>% pull(!!sym(var)), labnams[4])] <- global_colsmiss

  p <- ggplot(data = riket, aes(x = "", y = percent, fill = !!sym(var))) +
    geom_bar(stat = "identity") +
    theme_void() +
    theme(legend.position = "none") +
    geom_text(aes(x = 1.6, label = !!sym(var)), position = position_stack(vjust = 0.5), size = 5) +
    coord_polar(theta = "y") +
    scale_fill_manual(values = cols)

  return(p)
}
