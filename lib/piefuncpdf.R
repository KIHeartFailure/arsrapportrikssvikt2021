piefuncpdf <- function(var, type = "Index", bytype = FALSE,
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

  if (!bytype) {
    p <- ggplot(data = riket, aes(x = "", y = percent, fill = !!sym(var))) +
      geom_bar(stat = "identity") +
      theme_void() +
      theme(legend.position = "none") +
      geom_text(aes(x = 1.6, label = !!sym(var)), position = position_stack(vjust = 0.5), size = 5) +
      coord_polar(theta = "y") +
      scale_fill_manual(values = cols)
  }
  if (bytype) {
    riket <- tmp %>%
      group_by(ttype) %>%
      count(!!sym(var)) %>%
      mutate(
        tot = sum(n),
        percent = n / tot * 100,
        !!sym(var) := replace_na(!!sym(var), labnams[4]),
        !!sym(var) := paste0(!!sym(var), "\n", fn(percent, 0), "%")
      )

    cols <- rep(global_colsblue[2:(nrow(riket) / length(type) + 1)], each = length(type))
    cols[str_detect(riket %>% pull(!!sym(var)), labnams[4])] <- global_colsmiss

    p <- ggplot(data = riket, aes(x = "", y = percent, fill = !!sym(var))) +
      facet_wrap(~ttype, nrow = 1) +
      geom_bar(stat = "identity") +
      theme_void() +
      theme(
        legend.position = "none",
        strip.text.x = element_text(
          size = 14, face = "bold"
        )
      ) +
      geom_text(aes(x = 1.95, label = !!sym(var)), position = position_stack(vjust = 0.5), size = 4.5) +
      # geom_text(aes(x = 2.3, label = !!sym(var)), position = position_stack(vjust = 0.5), size = 6) +
      # geom_label(aes(label = ttype), position = position_fill(vjust = 0.5), size = 6) +
      coord_polar(theta = "y") +
      scale_fill_manual(values = cols)
  }
  return(p)
  # if (bytype){
  #
  #   riket <- tmp %>%
  #     group_by(ttype) %>%
  #     count(!!sym(var)) %>%
  #     mutate(
  #       tot = sum(n),
  #       percent = n / tot * 100,
  #       !!sym(var) := replace_na(!!sym(var), labnams[4]),
  #       !!sym(var) := paste0(!!sym(var), "\n", fn(percent, 0), "%")
  #     )
  #
  #   par(mfrow = c(1, 3))
  #   cols <- rev(global_colsblue[1:nrow(riket %>% filter(ttype == "Index"))])
  #   cols[str_detect(riket %>% pull(!!sym(var)), labnams[4])] <- global_colsmiss
  #
  #   pie(riket %>% filter(ttype == "Index") %>% pull(percent), col = cols, labels = riket %>% filter(ttype == "Index") %>% pull(!!sym(var)), border = NA, clockwise = T, sub = "Index")
  #   #pie(riket %>% filter(ttype == "Uppföljning") %>% pull(percent), col = cols, labels = riket %>% filter(ttype == "Uppföljning") %>% pull(!!sym(var)), border = NA, clockwise = T, sub = "Uppföljning")
  #   #pie(riket %>% filter(ttype == "Årlig uppföljning") %>% pull(percent), col = cols, labels = riket %>% filter(ttype == "Årlig uppföljning") %>% pull(!!sym(var)), border = NA, clockwise = T, sub = "Årlig uppföljning")
}
