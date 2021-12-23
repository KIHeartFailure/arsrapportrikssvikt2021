timefuncpdf <- function(qi, startime = ar - 4, stoptime = ar, ll = NULL, ul = NULL,
                        data = rsdata, ylimmin = NULL, onlyindex = FALSE) {
  if (!onlyindex) {
    tmp <- data %>%
      filter(indexyear >= startime &
        indexyear <= stoptime &
        !is.na(!!sym(qi))) %>%
      mutate(!!sym(qi) := factor(!!sym(qi)),
        vtype = factor(vtype),
        ttype = factor(ttype)
      )

    # riket
    # riket <- tmp %>%
    #   group_by(ttype, indexyear) %>%
    #   count(!!sym(qi)) %>%
    #   mutate(
    #     tot = sum(n),
    #     percent = n / tot * 100
    #   ) %>%
    #   ungroup() %>%
    #   filter(!!sym(qi) == 1) %>%
    #   mutate(
    #     unit = "Riket",
    #     byvar = 1
    #   )

    # # per vtyp
    vtype <- tmp %>%
      filter(!is.na(vtype)) %>%
      group_by(vtype, ttype, indexyear, .drop = F) %>%
      count(!!sym(qi), .drop = F) %>%
      mutate(
        tot = sum(n),
        percent = n / tot * 100
      ) %>%
      ungroup() %>%
      filter(!!sym(qi) == 1) %>%
      filter(tot >= 10) %>%
      mutate(
        ltymy = as.numeric(ttype),
        colmy = as.numeric(vtype) + as.numeric(vtype) - 1
      ) %>%
      rename(unit = vtype)

    if (is.null(ylimmin)) {
      ymin <- floor(min(vtype$percent, na.rm = T) / 10) * 10
      ymax <- ceiling(max(vtype$percent, na.rm = T) / 10) * 10
      ylimmin <- c(ymin, ymax + 2)
    }

    all <- vtype %>%
      select(ttype, indexyear, percent, unit) %>%
      pivot_wider(names_from = c(ttype, unit), values_from = percent) %>%
      select(-indexyear) %>%
      as.matrix()

    cexmy <- 1
    # c(bottom, left, top, right) default c(5, 4, 4, 2) + 0.1.
    par(mar = c(4, 4, 0, 12) + 0.1)

    matplot(all,
      # type = "b",
      type = "l",
      # pch = unique(vtype$ltymy + 14),
      col = unique(global_cols[vtype$colmy]),
      lty = unique(vtype$ltymy),
      lwd = 3,
      # cex = 1.5,
      axes = FALSE,
      xaxs = "i",
      yaxs = "i",
      ylim = ylimmin,
      xlim = c(1, stoptime - startime + 1),
      ylab = "Procent",
      xlab = labnams[1],
      cex.lab = cexmy
    )

    axis(2, seq(ymin, ymax, 10), cex.axis = cexmy, las = 2)

    abline(h = ll * 100, col = "#FFCA02", lty = 2, lwd = 1)
    abline(h = ul * 100, col = "#61A60F", lty = 2, lwd = 1)
    axis(2,
      at = ll * 100, labels = labnams[3],
      line = -24.9,
      tick = FALSE,
      cex.axis = cexmy,
      las = 2,
      hadj = 0
    )
    axis(2,
      at = ul * 100, labels = labnams[2],
      line = -24.9,
      tick = FALSE,
      cex.axis = cexmy,
      las = 2,
      hadj = 0
    )

    axis(1, at = 1:(stoptime - startime + 1), labels = startime:stoptime, cex.axis = cexmy)

    axis(2,
      at = all[stoptime - startime + 1, ], labels = str_replace(colnames(all), "_", " "),
      line = -24.9,
      tick = FALSE, cex.axis = cexmy,
      las = 2,
      hadj = 0,
      gap.axis = -10000000
    )
  }
  if (onlyindex) {
    tmp <- data %>%
      filter(indexyear >= startime &
        indexyear <= stoptime &
        !is.na(!!sym(qi)) &
        ttype == "Index") %>%
      mutate(!!sym(qi) := factor(!!sym(qi)),
        vtype = factor(vtype)
      )
    # # per vtyp
    vtype <- tmp %>%
      filter(!is.na(vtype)) %>%
      group_by(vtype, indexyear, .drop = F) %>%
      count(!!sym(qi), .drop = F) %>%
      mutate(
        tot = sum(n),
        percent = n / tot * 100
      ) %>%
      ungroup() %>%
      filter(!!sym(qi) == 1) %>%
      filter(tot >= 10) %>%
      mutate(colmy = as.numeric(vtype) + as.numeric(vtype) - 1) %>%
      rename(unit = vtype)

    if (is.null(ylimmin)) {
      ymin <- floor(min(vtype$percent, na.rm = T) / 10) * 10
      ymax <- ceiling(max(vtype$percent, na.rm = T) / 10) * 10
      ylimmin <- c(ymin, ymax + 2)
    }

    all <- vtype %>%
      select(indexyear, percent, unit) %>%
      pivot_wider(names_from = c(unit), values_from = percent) %>%
      select(-indexyear) %>%
      as.matrix()

    cexmy <- 1
    # c(bottom, left, top, right) default c(5, 4, 4, 2) + 0.1.
    par(mar = c(4, 4, 0, 11) + 0.1)

    matplot(all,
      # type = "b",
      type = "l",
      # pch = unique(vtype$ltymy + 14),
      col = unique(global_cols[vtype$colmy]),
      lty = 1,
      lwd = 3,
      # cex = 1.5,
      axes = FALSE,
      xaxs = "i",
      yaxs = "i",
      ylim = ylimmin,
      xlim = c(1, stoptime - startime + 1),
      ylab = "Procent",
      xlab = labnams[1],
      cex.lab = cexmy
    )

    axis(2, seq(ymin, ymax, 10), cex.axis = cexmy, las = 2)

    abline(h = ll * 100, col = "#FFCA02", lty = 2, lwd = 1)
    abline(h = ul * 100, col = "#61A60F", lty = 2, lwd = 1)
    axis(2,
      at = ll * 100, labels = labnams[3],
      line = -26,
      tick = FALSE,
      cex.axis = cexmy,
      las = 2,
      hadj = 0
    )
    axis(2,
      at = ul * 100, labels = labnams[2],
      line = -26,
      tick = FALSE,
      cex.axis = cexmy,
      las = 2,
      hadj = 0
    )

    axis(1, at = 1:(stoptime - startime + 1), labels = startime:stoptime, cex.axis = cexmy)

    axis(2,
      at = all[stoptime - startime + 1, ], labels = str_replace(colnames(all), "_", " "),
      line = -26,
      tick = FALSE, cex.axis = cexmy,
      las = 2,
      hadj = 0,
      gap.axis = -10000000
    )
  }
}
