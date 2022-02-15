timefunc <- function(qi = qitmp, starttime = global_year - 4, stoptime = global_year, ll = lltmp, ul = ultmp,
                     data = rsdata, ylimmin = c(0, 100), onlyindex = FALSE, adjtext = NULL) {
  if (!onlyindex) {
    tmp <- data %>%
      filter(indexyear %in% paste(seq(starttime, stoptime, 1)) &
        !is.na(!!sym(qi)))

    # riket
    riket <- tmp %>%
      group_by(ttype, indexyear, .drop = F) %>%
      count(!!sym(qi), .drop = F) %>%
      mutate(
        tot = sum(n),
        percent = n / tot * 100
      ) %>%
      ungroup() %>%
      filter(!!sym(qi) == 1) %>%
      filter(tot >= 10) %>%
      mutate(
        ltymy = as.numeric(ttype)
      )

    # if (is.null(ylimmin)) {
    #  ymin <- floor(min(riket$percent, na.rm = T) / 10) * 10
    #  ymax <- ceiling(max(riket$percent, na.rm = T) / 10) * 10
    #  ylimmin <- c(ymin, ymax)
    # }

    all <- riket %>%
      select(ttype, indexyear, percent) %>%
      pivot_wider(names_from = ttype, values_from = percent) %>%
      select(-indexyear) %>%
      as.matrix()

    cexmy <- 1
    # c(bottom, left, top, right) default c(5, 4, 4, 2) + 0.1.
    par(mar = c(5.5, 4, .5, 10) + 0.1)

    matplot(all,
      type = "b",
      # type = "l",
      pch = 19,
      lty = 1,
      col = global_colsblue[c(1, 3, 5, 7)],
      # lty = unique(riket$ltymy),
      lwd = 3,
      # cex = 1.5,
      axes = FALSE,
      xaxs = "i",
      yaxs = "i",
      ylim = ylimmin,
      xlim = c(1 - 0.1, stoptime - starttime + 1 + 0.1),
      ylab = "Procent",
      xlab = labnams[1],
      cex.lab = cexmy
    )

    box(bty = "l")

    if (ylimmin[2] - ylimmin[1] <= 22) {
      int <- 5
    } else {
      int <- 10
    }

    axis(2, seq(ylimmin[1], ylimmin[2], int), cex.axis = cexmy, las = 2)

    abline(h = ll * 100, col = "#FFCA02", lty = 2, lwd = 1)
    abline(h = ul * 100, col = "#61A60F", lty = 2, lwd = 1)

    axis(1, at = 1:(stoptime - starttime + 1), labels = starttime:stoptime, cex.axis = cexmy)

    if (is.null(adjtext)) {
      adjtext <- rep(0, length(colnames(all)))
    }
    axis(2,
      at = all[stoptime - starttime + 1, ] + adjtext, labels = str_replace(colnames(all), "_", " "),
      line = -32.2,
      tick = FALSE, cex.axis = cexmy,
      las = 2,
      hadj = 0,
      gap.axis = -10000000
    )

    if (ylimmin[1] != 0) {
      lines(x = c(0, 1), y = c(ylimmin[1], ylimmin[1] + 0.5))
      lines(x = c(0, 1), y = c(ylimmin[1] + 0.5, ylimmin[1] + 1))
    }
  }

  if (onlyindex) {
    tmp <- data %>%
      filter(indexyear %in% paste(seq(starttime, stoptime, 1)) &
        !is.na(!!sym(qi)) &
        ttype == "Index")
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
      # mutate(colmy = as.numeric(vtype) + as.numeric(vtype) - 1) %>%
      rename(unit = vtype)

    if (is.null(ylimmin)) {
      ymin <- floor(min(vtype$percent, na.rm = T) / 10) * 10
      ymax <- ceiling(max(vtype$percent, na.rm = T) / 10) * 10
      ylimmin <- c(ymin, ymax)
    }

    all <- vtype %>%
      select(indexyear, percent, unit) %>%
      pivot_wider(names_from = c(unit), values_from = percent) %>%
      select(-indexyear) %>%
      as.matrix()

    cexmy <- 1
    # c(bottom, left, top, right) default c(5, 4, 4, 2) + 0.1.
    par(mar = c(5.5, 4, 0.5, 10) + 0.1)

    matplot(all,
      type = "b",
      # type = "l",
      pch = 19,
      col = global_colsblue[c(3, 6)],
      lty = 1,
      lwd = 3,
      # cex = 1.5,
      axes = FALSE,
      xaxs = "i",
      yaxs = "i",
      ylim = ylimmin,
      xlim = c(1 - 0.1, stoptime - starttime + 1 + 0.1),
      ylab = "Procent",
      xlab = labnams[1],
      cex.lab = cexmy
    )

    box(bty = "l")

    if (ylimmin[2] - ylimmin[1] <= 22) {
      int <- 5
    } else {
      int <- 10
    }
    axis(2, seq(ylimmin[1], ylimmin[2], int), cex.axis = cexmy, las = 2)

    abline(h = ll * 100, col = "#FFCA02", lty = 2, lwd = 1)
    abline(h = ul * 100, col = "#61A60F", lty = 2, lwd = 1)

    axis(1, at = 1:(stoptime - starttime + 1), labels = starttime:stoptime, cex.axis = cexmy)

    if (is.null(adjtext)) {
      adjtext <- rep(0, length(colnames(all)))
    }
    axis(2,
      at = all[stoptime - starttime + 1, ] + adjtext, labels = str_replace(colnames(all), "_", " "),
      line = -32.2,
      tick = FALSE, cex.axis = cexmy,
      las = 2,
      hadj = 0,
      gap.axis = -10000000
    )
    if (ylimmin[1] != 0) {
      lines(x = c(0, 1), y = c(ylimmin[1], ylimmin[1] + 0.5))
      lines(x = c(0, 1), y = c(ylimmin[1] + 0.5, ylimmin[1] + 1))
    }
  }
  legend("bottom",
         inset = c(-0, -0.21), xpd = NA,
         legend = labnams[2:3],
         lty = 2,
         col = c("#61A60F", "#FFCA02"),
         bty = "n",
         cex = cexmy,
         horiz = TRUE
  )
}
