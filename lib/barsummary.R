barsummary <- function(qi,
                       startime = ar - 2, stoptime = ar,
                       ll = NULL, ul = NULL,
                       unit,
                       data = rsdata) {
  tmp <- data %>%
    filter(indexyear >= startime &
      indexyear <= stoptime &
      !is.na(!!sym(qi)) &
      vtype %in% unit) %>%
    mutate(
      indexyear = factor(indexyear),
      !!sym(qi) := factor(!!sym(qi))
    )

  all <- tmp %>%
    group_by(ttype, indexyear, .drop = F) %>%
    count(!!sym(qi), .drop = F) %>%
    mutate(
      tot = sum(n),
      percent = as.numeric(fn(n / tot * 100, 0))
    ) %>%
    ungroup() %>%
    filter(!!sym(qi) == 1)

  all <- all %>%
    mutate(
      cols = case_when(
        indexyear == startime ~ "grey75",
        indexyear == startime + 1 ~ "grey55",
        TRUE ~ global_cols[2]
      ),
      ntot = paste0(n, " av ", tot),
      per = paste0(percent, "%"),
      per = if_else(n < 10, "", per),
      ntot = if_else(n < 10, "", ntot),
      percent = if_else(n < 10, 0, percent),
      row = 1:n()
    )

  cexmy <- .75
  # c(bottom, left, top, right)
  par(mar = c(3, 4, 3, 0) + 0.1)

  b <- barplot(percent ~ indexyear + ttype,
    data = all,
    beside = TRUE,
    axes = FALSE,
    ylab = "Procent",
    xlab = "",
    col = all$cols,
    border = "white",
    names.arg = c(NA, NA, NA, NA),
    cex.lab = cexmy,
    ylim = c(0, 100)
  )

  axis(2, seq(0, 100, 20), cex.axis = cexmy, las = 2)

  abline(h = ll * 100, col = "#FFCA02", lty = 2, lwd = 1)
  abline(h = ul * 100, col = "#61A60F", lty = 2, lwd = 1)

  axis(1, at = b, labels = all$indexyear, line = 0, tick = FALSE, cex.axis = cexmy, gap.axis = -10000000)

  axis(3, at = b, labels = all$per, line = 0, tick = FALSE, cex.axis = cexmy, hadj = 0.5, gap.axis = -10000000)

  axis(1, at = b[2, ], labels = levels(all$ttype), line = 1, tick = FALSE, cex.axis = cexmy, gap.axis = -10000000)
}
