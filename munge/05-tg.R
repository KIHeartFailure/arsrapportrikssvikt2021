tg_lan <- full_join(
  inclan %>% mutate(inc = `2019`) %>% select(X1, inc),
  prevlan %>% mutate(prev = `2019`) %>% select(X1, prev),
  by = "X1"
) %>%
  filter(X1 != "RIKET") %>%
  mutate(
    region = str_sub(X1, 4),
    region = case_when(
      region == "Södermanland" ~ "Sörmanland",
      region == "Jämtland" ~ "Jämtland Härjedalen",
      TRUE ~ region
    )
  )

tmp_inc <- inclan %>%
  filter(X1 == "RIKET") %>%
  t() %>%
  as.data.frame() %>%
  rownames_to_column(var = "ar") %>%
  filter(V1 != "RIKET") %>%
  rename(inc = V1)

tmp_prev <- prevlan %>%
  filter(X1 == "RIKET") %>%
  t() %>%
  as.data.frame() %>%
  rownames_to_column(var = "ar") %>%
  filter(V1 != "RIKET") %>%
  rename(prev = V1)

tg_overtime <- full_join(tmp_inc, tmp_prev, by = "ar")

inc2019 <- inc2019 %>%
  rename(
    ninc = Antal...2,
    pinc = `%...3`
  ) %>%
  select(X1, ninc, pinc)


prev2019 <- prev2019 %>%
  rename(
    nprev = Antal...2,
    pprev = `%`
  ) %>%
  select(X1, nprev, pprev)

tg_sjukhustg <- full_join(inc2019, prev2019, by = "X1") %>%
  filter(!X1 %in% c("Övriga vårdenheter", "RIKET"))
