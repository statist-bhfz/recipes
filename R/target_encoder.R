


res <- gather(select(x, one_of(cols, target)),
              key = "variable",
              value = "value",
              tidyselect::one_of(cols)) %>%
  group_by(variable, value) %>%
  summarise(mean = mean(!!sym(target)))


by_pair <- "value"
names(by_pair) <- cols[1]

rename_pair <- list(sym("mean"))
names(rename_pair) <- paste0(cols[1], "_mean")

left_join(select(x, tidyselect::one_of(cols[1])),
          filter(res, variable == cols[1]),
          by = by_pair) %>%
  select(mean) %>%
  rename(!!!rename_pair)
