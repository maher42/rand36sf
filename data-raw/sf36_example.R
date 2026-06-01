## The `sf36_example` dataset

opts <- c(5, 5, rep(3, 10), rep(2, 7),
          5, 6, 5, 6, 6, 6, 6, 6, 6, 6, 6, 6, 5, 5, 5, 5, 5)

# 6 respondents, each measured at 2 timepoints (12 rows).
set.seed(42)
n <- 12
items <- as.data.frame(lapply(opts, function(m) sample.int(m, n, replace = TRUE)))
names(items) <- paste0("item", seq_len(36))

sf36_example <- cbind(
  id        = rep(1:6, each = 2),
  timepoint = rep(1:2, times = 6),
  items
)

usethis::use_data(sf36_example, overwrite = TRUE)
