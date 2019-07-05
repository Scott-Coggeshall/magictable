set.seed(323)

ids <- 1:20
tx_vec <- sample(c(0, 1), length(ids), replace = T)

id_vec <- rep(ids, sample(c(1, 2), length(ids), replace = T))

test_df <- data.frame(id = id_vec, time = unlist(sapply(table(id_vec), function(x) cumsum(rep(1, x)))),
                      tx = rep(tx_vec, table(id_vec)))


test_df$x1 <- rnorm(nrow(test_df))

test_df$x2 <- rexp(nrow(test_df))

test_df$x3 <- runif(nrow(test_df))

test_df$x4 <- factor(sample(c("none", "low", "medium", "high"), nrow(test_df), replace = T, prob = c(.1, .3, .4, .2)))

test_df$x5 <- factor(sample(c("white", "black", "other"), nrow(test_df), replace = T, prob = c(.7, .2, .1)))

names(test_df)[grep("x1|2|3|4|5", names(test_df))] <- c("bp", "height", "score", "severity", "race")

write.csv(test_df, "data-raw/test_df.csv")

usethis::use_data(test_df, overwrite = TRUE)
