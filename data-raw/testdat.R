set.seed(323)

ids <- 1:20
tx_vec <- sample(c(0, 1), length(ids), replace = T)

id_vec <- rep(ids, sample(c(1, 2), length(ids), replace = T))

test_df <- data.frame(id = id_vec, time = unlist(sapply(table(id_vec), function(x) cumsum(rep(1, x)))),
                      tx = rep(tx_vec, table(id_vec)))


test_df$x1 <- rnorm(nrow(test_df))

test_df$x2 <- rexp(nrow(test_df))

test_df$x3 <- runif(nrow(test_df))


test_tables <- survey_table(test_df, vars = c("x1", "x2"), stat_func = compute_stats, condvar_wide = "time", condvar_long = "tx")

make_table(tables = test_tables, condvar_wide_labels = c("Time 1", "Time 2"), condvar_long_labels = c("No Tx", "Tx"),
           stat_labels = c("Mean", "SD", "N"), var_labels = c("X1", "X2"))


lapply(unique(test_df$time), function(x){

  temp_list <- lapply(unique(test_df$tx[test_df$time == x]), function(y){

    dat <- as.data.frame(t(sapply(test_df[test_df$time == x & test_df$tx == y, c("x1", "x2", "x3")], function(z) c(mean(z), sd(z)))))

    dat$group <- y
    dat$name <- c("x1", "x2", "x3")


    dat
  })

  temp_df <- Reduce(rbind, temp_list)

  temp_df <- temp_df[order(temp_df$name, temp_df$group), ]

  temp_df[, -ncol(temp_df)]


})

names(test_df)[grep("x1|2|3", names(test_df))] <- c("bp", "height", "score")

write.csv(test_df, "data-raw/test_df.csv")

usethis::use_data(test_df, overwrite = TRUE)
