## Test parse_algos() ----

combo_list <- list(
  combo1 = list(
    instruments = list("instr1"),
    rules = list("rule1")
  ),
  combo2 = list(
    instruments = list("instr2"),
    rules = list("rule2")
  )
)

unlist(combo_list$combo1$instruments)


algos <- list(
  list( ## We might name this "subset"
    instruments = list("instr1"),
    rules = list("rule1", "rule2")
  ),
)

expand.grid(algos[[1]])

## Don't use expand.grid(). Better use expand_algos() below...


## Expand algos ----

expand_algos <- function(algos) {
  #expanded_algos_as_dataframes <- list()
  expanded_algos <- list()
  expanded_algos_ID <- 1

  for(subset_ID in seq_along(algos)) {
    for(instrument in algos[[subset_ID]]$instruments) {
      for(rule in algos[[subset_ID]]$rules) {
        expanded_algos[[expanded_algos_ID]] <- list(
          instrument = instrument,
          rule = rule)
        expanded_algos_ID = expanded_algos_ID + 1
      }
    }
  }
  expanded_algos
}

algos <- list(
  list( ## We might name this "subset1"
    instruments = list("~/git/rsystrade/misc/temp/data/testdata1.csv"),
    rules = list("rule1", "rule2")
  ),
  list( ## We might name this "subset2"
    instruments = list("~/git/rsystrade/misc/temp/data/testdata2.csv"),
    rules = list("rule1", "rule2")
  )
)

expanded_algos <- expand_algos(algos)




## From parse_algos() ----

get_unique_instr_paths_from_expanded_algos_list <- function(expanded_algos) {
  ## Collect all instrument paths from expanded_algos
  instrument_paths <- list()
  for(i in seq_along(expanded_algos)) {
    instrument_paths[i] <- expanded_algos[[i]]$instrument
  }

  ## Make list of only unique paths
  unique_instrument_paths <- unique(instrument_paths)
  unique_instrument_paths
}

get_unique_instr_paths_from_expanded_algos_list(expanded_algos)





## Test parse_algos() ----

parse_algos <- function(algos, mode = "sim") {
  if(mode == "sim") {

    ## Expand algos nested list to all permutations of algos.
    expanded_algos <- expand_algos(algos)

    ## Collect all instrument paths from expanded_algos
    # instrument_paths <- list()
    # for(i in seq_along(expanded_algos)) {
    #   instrument_paths[i] <- expanded_algos[[i]]$instrument
    # }

    ## Make list of only unique paths
    #unique_instrument_paths <- unique(instrument_paths)
    unique_instrument_paths <-
      get_unique_instr_paths_from_expanded_algos_list(expanded_algos)

    ## Make list of only unique instrument data sets
    unique_instrument_datasets <- list()

    ## Read the data into the list
    for(i in seq_along(unique_instrument_paths)) {
      unique_instrument_datasets[[i]] <- data.frame(
        read.csv(unique_instrument_paths[[i]]))
    }

    ## Make a list of instrument datasets
    for(i in seq_along(expanded_algos)) {
      #path_i = algos[[i]]$instruments[[1]]
      expanded_algos[[i]]$path = expanded_algos[[i]]$instrument
      expanded_algos[[i]]$data <-
        unique_instrument_datasets[[which(unlist(unique_instrument_paths) ==
                                            expanded_algos[[i]]$path)]]

      ## Set instrument to instrument name instead of instrument data set path
      expanded_algos[[i]]$instrument <-
        tools::file_path_sans_ext(basename(expanded_algos[[i]]$path))

      ## Assign name as comment to data frame
      comment(expanded_algos[[i]]$data) <- expanded_algos[[i]]$instrument

      ## Now we can get the data frame name:
      ## > comment(expanded_algos[[1]]$data)
      ## [1] "instrument1"
    }

    ## Return list of all algos
    expanded_algos
  } else if (mode == "live") {
    stop("\"Live mode is not implemented yet.\"")
  } else {
    stop(
      paste("\nProvide a valid mode when running system().",
            "\nValid modes are \"sim\" (default) or \"live\".")
    )
  }
}

get_unique_instr_names_from_parsed_algos_list <- function(parsed_algos) {
  ## Collect all instrument paths from expanded_algos
  instrument_paths <- list()
  for(i in seq_along(parsed_algos)) {
    instrument_paths[i] <- parsed_algos[[i]]$instrument
  }

  ## Make list of only unique paths
  unique_instrument_paths <- unique(instrument_paths)
  unique_instrument_paths
}

get_num_instr_from_parsed_algos_list <- function(parsed_algos) {
  num_instr <-
    length(get_unique_instr_names_from_parsed_algos_list(parsed_algos))

  ## Check that num_instr is at least 1 and not NA.
  if(num_instr >= 1 && !is.na(num_instr)) {num_instr} else {
    stop("At least one instrument must be provided to the system.")
  }

}


write.csv(data.frame(x = rnorm(10)), "~/git/rsystrade/misc/temp/data/testdata1.csv")
write.csv(data.frame(x = rnorm(10)), "~/git/rsystrade/misc/temp/data/testdata2.csv")

algos <- list(
  list( ## We might name this "subset1"
    instruments = list("~/git/rsystrade/misc/temp/data/testdata1.csv"),
    rules = list("rule1", "rule1")
  ),
  list( ## We might name this "subset2"
    instruments = list("~/git/rsystrade/misc/temp/data/testdata2.csv"),
    rules = list("rule1", "rule2")
  )
)



parsed_algos <- parse_algos(algos, mode = "sim")

parsed_algos

get_unique_instr_names_from_parsed_algos_list(parsed_algos)

get_num_instr_from_parsed_algos_list(parsed_algos)



## Test parse_algos() with opening and closing rules ----

write.csv(data.frame(x = rnorm(10)), "~/git/rsystrade/misc/temp/data/testdata1.csv")
write.csv(data.frame(x = rnorm(10)), "~/git/rsystrade/misc/temp/data/testdata2.csv")

opening_rule_1 <- function(x) {
  print(paste("opening_rule_1:", x))
}

opening_rule_2 <- function(x) {
  print(paste("opening_rule_2:", x))
}

closing_rule_1 <- function(x) {
  print(paste("closing_rule_1:", x))
}

closing_rule_2 <- function(x) {
  print(paste("closing_rule_2:", x))
}


x1 <- 10
x2 <- 20
x3 <- 30
x4 <- 40
algos <- list(
  list( ## We might name this "subset1"
    instruments = list("~/git/rsystrade/misc/temp/data/testdata1.csv"),
    rules = list(
      list(opening_rule_1, closing_rule_1),
      list(opening_rule_2, closing_rule_2)
    )
  ),
  list( ## We might name this "subset2"
    instruments = list("~/git/rsystrade/misc/temp/data/testdata2.csv"),
    rules = list(
      c(opening_rule_1, closing_rule_1),
      c(opening_rule_2, closing_rule_2)
    )
  )
)

parsed_algos <- parse_algos(algos, mode = "sim")


## parse_algos() loads data to separate list (not into the algo list) ----

parse_algos <- function(algos, mode = "sim") {
  if(mode == "sim") {

    ## Expand algos nested list to all permutations of algos.
    expanded_algos <- expand_algos(algos)

    ## Collect all instrument paths from expanded_algos
    # instrument_paths <- list()
    # for(i in seq_along(expanded_algos)) {
    #   instrument_paths[i] <- expanded_algos[[i]]$instrument
    # }

    ## Make list of only unique paths given instrument names from input algos.
    ## This is only needed if we are loading the data into each algo. But we
    ## don't do that.
    #unique_instrument_paths <- unique(instrument_paths)
    # unique_instrument_paths <-
    #   get_unique_instr_paths_from_expanded_algos_list(expanded_algos)

    ## Make list of only unique instrument data sets
    ## This is only needed if we are loading the data into each algo. But we
    ## don't do that.
    # unique_instrument_datasets <- list()

    ## Read the data into the list.
    ## This is only needed if we are loading the data into each algo. But we
    ## don't do that.
    # for(i in seq_along(unique_instrument_paths)) {
    #   unique_instrument_datasets[[i]] <- data.frame(
    #     read.csv(unique_instrument_paths[[i]]))
    # }


    ## Make a list of instrument datasets
    #for(i in seq_along(expanded_algos)) {
      #path_i = algos[[i]]$instruments[[1]]
      ## This is only needed if we are loading the data into each algo. But we
      ## don't do that.
      #expanded_algos[[i]]$path = expanded_algos[[i]]$instrument

      ## Store the data for each algo inside the algo list.
      ## Note: We don't do that because if different rules are applied to the
      ## same data set, the same data set will be loaded multiple times (wich
      ## would work fine, but is not efficient).
      # expanded_algos[[i]]$data <-
      #   unique_instrument_datasets[[which(unlist(unique_instrument_paths) ==
      #                                       expanded_algos[[i]]$path)]]

      ## When input instrument list in algo contains a path instead of a name:
      ## Set instrument to instrument name instead of instrument data set path
      # expanded_algos[[i]]$instrument <-
      #   tools::file_path_sans_ext(basename(expanded_algos[[i]]$path))

      ## Assign name as comment to data frame.
      ## This is only needed if we are loading the data into each algo. But we
      ## don't do that.
      # comment(expanded_algos[[i]]$data) <- expanded_algos[[i]]$instrument

      ## Now we can get the data frame name:
      ## > comment(expanded_algos[[1]]$data)
      ## [1] "instrument1"
    #}

    ## Return list of all algos
    expanded_algos
  } else if (mode == "live") {
    stop("\"Live mode is not implemented yet.\"")
  } else {
    stop(
      paste("\nProvide a valid mode when running system().",
            "\nValid modes are \"sim\" (default) or \"live\".")
    )
  }
}

## Instruments are provided to algos a instrument names (character strings).
get_unique_instr_paths_from_expanded_algos_list <- function(
    expanded_algos,
    instrument_data_folder_path
    ) {
  ## Collect all instrument paths from expanded_algos
  instrument_paths <- list()
  for(i in seq_along(expanded_algos)) {
    instrument_paths[i] <-
      paste0(instrument_data_folder_path, expanded_algos[[i]]$instrument, ".csv")
  }

  ## Make list of only unique paths
  unique_instrument_paths <- unique(instrument_paths)
  unique_instrument_paths
}

get_unique_instr_names_from_parsed_algos_list <- function(parsed_algos) {
  ## Collect all instrument names from expanded_algos
  instrument_names <- list()
  for(i in seq_along(parsed_algos)) {
    instrument_names[i] <- parsed_algos[[i]]$instrument
  }

  ## Make list of only unique paths
  unique_instrument_names <- unique(instrument_names)
  unique_instrument_names
}

## Load data into system$instrument_data_sets list.
load_instrument_data_sets <- function(
    expanded_algos,
    instrument_data_folder_path
  ) {
  unique_instrument_paths <-
    get_unique_instr_paths_from_expanded_algos_list(
      expanded_algos,
      instrument_data_folder_path
    )
  unique_instrument_names <-
    get_unique_instr_names_from_parsed_algos_list(
      expanded_algos
    )
  unique_instrument_datasets <- list()
  for(i in seq_along(unique_instrument_paths)) {
    unique_instrument_datasets[[i]] <- data.frame(
      read.csv(unique_instrument_paths[[i]])
    )
    ## Assign name as comment to data frame.
    comment(unique_instrument_datasets[[i]]) <-  unique_instrument_names[[i]]
  }
  unique_instrument_datasets
}

write.csv(data.frame(x = rnorm(10)), "~/git/rsystrade/misc/temp/data/testdata1.csv")
write.csv(data.frame(x = rnorm(10)), "~/git/rsystrade/misc/temp/data/testdata2.csv")

system <- list()

opening_rule_1 <- function(x) {
  print(paste("opening_rule_1:", x))
}

opening_rule_2 <- function(x) {
  print(paste("opening_rule_2:", x))
}

closing_rule_1 <- function(x) {
  print(paste("closing_rule_1:", x))
}

closing_rule_2 <- function(x) {
  print(paste("closing_rule_2:", x))
}


x1 <- 10
x2 <- 20
x3 <- 30
x4 <- 40
system$algos <- list(
  list( ## We might name this "subset1"
    instruments = list("testdata1"),
    rules = list(
      list(opening_rule_1, closing_rule_1),
      list(opening_rule_2, closing_rule_2)
    )
  ),
  list( ## We might name this "subset2"
    instruments = list("testdata2"),
    rules = list(
      c(opening_rule_1, closing_rule_1),
      c(opening_rule_2, closing_rule_2)
    )
  )
)

## Parsed algos ----
system$algos <- parse_algos(system$algos, mode = "sim")

system$instrument_data_sets <- load_instrument_data_sets(
  expanded_algos = system$algos,
  instrument_data_folder_path = "~/git/rsystrade/misc/temp/data/"
)

### Opening rule of algo 1:
system$algos[[1]]$rules[[1]](x1)

### Closing rule of algo 1:
system$algos[[1]]$rules[[2]](x2)

### Opening rule of algo 2:
system$algos[[2]]$rules[[1]](x3)

## Closing rule of algo 2:
system$algos[[2]]$rules[[2]](x4)

x1 <- 100

### Opening rule of algo 3:
system$algos[[3]]$rules[[1]](x1)

system$instrument_data_sets[[1]]
system$instrument_data_sets[[2]]

comment(system$instrument_data_sets[[1]])
comment(system$instrument_data_sets[[2]])



## ----

get_unique_instr_names_from_parsed_algos_list <- function(parsed_algos) {
  ## Collect all instrument names from expanded_algos
  instrument_names <- list()
  for(i in seq_along(parsed_algos)) {
    instrument_names[i] <- parsed_algos[[i]]$instrument
  }

  ## Make list of only unique names
  unique_instrument_names <- unique(instrument_names)
  unique_instrument_names
}

get_instr_names_by_parsed_algo <- function(parsed_algos) {
  ## Collect all instrument names from expanded_algos
  instrument_names <- list()
  for(i in seq_along(parsed_algos)) {
    instrument_names[i] <- parsed_algos[[i]]$instrument
  }

  ## Make list of only instrument names
  instrument_names
}

system <- list(
  algos = list(
    list(instrument = "instr1", rule = "rule1"),
    list(instrument = "instr1", rule = "rule2"),
    list(instrument = "instr2", rule = "rule1"),
    list(instrument = "instr2", rule = "rule2")
  ),
  signal_tables = list(
    data.frame(a = 101:110, b = 111:120, c = 121:130),
    data.frame(a = 201:210, b = 211:220, c = 221:230),
    data.frame(a = 301:310, b = 311:320, c = 321:330)
  )
)

system$algos
system$algos[[1]]$instrument

unique_instr_names <- get_unique_instr_names_from_parsed_algos_list(system$algos)
instr_names <- get_instr_names_by_parsed_algo(system$algos)

unique_instr_names
instr_names

instr_names[which(unlist(instr_names) == unique_instr_names[[1]])]
instr_names[which(unlist(instr_names) == unique_instr_names[[2]])]

which(unlist(instr_names) == unique_instr_names[[1]])
which(unlist(instr_names) == unique_instr_names[[2]])






## ----

tmp_list <- list("a" = 1, "b" = 2, "c" = 3, "d" = 4)

tmp_list[list(
  "a",
  "c"
)] <- list(10, 30)
tmp_list




##

## Test function structure ----

f <- function(x) {
  y <- 14
  g <- function(...){
    print(y)
  }
  h <- function(...) {
    z <- x + y
    print(eval(substitute(alist(...))))
    print(z)
  }
  k <- function(...) {
    print(eval(substitute(alist(...))))
  }

  print(x)
  y <- 12
  g(y)
  h(x = 100)
  k(x, y)
}

f(10)




## Pipes ----

nrows <- 5
ncols <- 3
df <- data.frame(matrix(rep(2, nrows * ncols ), ncol = ncols))


modify_col1 <- function(row_n, prev_row) {
  row_n[[1]] <- prev_row[[1]]
  row_n
}
modify_col2 <- function(row_n, prev_row) {
  row_n[[2]] <- prev_row[[2]] + log(row_n[[1]])
  row_n
}
modify_col3 <- function(row_n, prev_row) {
  row_n[[3]] <- prev_row[[3]] + log(row_n[[2]])
  row_n
}

modify_df <- function(df) {
  for(i in 2:nrow(df)) {
    df[i, ] <- df[i, ]  %>%
      modify_col1(., prev_row = df[i-1, ]) %>%
      modify_col2(., prev_row = df[i-1, ]) %>%
      modify_col3(., prev_row = df[i-1, ])
  }
  df
}


version4(df)

library(purrr)

## Progress bar ----

# Create a list of numbers to square
numbers <- 1:10

# Use map() to square each number in the list
squares <- map(numbers, function(x) {
  x^2
}, .progress =TRUE)

# View the resulting list of squares
squares



## ----

f1 <- function() {rnorm(20)}
f2 <- function() {runif(20)}
f_list <- list(f1 = "f1", f2 = "f2")
df_test <- map_df(10^(1:2), run_benchmark, ncol = 3, functions = f_list, times = 1) %>%order_graphs()
plot_bm(df_test, "nrow", mean_or_median = median)




## Test `calculate_normalization_factor()` ----
## Se also "Volatility Targeting" below

## Result varies wildly when mean of input signal is close to 0
for(x in replicate(10, calculate_normalization_factor(rnorm(100, 0, 1)))) {
  print(x)
}

# [1] 15.19099
# [1] 8.814055
# [1] 11.69273
# [1] 10.34344
# [1] 5.220538
# [1] 152.523
# [1] 5.964465
# [1] 90.6625
# [1] 19.78461
# [1] 69.47908



## Mean close to 10
for(x in replicate(10, calculate_normalization_factor(rnorm(100, 10, 1)))) {
  print(x)
}
# [1] 0.09961508
# [1] 0.09931197
# [1] 0.1000919
# [1] 0.09885538
# [1] 0.09967997
# [1] 0.1000712
# [1] 0.1004248
# [1] 0.100403
# [1] 0.09969977
# [1] 0.09868076


## Mean close to 1
for(x in replicate(10, calculate_normalization_factor(rnorm(100, 1, 1)))) {
  print(x)
}
# [1] 1.038298
# [1] 1.115135
# [1] 0.9769068
# [1] 1.025624
# [1] 1.111125
# [1] 0.9964326
# [1] 1.234892
# [1] 0.9007888
# [1] 1.013072
# [1] 0.9909893


## Cap signal
cap(4, -2, 2)
cap(NA, -2, 2)




## Correlations ----

parse_algos <- function(algos, mode = "sim") {
  if(mode == "sim") {

    ## Expand algos nested list to all permutations of algos.
    expanded_algos <- expand_algos(algos)

    ## Collect all instrument paths from expanded_algos
    # instrument_paths <- list()
    # for(i in seq_along(expanded_algos)) {
    #   instrument_paths[i] <- expanded_algos[[i]]$instrument
    # }

    ## Make list of only unique paths given instrument names from input algos.
    ## This is only needed if we are loading the data into each algo. But we
    ## don't do that.
    #unique_instrument_paths <- unique(instrument_paths)
    # unique_instrument_paths <-
    #   get_unique_instr_paths_from_expanded_algos_list(expanded_algos)

    ## Make list of only unique instrument data sets
    ## This is only needed if we are loading the data into each algo. But we
    ## don't do that.
    # unique_instrument_datasets <- list()

    ## Read the data into the list.
    ## This is only needed if we are loading the data into each algo. But we
    ## don't do that.
    # for(i in seq_along(unique_instrument_paths)) {
    #   unique_instrument_datasets[[i]] <- data.frame(
    #     read.csv(unique_instrument_paths[[i]]))
    # }


    ## Make a list of instrument datasets
    #for(i in seq_along(expanded_algos)) {
    #path_i = algos[[i]]$instruments[[1]]
    ## This is only needed if we are loading the data into each algo. But we
    ## don't do that.
    #expanded_algos[[i]]$path = expanded_algos[[i]]$instrument

    ## Store the data for each algo inside the algo list.
    ## Note: We don't do that because if different rules are applied to the
    ## same data set, the same data set will be loaded multiple times (wich
    ## would work fine, but is not efficient).
    # expanded_algos[[i]]$data <-
    #   unique_instrument_datasets[[which(unlist(unique_instrument_paths) ==
    #                                       expanded_algos[[i]]$path)]]

    ## When input instrument list in algo contains a path instead of a name:
    ## Set instrument to instrument name instead of instrument data set path
    # expanded_algos[[i]]$instrument <-
    #   tools::file_path_sans_ext(basename(expanded_algos[[i]]$path))

    ## Assign name as comment to data frame.
    ## This is only needed if we are loading the data into each algo. But we
    ## don't do that.
    # comment(expanded_algos[[i]]$data) <- expanded_algos[[i]]$instrument

    ## Now we can get the data frame name:
    ## > comment(expanded_algos[[1]]$data)
    ## [1] "instrument1"
    #}

    ## Return list of all algos
    expanded_algos
  } else if (mode == "live") {
    stop("\"Live mode is not implemented yet.\"")
  } else {
    stop(
      paste("\nProvide a valid mode when running system().",
            "\nValid modes are \"sim\" (default) or \"live\".")
    )
  }
}

get_unique_instr_paths_from_expanded_algos_list <- function(
    expanded_algos,
    instrument_data_folder_path
) {
  ## Collect all instrument paths from expanded_algos
  instrument_paths <- list()
  for(i in seq_along(expanded_algos)) {
    instrument_paths[i] <-
      paste0(instrument_data_folder_path, expanded_algos[[i]]$instrument, ".csv")
  }

  ## Make list of only unique paths
  unique_instrument_paths <- unique(instrument_paths)
  unique_instrument_paths
}

get_unique_instr_names_from_parsed_algos_list <- function(parsed_algos) {
  ## Collect all instrument names from expanded_algos
  instrument_names <- list()
  for(i in seq_along(parsed_algos)) {
    instrument_names[i] <- parsed_algos[[i]]$instrument
  }

  ## Make list of only unique paths
  unique_instrument_names <- unique(instrument_names)
  unique_instrument_names
}

## Load data into system$instrument_data_sets list.
load_instrument_data_sets <- function(
    expanded_algos,
    instrument_data_folder_path
) {
  unique_instrument_paths <-
    get_unique_instr_paths_from_expanded_algos_list(
      expanded_algos,
      instrument_data_folder_path
    )
  unique_instrument_names <-
    get_unique_instr_names_from_parsed_algos_list(
      expanded_algos
    )
  unique_instrument_datasets <- list()
  for(i in seq_along(unique_instrument_paths)) {
    unique_instrument_datasets[[i]] <- data.frame(
      read.csv(unique_instrument_paths[[i]])
    )
    ## Assign name as comment to data frame.
    comment(unique_instrument_datasets[[i]]) <-  unique_instrument_names[[i]]
  }
  unique_instrument_datasets
}

write.csv(data.frame(
    label = letters[1:10],
    price = round(rnorm(10), 2)
  ), "~/git/rsystrade/misc/temp/data/testdata1.csv"
)
write.csv(data.frame(
    label = letters[1:10],
    price = round(rnorm(10), 2)
    ), "~/git/rsystrade/misc/temp/data/testdata2.csv"
)

system <- list()

opening_rule_1 <- function(x) {
  print(paste("opening_rule_1:", x))
}

opening_rule_2 <- function(x) {
  print(paste("opening_rule_2:", x))
}

closing_rule_1 <- function(x) {
  print(paste("closing_rule_1:", x))
}

closing_rule_2 <- function(x) {
  print(paste("closing_rule_2:", x))
}


x1 <- 10
x2 <- 20
x3 <- 30
x4 <- 40
system$algos <- list(
  list( ## We might name this "subset1"
    instruments = list("testdata1"),
    rules = list(
      list(opening_rule_1, closing_rule_1),
      list(opening_rule_2, closing_rule_2)
    )
  ),
  list( ## We might name this "subset2"
    instruments = list("testdata2"),
    rules = list(
      c(opening_rule_1, closing_rule_1),
      c(opening_rule_2, closing_rule_2)
    )
  )
)

## Parsed algos ----
system$algos <- parse_algos(system$algos, mode = "sim")

system$instrument_data_sets <- load_instrument_data_sets(
  expanded_algos = system$algos,
  instrument_data_folder_path = "~/git/rsystrade/misc/temp/data/"
)


price_vectors <- data.frame(
  sapply(system$instrument_data_sets, function(x) list(price = x$price))
)

instrument_list <- sapply(system$instrument_data_sets, function(x) comment(x))
instrument_list
names(price_vectors) <- instrument_list
str(price_vectors)
typeof(system$instrument_data_sets)
price_vectors[, 2]

instrument_correlations <- cor(price_vectors)

num_instruments <- 3
matrix(rep(NA, num_instruments * num_instruments), nrow = num_instruments)


## SDM----

f_sig_div_mult <- function(signal_correlations, signal_weights) {
  H <- signal_correlations
  W <- signal_weights
  crossprod(t(W %*% H),  W)
}

signal_1 <- round(rnorm(1000), 2)
signal_2 <- round(rnorm(1000), 2)

signals <- data.frame(
  x1 = signal_1,
  x2 = signal_2
)

cor(signal_1, signal_2)
cor(signals)

H <- cor(signals)
W <- c(0.5, 0.5)

f_sig_div_mult(H, W)


lower_cap_signal <- function(signal, min_signal) {
  max(min_signal, signal)
}

lower_cap_matrix <- function(input, min_signal) {
  apply(H,
        c(1,2),
        lower_cap_signal,
        min_signal = min_signal
  )
}
lower_cap_matrix(H, 0.5)

lower_cap_signal(H, min_signal = 0)
H

f_sig_div_mult <- function(
    signal_correlations,
    signal_weights,
    min = 0,
    max = 2.5) {
  H <- signal_correlations
  W <- signal_weights

  capped_H <- lower_cap_signal(H, min_signal = min)
  upper_cap_signal(
    crossprod(t(W %*% capped_H),  W),
    max_signal = max
  )
}








