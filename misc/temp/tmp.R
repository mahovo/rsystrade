# Test parse_algos() ----

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


# Expand algos ----

source("/Users/mhvpbp13/git/rsystrade/R/parse_algos.R")

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




# From parse_algos() ----

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





# Test parse_algos() ----

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



# Test parse_algos() with opening and closing rules ----

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




# parse_algos() loads data to separate list (not into the algo list) ----

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

# Parsed algos ----
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



# ----

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






# ----

tmp_list <- list("a" = 1, "b" = 2, "c" = 3, "d" = 4)

tmp_list[list(
  "a",
  "c"
)] <- list(10, 30)
tmp_list



# Test function structure ----

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


##

f <- function(x, ...) {

  h <- function(x, y = 14) {
    z <- x + y
    print(z)
  }

  h(x, ...)
}

f(x = 10)


##

f <- function() {
  y <<- 10
  print(g())
}
g <- function() {
  y
}
f()


##

f <- function(x, ...) {

  switch(
    (eval(substitute(alist(...))))$mode,
    "a" = print(x),
    "b" = print((eval(substitute(alist(...))))$y)
  )
}
f(x = 10, mode = "b", y = 20)



##

f <- function(...) {
  print(alist(...))
  print(substitute(alist(...)))
  print(eval(substitute(alist(...))))
}
f(x = 10, y = 20)


##


f <- function(x, y, ...) {
  print(x)
  g(y,  ...)
}
g <- function(y, ...) {
  print(y)
  h1 <- function(z1 = "z1") {
    print(z1)
  }
  h2 <- function(z2 = "z2") {
    print(z2)
  }
  h1(...)
  h2(...)
}

f(x = "x", y = "y")
f(x = "x", y = "y", z = "zz")


##

config <- list(
  signal_normalization_factors_method = "equal",
  signal_normalization_factors_args = list(
    equal = list(equal_norm_factor = 100, bla = 200),
    median_pool_all = list(min_periods_median_pool_all = 250)
  )
)


update_signal_normalization_factors <- function(
    parsed_algos,
    signal_tables,
    instrument_data_sets,
    target,
    method,
    ...
  ) {

  print(list(...))
  print(signal_tables)
  print(instrument_data_sets)
  print(target)
  print(method)
  equal <- function(parsed_algos, args = list(equal_norm_factor = 1, bla = 2)) {
    print(parsed_algos)
    print(paste("equal_norm_factor:", args$equal_norm_factor))
    print(paste("bla:", args$bla))
  }
  equal(
    parsed_algos,
    ...)
}

update_signal_normalization_factors(
  parsed_algos = "parsed_algos",
  signal_tables = "signal_tables",
  instrument_data_sets = "instrument_data_sets",
  target = 1,
  method = "equal",
  args = config$signal_normalization_factors_args[[config$signal_normalization_factors_method]]
)



# Pipes ----

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

# Progress bar ----

# Create a list of numbers to square
numbers <- 1:10

# Use map() to square each number in the list
squares <- map(numbers, function(x) {
  x^2
}, .progress =TRUE)

# View the resulting list of squares
squares



# ----

f1 <- function() {rnorm(20)}
f2 <- function() {runif(20)}
f_list <- list(f1 = "f1", f2 = "f2")
df_test <- map_df(10^(1:2), run_benchmark, ncol = 3, functions = f_list, times = 1) %>%order_graphs()
plot_bm(df_test, "nrow", mean_or_median = median)




# Test `calculate_normalization_factor()` ----
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




# Correlations ----

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



# Parsed algos ----
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


# SDM ----

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




# Signal weights  ----

get_num_rules_per_inst_from_parsed_algos <- function(parsed_algos) {
  inst_names <- unlist(get_inst_names_by_parsed_algo(parsed_algos))
  unique_names <- unique(inst_names)

  ## List of numbers of rules for each instrument.
  ## One named item for each instrument. The name of the item is the name of
  ## the instrument.
  ## Each item in the list is a number of rules for the corresponding
  ## instrument.
  num_rules <- numeric(length(unique_names))
  names(num_rules) <- unique_names

  for(i in seq_along(unique_names)) {
    num_rules[i] <- sum(inst_names == unique_names[i])
  }
  num_rules
}

get_inst_names_by_parsed_algo <- function(parsed_algos) {
  ## Collect all instrument names from expanded_algos
  instrument_names <- list()
  for(i in seq_along(parsed_algos)) {
    instrument_names[i] <- parsed_algos[[i]]$instrument
  }

  ## Make list of only instrument names
  instrument_names
}


opening_rule_1 <- function(x) {
  print(paste("opening_rule_1:", x))
}

opening_rule_2 <- function(x) {
  print(paste("opening_rule_2:", x))
}

opening_rule_3 <- function(x) {
  print(paste("opening_rule_3:", x))
}

closing_rule_1 <- function(x) {
  print(paste("closing_rule_1:", x))
}

closing_rule_2 <- function(x) {
  print(paste("closing_rule_2:", x))
}

closing_rule_3 <- function(x) {
  print(paste("closing_rule_3:", x))
}

algos <- list(
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
      c(opening_rule_2, closing_rule_2),
      c(opening_rule_3, closing_rule_3)
    )
  )
)

parsed_algos <- parse_algos(algos)

get_num_rules_per_inst_from_parsed_algos(parsed_algos)




calculate_equal_signal_weights <- function(parsed_algos) {
  num_signals <- get_num_rules_per_inst_from_parsed_algos(parsed_algos)
  weights_by_instrument <- numeric(length(num_signals))
  for(i in seq_along(num_signals)) {
    if(!is.na(num_signals[[i]]) && num_signals[[i]] >= 1) {
      weights_by_instrument[i] <- 1/num_signals[[i]]
    } else {
      stop("At least one rule must be provided to the system.")
    }
  }

  weights_by_algo <- numeric(length(algos))

  id <- 1
  for(i in seq_along(num_signals)) {
    for(j in 1:num_signals[[i]]) {
      weights_by_algo[[id]] <- weights_by_instrument[[i]]
      id <- id + 1
    }
  }
  weights_by_algo
}

calculate_equal_signal_weights(parsed_algos)


# Instrument weights ----

calculate_equal_inst_weights <- function(parsed_algos) {
  n <- get_num_inst_from_parsed_algos_list(algos)
  rep(1/n, n)
}

opening_rule_1 <- function(x) {
  print(paste("opening_rule_1:", x))
}

opening_rule_2 <- function(x) {
  print(paste("opening_rule_2:", x))
}

opening_rule_3 <- function(x) {
  print(paste("opening_rule_3:", x))
}

closing_rule_1 <- function(x) {
  print(paste("closing_rule_1:", x))
}

closing_rule_2 <- function(x) {
  print(paste("closing_rule_2:", x))
}

closing_rule_3 <- function(x) {
  print(paste("closing_rule_3:", x))
}

algos <- list(
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
      c(opening_rule_2, closing_rule_2),
      c(opening_rule_3, closing_rule_3)
    )
  )
)

parsed_algos <- parse_algos(algos)

calculate_equal_inst_weights(parsed_algos)




# Exponential Weighted Average ----

n <- 200
L <- 25
x <- rnorm(n, 0, 4)
A <- 2 / (1 + n)

sum(A^(1:(n-1) - 1)) / sum(A^(0:(n-2)))

f_ewa <- function(x, lookback) {
  L <- lookback
  A <- 2/(1 + L)
  x_window <- tail(x, L)
  w <- A^((0):(L - 1))
  (sum(w %*% x_window)) / sum(w)
}

f_ewa(x, L)


set.seed(12345)
n <- 100
x <- rnorm(n)
y <- x + rnorm(n)

# Compute the exponential weighted correlation matrix
f_ewa <- function(x, lookback = NA) {
  if(is.na(lookback)) {
    L <- length(x)
    x_window <- x
  } else {
    L <- lookback
    x_window <- tail(x, L)
  }
  lambda <- 2/(1 + L)
  w <- lambda^((0):(L - 1))
  (sum(w %*% x_window)) / sum(w)
}

f_ewcor <- function(x, y, lookback) {
  L <- lookback
  x_window <- tail(x, L)
  y_window <- tail(y, L)
  xc <- f_ewa(x)
  yc <- f_ewa(y)
  num <- f_ewa((x - xc) * (y - yc))
  denom <- sqrt(f_ewa((x - xc)^2) * f_ewa((y - yc)^2))
  num / denom
}

ewcor_matrix <- function(data, lookback = NA) {
  if(is.na(lookback)) {
    L <- nrow(data)
    data_window <- data
  } else {
    L <- lookback
    data_window <- tail(data, L)
  }
  m <- ncol(data)
  cor_mat <- matrix(0, ncol=m, nrow=m)
  for (i in 1:m) {
    for (j in 1:m) {
      cor_mat[i, j] <- f_ewcor(data_window[ , i], data_window[ , j], L)
    }
  }
  cor_mat
}


data <- data.frame(x, y)
ewcor_matrix(data, 25)




# `signal_normalization_factors()` ----


get_rule_names_by_parsed_algo <- function(parsed_algos) {
  ## Collect all instrument names from expanded_algos
  rule_names <- list()
  for(i in seq_along(parsed_algos)) {
    rule_names[i] <- paste0(
      "[",
      names(parsed_algos[[i]]$rule[1]),
      ", ",
      names(parsed_algos[[i]]$rule[2]),
      "]"
    )
  }

  ## Make list of only instrument names
  rule_names
}


get_unique_rule_names_from_parsed_algos_list <- function(parsed_algos) {
  ## Collect all instrument names from expanded_algos
  rule_names <- get_rule_names_by_parsed_algo(parsed_algos)

  ## Make list of only unique names
  unique(rule_names)
}


get_num_rules_from_parsed_algos_list <- function(parsed_algos) {
  num_rules <-
    length(get_unique_rule_names_from_parsed_algos_list(parsed_algos))

  ## Check that num_instr is at least 1 and not NA.
  if(num_rules >= 1 && !is.na(num_rules)) {num_rules} else {
    stop("At least one rule must be provided to the system.")
  }

}

opening_rule_1 <- function(x) {
  print(paste("opening_rule_1:", x))
}

opening_rule_2 <- function(x) {
  print(paste("opening_rule_2:", x))
}

opening_rule_3 <- function(x) {
  print(paste("opening_rule_3:", x))
}

closing_rule_1 <- function(x) {
  print(paste("closing_rule_1:", x))
}

closing_rule_2 <- function(x) {
  print(paste("closing_rule_2:", x))
}

closing_rule_3 <- function(x) {
  print(paste("closing_rule_3:", x))
}

algos <- list(
  list( ## We might name this "subset1"
    instruments = list("testdata1"),
    rules = list(
      list("or1" = opening_rule_1, "cr1" = closing_rule_1),
      list("or2" = opening_rule_2, "cr2" = closing_rule_2)
    )
  ),
  list( ## We might name this "subset2"
    instruments = list("testdata2"),
    rules = list(
      c("or1" = opening_rule_1, "cr1" = closing_rule_1),
      c("or2" = opening_rule_2, "cr2" = closing_rule_2),
      c("or3" = opening_rule_3, "cr3" = closing_rule_3)
    )
  )
)

parsed_algos <- parse_algos(algos)

number_of_unique_rules <- get_num_rules_from_parsed_algos_list(parsed_algos)
signal_normalization_factors <- rep(NA, number_of_unique_rules)
names(signal_normalization_factors) <- get_unique_rule_names_from_parsed_algos_list(parsed_algos)
signal_normalization_factors


# ---- Get all past signals

sig_tabs <- list(
  data.frame("raw_signal" = c(1,2,3), "bla" = "blabla"),
  data.frame("raw_signal" = c(4,5,6), "bla" = "blabla"),
  data.frame("raw_signal" = c(7,8,9), "bla" = "blabla")
)

get_all_past_raw_signals <- function(signal_tables) {
  lapply(signal_tables, function(signal_table) {
    signal_table$raw_signal
  })
}

get_all_past_raw_signals(sig_tabs)


# Parse string as function ----

f <- function(x) {print(x)}

f_ <- "f"

ff <- eval(parse(text = f_))
ff(10)



# Get all signals for each rule ----

get_rule_names_by_parsed_algo <- function(parsed_algos) {
  ## Collect all instrument names from expanded_algos
  rule_names <- list()
  for(i in seq_along(parsed_algos)) {
    rule_names[i] <- paste0(
      "[",
      names(parsed_algos[[i]]$rule[1]),
      ", ",
      names(parsed_algos[[i]]$rule[2]),
      "]"
    )
  }

  ## Make list of only instrument names
  rule_names
}


get_unique_rule_names_from_parsed_algos_list <- function(parsed_algos) {
  ## Collect all instrument names from expanded_algos
  rule_names <- get_rule_names_by_parsed_algo(parsed_algos)

  ## Make list of only unique names
  unique(rule_names)
}


get_num_rules_from_parsed_algos_list <- function(parsed_algos) {
  num_rules <-
    length(get_unique_rule_names_from_parsed_algos_list(parsed_algos))

  ## Check that num_instr is at least 1 and not NA.
  if(num_rules >= 1 && !is.na(num_rules)) {num_rules} else {
    stop("At least one rule must be provided to the system.")
  }

}

opening_rule_1 <- function(x) {
  print(paste("opening_rule_1:", x))
}

opening_rule_2 <- function(x) {
  print(paste("opening_rule_2:", x))
}

opening_rule_3 <- function(x) {
  print(paste("opening_rule_3:", x))
}

closing_rule_1 <- function(x) {
  print(paste("closing_rule_1:", x))
}

closing_rule_2 <- function(x) {
  print(paste("closing_rule_2:", x))
}

closing_rule_3 <- function(x) {
  print(paste("closing_rule_3:", x))
}

algos <- list(
  list( ## We might name this "subset1"
    instruments = list("testdata1"),
    rules = list(
      list("or1" = opening_rule_1, "cr1" = closing_rule_1),
      list("or2" = opening_rule_2, "cr2" = closing_rule_2)
    )
  ),
  list( ## We might name this "subset2"
    instruments = list("testdata2"),
    rules = list(
      c("or1" = opening_rule_1, "cr1" = closing_rule_1),
      c("or2" = opening_rule_2, "cr2" = closing_rule_2),
      c("or3" = opening_rule_3, "cr3" = closing_rule_3)
    )
  )
)

parsed_algos <- parse_algos(algos)

system <- list()
system$algos <- parsed_algos
system$signal_tables <- list(
    data.frame("raw_signal" = 1:20, "bla" = 1:20),
    data.frame("raw_signal" = 1:20*2, "bla" = 1:20/2),
    data.frame("raw_signal" = 1:20*3, "bla" = 1:20/3),
    data.frame("raw_signal" = 1:20*4, "bla" = 1:20/4),
    data.frame("raw_signal" = 1:20*5, "bla" = 1:20/5),
    data.frame("raw_signal" = 1:20*6, "bla" = 1:20/6)
  )

## Get rule name for each algo in the order they appear in the algos list
rule_names_by_algo <- get_rule_names_by_parsed_algo(parsed_algos)
rule_names_by_algo

## Get all unique rule names in a list
all_unique_rule_names <- get_unique_rule_names_from_parsed_algos_list(parsed_algos)
all_unique_rule_names

## For each unique rule name, get the IDs of that rule in the
## rule_names_by_algo list
IDs_grouped_by_rule_names <- split(
  seq_along(rule_names_by_algo),
  unlist(rule_names_by_algo)
)
IDs_grouped_by_rule_names

## For each unique rule name, get all signals produced by that rule.
## We are assuming that algos are in the same order as signal tables
raw_signals_list <- list()
for(rule in unlist(all_unique_rule_names)) {
  raw_signals_list[[rule]] <- lapply(
    system$signal_tables[unlist(IDs_grouped_by_rule_names[rule])],
    function(x) {x$raw_signal}
  )
}

raw_signals_list
str(raw_signals_list)


# Test pool_traded() ----

opening_rule_1 <- function(x) {
  x
}

opening_rule_2 <- function(x) {
  x
}

opening_rule_3 <- function(x) {
  x
}

closing_rule_1 <- function(x) {
  x
}

closing_rule_2 <- function(x) {
  x
}

closing_rule_3 <- function(x) {
  x
}

algos <- list(
  list( ## We might name this "subset1"
    instruments = list("testdata1"),
    rules = list(
      list("or1" = opening_rule_1, "cr1" = closing_rule_1),
      list("or2" = opening_rule_2, "cr2" = closing_rule_2)
    )
  ),
  list( ## We might name this "subset2"
    instruments = list("testdata2"),
    rules = list(
      c("or1" = opening_rule_1, "cr1" = closing_rule_1),
      c("or2" = opening_rule_2, "cr2" = closing_rule_2),
      c("or3" = opening_rule_3, "cr3" = closing_rule_3)
    )
  )
)

parsed_algos <- parse_algos(algos)


f_indiv_normalization_factor <- function(raw_signal, target = 1) {
  target/mean(abs(raw_signal))
}

pool_traded <- function(signal_tables, algos) {
  #algos <- parsed_algos
  ## Get rule name for each algo in the order they appear in the algos list
  rule_names_by_algo <- get_rule_names_by_parsed_algo(algos)
  #rule_names_by_algo

  ## Get all unique rule names in a list
  all_unique_rule_names <- get_unique_rule_names_from_parsed_algos_list(algos)
  #all_unique_rule_names

  ## For each unique rule name, get the IDs of that rule in the
  ## rule_names_by_algo list
  IDs_grouped_by_rule_names <- split(
    seq_along(rule_names_by_algo),
    unlist(rule_names_by_algo)
  )
  #IDs_grouped_by_rule_names

  ## For each unique rule name, get all signals produced by that rule.
  ## We are assuming that algos are in the same order as signal tables
  raw_signals_list <- list()
  for(rule in unlist(all_unique_rule_names)) {
    raw_signals_list[[rule]] <- lapply(
      system$signal_tables[unlist(IDs_grouped_by_rule_names[rule])],
      function(x) {x$raw_signal}
    )
  }
  raw_signals_list

  lapply(raw_signals_list,
         function(raw_signal) {
           f_indiv_normalization_factor(unlist(raw_signal))
         }
  )
}

pool_traded(system$signal_tables, parsed_algos)


# Test pool_traded() - data frame version ----

opening_rule_1 <- function(x) {
  x
}

opening_rule_2 <- function(x) {
  x
}

opening_rule_3 <- function(x) {
  x
}

closing_rule_1 <- function(x) {
  x
}

closing_rule_2 <- function(x) {
  x
}

closing_rule_3 <- function(x) {
  x
}

algos <- list(
  list( ## We might name this "subset1"
    instruments = list("testdata1"),
    rules = list(
      list("or1" = opening_rule_1, "cr1" = closing_rule_1),
      list("or2" = opening_rule_2, "cr2" = closing_rule_2)
    )
  ),
  list( ## We might name this "subset2"
    instruments = list("testdata2"),
    rules = list(
      c("or1" = opening_rule_1, "cr1" = closing_rule_1),
      c("or2" = opening_rule_2, "cr2" = closing_rule_2),
      c("or3" = opening_rule_3, "cr3" = closing_rule_3)
    )
  )
)

parsed_algos <- parse_algos(algos)


f_indiv_normalization_factor <- function(raw_signal, target = 1) {
  target/mean(abs(raw_signal))
}

pool_traded <- function(signal_tables, algos) {
  #algos <- parsed_algos
  ## Get rule name for each algo in the order they appear in the algos list
  rule_names_by_algo <- get_rule_names_by_parsed_algo(algos)
  #rule_names_by_algo

  ## Get all unique rule names in a list
  all_unique_rule_names <- get_unique_rule_names_from_parsed_algos_list(algos)
  #all_unique_rule_names

  ## For each unique rule name, get the IDs of that rule in the
  ## rule_names_by_algo list
  IDs_grouped_by_rule_names <- split(
    seq_along(rule_names_by_algo),
    unlist(rule_names_by_algo)
  )
  #IDs_grouped_by_rule_names

  ## For each unique rule name, get all signals produced by that rule.
  ## We are assuming that algos are in the same order as signal tables
  raw_signals_list <- list()
  for(rule in unlist(all_unique_rule_names)) {
    raw_signals_list[[rule]] <- lapply(
      system$signal_tables[unlist(IDs_grouped_by_rule_names[rule])],
      function(x) {x$raw_signal}
    )
  }
  #raw_signals_list
  #raw_signals_df <- data.frame(raw_signals_list)
  #names(raw_signals_df) <- names(raw_signals_list)


  raw_signals_list <- list()
  for(rule in unlist(all_unique_rule_names)) {
    ll <- data.frame(lapply(
      system$signal_tables[unlist(IDs_grouped_by_rule_names[rule])],
      function(x) {x$raw_signal}
    ))
    df <- data.frame(ll)
    names(df) <- NULL

    raw_signals_list[[rule]] <- df
    #str(ll)
    #raw_signals_list[[rule]] <- df
    #names(df) <- names(ll)
  }
  #raw_signals_list

  ## When we unlist the raw signal data frame, all the column vectors in that
  ## data frame are concatenated to one vector. So in effect we are pooling all
  ## the instruments for each rule. We are not taking the cross section median
  ## across instruments, as we do for pool_all(), because the number of
  ## instruments per rule is likely to be small - taking the median of two
  ## values doesn't make much sense.
  lapply(raw_signals_list,
         function(raw_signal) {
           f_indiv_normalization_factor(unlist(raw_signal), target = 1)
         }
  )
}

pool_traded(system$signal_tables, parsed_algos)




# Test pool_all() ----

opening_rule_1 <- function(x) {
  x
}

opening_rule_2 <- function(x) {
  x * 2
}

opening_rule_3 <- function(x) {
  x * 3
}

closing_rule_1 <- function(x) {
  x
}

closing_rule_2 <- function(x) {
  x
}

closing_rule_3 <- function(x) {
  x
}

algos <- list(
  list( ## We might name this "subset1"
    instruments = list("testdata1"),
    rules = list(
      list("opening_rule_1", "cclosing_rule_1"),
      list("opening_rule_2", "closing_rule_2")
    )
  ),
  list( ## We might name this "subset2"
    instruments = list("testdata2"),
    rules = list(
      c("opening_rule_1", "closing_rule_1"),
      c("opening_rule_2", "closing_rule_2"),
      c("opening_rule_3", "closing_rule_3")
    )
  )
)

parsed_algos <- parse_algos(algos)

system <- list()
system$algos <- parsed_algos
system$signal_tables <- list(
  data.frame("raw_signal" = 1:2, "bla" = 1:20),
  data.frame("raw_signal" = 1:2 * 20, "bla" = 1:20/2),
  data.frame("raw_signal" = 1:2 * 300, "bla" = 1:20/3),
  data.frame("raw_signal" = 1:2 * 4000, "bla" = 1:20/4),
  data.frame("raw_signal" = 1:2 * 50000, "bla" = 1:20/5)
)


f_indiv_normalization_factor <- function(raw_signal, target = 1) {
  target/mean(abs(raw_signal))
}

pool_all <- function(signal_tables, algos) {
  #algos <- parsed_algos
  ## Get rule name for each algo in the order they appear in the algos list
  rule_names_by_algo <- get_rule_names_by_parsed_algo(algos)
  #rule_names_by_algo

  ## Get all unique rule names in a list
  all_unique_rule_names <- get_unique_rule_names_from_parsed_algos_list(algos)
  #all_unique_rule_names

  ## For each unique rule name, get the IDs of that rule in the
  ## rule_names_by_algo list
  IDs_grouped_by_rule_names <- split(
    seq_along(rule_names_by_algo),
    unlist(rule_names_by_algo)
  )
  #IDs_grouped_by_rule_names

  ## For each unique rule name, get all signals produced by that rule.
  ## We are assuming that algos are in the same order as signal tables
  raw_signals_list <- list()
  for(rule in unlist(all_unique_rule_names)) {
    ll <- lapply(
      system$signal_tables,
      function(x) {x$raw_signal}
    )
    df <- data.frame(ll)
    names(df) <- NULL

    raw_signals_list[[rule]] <- df
    #str(ll)
    #raw_signals_list[[rule]] <- df
    #names(df) <- names(ll)
  }
  raw_signals_list

  ## When we unlist the raw signal data frame, all the column vectors in that
  ## data frame are concatenated to one vector. So in effect we are pooling all
  ## the instruments for each rule. We are not taking the cross section median
  ## across instruments, as we do for pool_all(), because the number of
  ## instruments per rule is likely to be small - taking the median of two
  ## values doesn't make much sense.
  lapply(raw_signals_list,
         function(raw_signal) {
           f_indiv_normalization_factor(unlist(raw_signal), target = 1)
         }
  )
}

pool_all(system$signal_tables, parsed_algos)





# median_pool_all()
## Apply each rule to all instruments

sim_algos <- list()
k <- 1
for(i in seq_along(rule_functions)) {
  for(j in seq_along(data_sets)) {
    sim_algos[[k]] <- list(
        "data" = data_sets[[j]],
        "rule" = list(
          rule_functions[[i]][[1]],
          rule_functions[[i]][[2]]
        )
    )
    k <- k + 1
  }
}


## Check df behaviour

df <- data.frame()
df[1, 1] <- 11
df[1, 2] <- 12
df[1, 3] <- 13
df[2, 1] <- 21
df[2, 2] <- 22
df[2, 3] <- 23
df[3, 1] <- 31
df[3, 2] <- 32
df[3, 3] <- 33
df

median(unlist(df[3, ]))

ll <- lapply(
  df,
  function(x) {print(mean(abs(x)))}
)
unlist(ll)



## Calculate pooled estimate

data_sets <- list(
  data.frame("time" = 1:20, "price" = 100 - (1:20)),
  data.frame("time" = 1:20*2, "price" = 200 - (1:20)),
  data.frame("time" = 1:20*3, "price" = 300 - (1:20))
)

# raw_signals <- data.frame()
# k <- 1
# for(i in seq_along(rule_functions)) {
#   for(j in seq_along(data)) {
#     for(t in 1:20) {
#       raw_signals[t, k] <- rule_functions[[i]][[1]](data_sets[[j]][t ,1])
#     }
#     k <- k + 1
#   }
# }
# raw_signals

min_periods <- 25
num_rows <- 20 + min_periods

## Columns are raw_signal vectors
raw_signals_df <- data.frame()
for(i in seq_along(sim_algos)) {
  for(t in 1:(num_rows - min_periods)) {
    #raw_signals_df[t, i] <- rule_functions[[i]][[1]](data_sets[[j]][t ,1])
    raw_signals_df[t, i] <- sim_algos[[i]]$rule[[1]](sim_algos[[i]]$data[t, 1])
  }
}
raw_signals_df

cs_medians <- data.frame()
for(i in seq_along(data_sets)) {
  for(j in 1:(num_rows - min_periods)) {
    cs_medians[j, i] <- median(unlist(raw_signals_df[j, ])) ## Row median
  }
}
cs_medians

## Take mean absolute value of each vector of medians
mav_for_each_rule <- lapply(
  cs_medians,
  function(x) {unlist(mean(abs(x)))}
)

target = 1
target / unlist(mav_for_each_rule)



# ----
x <- list()
x1 <- list(2)
x2 <- list(2, 3)
x3 <- list(2, 3, 4)

length(x) %in% c(1,2)
length(x1) %in% c(1,2)
length(x2) %in% c(1,2)
length(x3) %in% c(1,2)

match(2, x3)


# get_rule_names_by_parsed_algo() ----

## A rule can contain one signal rule and optionally one stop loss rule.

rule_names <- list()
for(i in seq_along(parsed_algos)) {
  rule_names[i] <- paste0(
    "[",
    names(parsed_algos[[i]]$rule[1]),
    ", ",
    names(parsed_algos[[i]]$rule[2]),
    "]"
  )
}


rule_names <- list()
for(i in seq_along(parsed_algos)) {
  rule_names[i] <- ""
  for(j in seq_along(parsed_algos[[i]]$rule)) {
    rule_names[i] <- paste(rule_names[i], names(parsed_algos[[i]]$rule[j]), sep = ",")
  }
  rule_names[i] <- substring(rule_names[[i]], 2)
  rule_names[i] <- paste0("{",rule_names[i], "}")
}
rule_names



##



get_rule_names_by_parsed_algo <- function(parsed_algos) {
  ## Collect all instrument names from expanded_algos
  rule_names <- list()
  for(i in seq_along(parsed_algos)) {
    rule_names[i] <- ""
    ## Append each name string to the previous one and separate by comma
    for(j in seq_along(parsed_algos[[i]]$rule)) {
      rule_names[i] <- paste(rule_names[i], parsed_algos[[i]]$rule[j], sep = ",")
    }
    ## Remove leading comma
    rule_names[i] <- substring(rule_names[[i]], 2)
    ## Add square brackets (no need)
    #rule_names[i] <- paste0("[",rule_names[i], "]")
  }

  ## Make list of only instrument names
  rule_names
}

get_rule_names_by_parsed_algo(parsed_algos)




# MAC ----

stop_loss_rule <- function(
    prices,
    instrument_risk,
    stop_loss_fraction,
    t_trade_open,
    direction,
    t) {
  price_unit_vol <- f_price_unit_vol(instrument_risk, prices[t])
  stop_loss_gap <- f_stop_loss_gap(price_unit_vol, stop_loss_fraction)
  print(stop_loss_gap)

  ## Close position if stop loss level was breached yesterday [LT, p. 138].
  ## Close position even if price has recovered. [LT, p. 141]
  stop_loss_level <- f_stop_loss_level(
    f_high_water_mark(prices, t - 1, t_trade_open),
    f_low_water_mark(prices, t - 1, t_trade_open),
    stop_loss_gap = stop_loss_gap,
    direction = direction,
    rnd  = FALSE
  )

  if(direction == 1) { ## If long
    if(prices[t] < stop_loss_level) { ## Close position if...
      open_close <- 0 ## Close
    } else {open_close <- 1}
  } else if(direction == -1) { ## If short
    open_close <- 1
    if(price > stop_loss_level) { ## Close position if...
      open_close <- 0 ## Close
    } else {open_close <- 1}
  } else {open_close <- 0} ## Should be redundant...
  open_close
}

f_stop_loss_level <- function(
    hwm,
    lwm,
    stop_loss_gap,
    direction = 0,
    rnd = TRUE) {
  if(direction == 1){ ## If long
    hwm - stop_loss_gap + runif(1, 0.01, 0.03) * rnd
  } else {lwm + stop_loss_gap - runif(1, 0.01, 0.03) * rnd}
}

f_inst_risk <- function(prices, t, window_length = 25) {
  if(is.na(window_length)) {window_length = t} ## Include all if window_length is NA
  if(t > window_length) {
    sd_ <- sd(f_returns_from_prices(prices[(t - window_length + 1):t])) * 15.87451 ## 252 is the number of business days in a year. sqrt(252) = 15.87451
  } else if (t > 1) {
    sd_ <- sd(f_returns_from_prices(prices[1:t])) * 15.87451
    warning("Window length for instrument risk calculation is bigger than length of price vector.\n")
  } else {
    sd_ <- NA
    warning("inst_risk=NA. Careful! This might break something.\n")
  }
  sd_
}

f_returns_from_prices <- function(prices) {
  N <- length(prices)
  (head(prices, N - 1) / tail(prices, N - 1)) - 1
}

f_stop_loss_gap <- function(price_unit_vol, stop_loss_fraction) {
  price_unit_vol * stop_loss_fraction
}

f_price_unit_vol <- function(inst_risk, price) {
  inst_risk * price
}

f_high_water_mark <- function (prices, t, t_trade_open) {
  max(prices[t_trade_open:t])
}

f_low_water_mark <- function (prices, t, t_trade_open) {
  min(prices[t_trade_open:t])
}


set.seed <- 99
prices <- 100 + cumsum(rnorm(100))
stop_loss_signal <- numeric(100)

for(t in 27:100){
  stop_loss_signal[t] <- stop_loss_rule(
    prices = prices,
    instrument_risk = f_inst_risk(prices[1:t], t),
    stop_loss_fraction = 0.5,
    t_trade_open = 26,
    direction = 1,
    t = t)
}

stop_loss_signal

prices

prices[stop_loss_signal == "CLOSE"]

# get_signal_normalization_factors_by_algos ----

get_signal_normalization_factors_by_algos <- function(
    signal_normalization_factors, parsed_algos) {

  sig_norm_fact_by_algos <- list()
  rule_names_by_parsed_algo <- get_rule_names_by_parsed_algo(parsed_algos)

  for(i in seq_along(parsed_algos)) {
    sig_norm_fact_by_algos[[i]] <- signal_normalization_factors[rule_names_by_parsed_algo[[i]]][[1]]
  }

  flatten(sig_norm_fact_by_algos)
}


x <- get_signal_normalization_factors_by_algos(
  list(
    "mac_20_80,stop_loss" = 1.1,
    "mac_50_200,stop_loss" = 2.1
  ),
  parsed_algos
)
x

# Test system ----

n <- 500
min_periods <- 25

mac_20_80 <- function(prices) {
  mac_rule(
    prices,
    n_fast = 20L,
    n_slow = 80L
  )
}
mac_50_200 <- function(prices) {
  mac_rule(
    prices,
    n_fast = 50L,
    n_slow = 200L
  )
}
stop_loss <- stop_loss_rule

times <- as.timeDate(seq(from = as.Date("2000-01-01"), by = "day", length.out = n))
df1 <- data.frame(
  time = times,
  price = round(
    c(100, 100 * cumprod(1 + rnorm(n - 1, 0, 0.1/16))),
    2
  )
)
names(df1) <- c("time", "price")
names(df1)
df2 <- data.frame(
  time = times,
  price = round(
    c(100, 100 * cumprod(1 + rnorm(n -1, 0, 0.1/16))),
    2
  )
)
names(df2) <- c("time", "price")
names(df2)
write.csv(df1, "~/git/rsystrade/misc/temp/data/testdata3.csv"
)
write.csv(df2, "~/git/rsystrade/misc/temp/data/testdata4.csv"
)

algos <- list(
  list( ## We might name this "subset1"
    instruments = list("testdata3"),
    rules = list(
      list("mac_20_80"),
      list("mac_50_200")
    )
  ),
  list( ## We might name this "subset2"
    instruments = list("testdata4"),
    rules = list(
      c("mac_20_80"),
      c("mac_50_200")
    )
  )
)

parsed_algos <- parse_algos(algos)

inst_data <- load_instrument_data_sets(
  parsed_algos = parsed_algos,
  instrument_data_folder_path = "~/git/rsystrade/misc/temp/data/"
)
names(inst_data) <- unlist(get_unique_inst_names_from_parsed_algos_list(parsed_algos))

rule_functions <-load_rule_functions(parsed_algos)
names(rule_functions) <- get_unique_rule_function_names_by_parsed_algo(parsed_algos)

num_signals <- get_num_rules_from_parsed_algos_list(parsed_algos)
signal_tables <- list()
for(i in 1:num_signals) {
  ## One table for each algo (i.e each instrument + rule combination)
  signal_tables[[i]] <- data.frame(
    time = inst_data[[parsed_algos[[i]]$instrument]]$time[1:min_periods],
    price = inst_data[[parsed_algos[[i]]$instrument]]$price[1:min_periods],
    raw_signal = rep(NA, min_periods),
    normalized_signal = rep(NA, min_periods),
    clamped_signal = rep(NA, min_periods),
    signal_weight = rep(NA, min_periods)
  )
}

my_system <- make_system(
  algos = algos,
  init_capital = 1000000,
  system_risk_target = 0.12,
  risk_window_length = 25,
  stop_loss_fraction = 0.5,
  min_periods = min_periods,
  mode = "sim",
  instrument_data_folder_path = "~/git/rsystrade/misc/temp/data/"
)

run_system(
  my_system,
  min_periods = min_periods,
  mode = "sim",
  instrument_data_folder_path = "~/git/rsystrade/misc/temp/data/"
)

syst <- run_system(my_system,min_periods = min_periods,mode = "sim",instrument_data_folder_path = "~/git/rsystrade/misc/temp/data/")

View(syst$signal_tables[[1]])
View(syst$signal_tables[[2]])
View(syst$signal_tables[[3]])
View(syst$signal_tables[[4]])

View(syst$position_tables[[1]])
View(syst$position_tables[[2]])


View(syst$system_account_table)



## Which environment do rule functions live in? ----

## If we define a rule function in the global env, and reassign it inside
## another function, where will it live?

f <- function(x) {print(x)}
str(f)
eval(parse(text = "f"))

g <- function(y) {
  h <- eval(parse(text = "f"))
  str(h)
}

g()

## h lives in the global env, not the env of g!



