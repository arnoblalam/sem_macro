# name:         helper.R
# description:  Helper functions for R
# Helper functions

remove_last_row <- function(x) {
  x[-nrow(x),]
}

read_data <- function(path, col_types = c("date", "numeric"), skip = 5) {
  read.zoo(as.data.frame(remove_last_row(read_excel(path, 
                                                    col_types = col_types,
                                                    skip = skip))),
           tz = "GMT")
}