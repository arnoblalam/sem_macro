# name:         helper.R
# description:  Helper functions for R
# Helper functions

remove_last_row <- function(x) {
  x[-nrow(x),]
}