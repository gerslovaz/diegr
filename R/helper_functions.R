## helper functions

req_cols <- function(obj, required_cols) {
  # control required columns
  is.atomic(names(obj)) && all(required_cols %in% names(obj))
}
# in: animate_topo

