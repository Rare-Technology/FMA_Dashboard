table_column_alignment <- function(.data) {

  # centered <- c(
  #   "Total Counts"
  # )
  
  left <- c(
    "Country", "Subnational"
  )
  
  
  cols <- names(.data)
  
  list(
    # list(className = "table-text-center", targets = which(cols %in% centered) - 1),
    list(className = "table-text-nocenter", targets = which(cols %in% left) - 1)
  )
}