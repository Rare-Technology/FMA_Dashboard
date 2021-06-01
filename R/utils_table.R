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

create_gt_table <- function(.data,  var1) {
  vals <- seq(1, nrow(.data), by = 2)
  tbl <- .data %>%
    gt::gt() %>%
    # gt::tab_header(
    #   title = title,
    #   subtitle = subtitle
    # ) %>%
    gt::tab_style(
      style = list(
        # cell_fill(color = "lightcyan"),
        "font-variant: small-caps;"
      ),
      locations = gt::cells_body(columns = c({{ var1 }}))
    ) %>%
    gt::tab_style(
      style = list(
        gt::cell_fill(color = TABLE_ROW_COLOR)
      ),
      locations = gt::cells_body(
        rows = vals
      )
    ) %>%
    gt::cols_width(
      c(`Performance Indicator`) ~ px(125),
      gt::everything() ~ px(145)
    ) %>%
    gt::tab_options(
      table.font.size = 13
    )
}
