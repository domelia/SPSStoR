#' Convert wide table to long format
#'
#' This function reshapes a summary table created by `tbl()` from wide to long
#' format using `tidyr::pivot_longer()`. The first column (`var1`) is treated
#' as the identifier, and all remaining columns are gathered into two columns:
#' a factor column and a count column. The resulting long table is assigned to
#' a new object in the calling environment, with the name of the input table
#' and the suffix `_long` appended.
#'
#' @param tab A data frame or tibble whose first column is named `var1` and
#'   subsequent columns contain counts or frequencies to be reshaped.
#' @param factor An unquoted name to be used as the column name for the factor
#'   variable created by `pivot_longer()` (e.g., `factor = grp`).
#'
#' @return Invisibly returns the long-format tibble, and assigns it to a new
#'   object in the calling environment named `<tab>_long`, where `<tab>` is the
#'   name of the input object.
#'
#' @examples
#' \dontrun{
#' tbl_age <- tbl(data, age)
#' lng(tbl_age, factor = group)
#' tbl_age_long
#' }
lng <- function(tab, factor) {
  out_name <- paste0(deparse(substitute(tab)), "_long")
  factor_name <- rlang::as_name(substitute(factor))

  out <- tab |>
    tidyr::pivot_longer(
      cols      = -var1,
      names_to  = ,
      values_to = "count"
    )

  assign(out_name, out, envir = parent.frame())
  invisible(out)
}
