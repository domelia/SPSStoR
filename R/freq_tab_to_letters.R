#' Post-hoc letters for multiple-response column proportions (respondent-based)
#'
#' Computes SPSS-like post-hoc groups for a multiple-response question
#' on a respondent level. For each item (row) and each pair of columns
#' (groups, e.g. sex or age categories), it performs a chi-squared test
#' on a 2x2 table of respondents (selected vs. not selected), applies
#' multiple-comparisons correction, and assigns letter groupings so
#' that columns sharing a letter do not differ significantly in the
#' proportion of respondents selecting that item.
#'
#' @param df A data.frame containing the grouping variable and the
#'   multiple-response indicator variables (0/1).
#' @param group_var Name of the grouping variable (character scalar),
#'   e.g. \code{"sex"} or \code{"age_cats8"}.
#' @param q_prefix Character prefix common to the multiple-response
#'   items, e.g. \code{"B14_"}.
#' @param alpha Significance level used to define differences between
#'   column proportions. Default is 0.05.
#' @param p.adjust.method Method passed to \code{\link[stats]{p.adjust}}
#'   for multiple-comparisons correction within each row. Default is
#'   \code{"BH"} (Benjamini–Hochberg), as in SPSS column proportions
#'   with FDR adjustment.
#'
#' @details
#' Let \eqn{g} be a categorical grouping variable (e.g. sex) and
#' \eqn{Y_k} be binary multiple-response indicators (0/1) for items
#' \eqn{k = 1, \dots, K}. For each item \eqn{k} and pair of groups
#' \eqn{g_1, g_2}, the function forms a 2x2 table of respondents:
#'
#' \tabular{ccc}{
#'   & \eqn{g_1} & \eqn{g_2} \cr
#'   selected     & \eqn{x_{k1}} & \eqn{x_{k2}} \cr
#'   not selected & \eqn{n_{1} - x_{k1}} & \eqn{n_{2} - x_{k2}} \cr
#' }
#'
#' where \eqn{x_{kj}} is the number of respondents in group \eqn{g_j}
#' who selected item \eqn{k}, and \eqn{n_j} is the total number of
#' respondents in group \eqn{g_j}. A Pearson chi-squared test
#' (\code{\link[stats]{chisq.test}} with \code{correct = FALSE})
#' is applied to each 2x2 table. The resulting p-values are adjusted
#' within each row using \code{p.adjust.method} (default \code{"BH"}).
#'
#' Letter groupings are then constructed row-wise: columns that do not
#' show significant pairwise differences (adjusted p-value \eqn{\ge}
#' \code{alpha}) may share a letter; columns that differ significantly
#' from at least one other column receive at least one different letter.
#'
#' @return A data.frame with one row per multiple-response item and
#'   one column per level of \code{group_var}. Each cell contains a
#'   character string of one or more uppercase letters denoting the
#'   post-hoc grouping of that column within that row.
#'
#' @examples
#' \dontrun{
#' # df: data.frame with 'sex' and B14_* (0/1 multiple-response items)
#' letters_tab <- freq_tab_to_letters_resp(df,
#'                                         group_var = "sex",
#'                                         q_prefix  = "B14_",
#'                                         alpha     = 0.05,
#'                                         p.adjust.method = "BH")
#' }
#'
#' @export
freq_tab_to_letters_resp <- function(df,
                                     group_var,
                                     q_prefix,
                                     alpha = 0.05,
                                     p.adjust.method = "BH") {

  # build respondent-based frequency table: rows = items, cols = groups
  build_freq_tab_resp <- function(df, group_var, q_prefix) {
    g <- df[[group_var]]
    items <- df[, grepl(paste0("^", q_prefix), names(df)), drop = FALSE]
    item_names <- colnames(items)

    freq_mat <- sapply(item_names, function(item) {
      base::tapply(items[[item]] == 1, g, sum, na.rm = TRUE)
    })
    freq_mat <- t(freq_mat)  # rows = items, cols = groups

    freq_tab <- data.frame(
      group = item_names,
      freq_mat,
      check.names = FALSE
    )
    rownames(freq_tab) <- NULL
    freq_tab
  }

  pairwise_chisq_row_resp <- function(x, n_group, p.adjust.method = "BH") {
    stopifnot(length(x) == length(n_group))
    k  <- length(x)
    nm <- names(x)
    stopifnot(all(names(n_group) == nm))

    pmat <- matrix(NA_real_, nrow = k, ncol = k,
                   dimnames = list(nm, nm))

    for (i in 1:(k - 1)) {
      for (j in (i + 1):k) {
        x1 <- x[i]; n1 <- n_group[i]
        x2 <- x[j]; n2 <- n_group[j]
        if (n1 == 0 || n2 == 0) next

        tab_2x2 <- matrix(c(x1, n1 - x1,
                            x2, n2 - x2),
                          nrow = 2, byrow = TRUE)
        p <- suppressWarnings(
          stats::chisq.test(tab_2x2, correct = FALSE)$p.value
        )
        pmat[i, j] <- p
        pmat[j, i] <- p
      }
    }

    up <- upper.tri(pmat)
    pvec <- pmat[up]
    pvec_adj <- stats::p.adjust(pvec, method = p.adjust.method)
    pmat[up] <- pvec_adj
    pmat[lower.tri(pmat)] <- t(pmat)[lower.tri(pmat)]
    diag(pmat) <- NA_real_

    pmat
  }

  letters_from_pmat <- function(pmat, alpha = 0.05) {
    nm  <- rownames(pmat)
    k   <- length(nm)

    sig <- (pmat < alpha)
    diag(sig) <- FALSE

    letters_all <- LETTERS
    groups <- rep("", k)
    current_letter <- 1
    remaining <- seq_len(k)

    while (length(remaining) > 0) {
      this_group <- c()
      for (i in remaining) {
        if (length(this_group) == 0 ||
            all(!sig[i, this_group], na.rm = TRUE)) {
          this_group <- c(this_group, i)
        }
      }
      groups[this_group] <- paste0(groups[this_group],
                                   letters_all[current_letter])
      remaining <- setdiff(remaining, this_group)
      current_letter <- current_letter + 1
    }

    stats::setNames(groups, nm)
  }

  # main body --------------------------------------------------------------

  freq_tab <- build_freq_tab_resp(df, group_var, q_prefix)
  freq_mat <- as.matrix(freq_tab[, -1, drop = FALSE])
  group_names <- colnames(freq_mat)

  n_group <- base::table(df[[group_var]])
  n_group <- n_group[group_names]
  n_group <- as.numeric(n_group)
  names(n_group) <- group_names

  letter_mat <- t(apply(freq_mat, 1, function(x_row) {
    names(x_row) <- group_names
    pmat <- pairwise_chisq_row_resp(x_row, n_group,
                                    p.adjust.method = p.adjust.method)
    letters_from_pmat(pmat, alpha = alpha)
  }))

  rownames(letter_mat) <- freq_tab$group
  colnames(letter_mat) <- group_names

  as.data.frame(letter_mat, stringsAsFactors = FALSE)
}
