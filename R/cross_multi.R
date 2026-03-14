#' Bivariate table for multiple response questions
#'
#'This function is aimed to make crosstab with multiple response question, coded in binary variables. Output table is saved in global environment under the name "table_factor_main_question".
#'
#' @param df Dataframe imported from SPSS file through `haven::read_sav()` function.
#' @param q Prefix for multiple response variable (ex: if there are 5 subquestions V1_1, v1_2, V1_3 etc. "V1" should be entered into the function).
#' @param f Factor variable entered as a string ("V2")
#' @return Function returns crosstable saved into global environment, shows optionnally in console the results of chi-square tests and saves the table in excel format into working directory.

cross_multi <- function(df, q, f, chi=F, excel=F){
  if (!is.data.frame(df)) stop("df is not a dataframe")

  df[[f]] <- sjlabelled::as_label(df[[f]])
  set <- df %>%
    select(contains(q)) %>%
    select_if(is.numeric)

  labs <- set %>% sjlabelled::get_label()
  labs <- as_tibble(labs)

  tab <- cross.multi.table(set, df[[f]], true.codes = list("1"),
                           tfreq = "col", freq = TRUE)
  tab <- tab %>%
    as.data.frame() %>%
    rownames_to_column(var = "var")
  tab$var <- labs$value

  if(excel == T){
    xlsx::write.xlsx(tab, paste0("tab_", f, "_", q, ".xlsx"))
  }

  assign(paste0("tab_", f, "_", q), tab, .GlobalEnv)

  if(chi == T){
    CHIS <- sapply(names(set), function(var_name) {
      tab_cont <- table(df[[f]], df[[var_name]])
      if(all(dim(tab_cont) == c(1,1))) return(c(Chi = NA, p = NA))
      test <- chisq.test(tab_cont)
      c(Chi = round(test$statistic, 2), p = round(test$p.value, 4))
    })
    results <- t(CHIS)
    return(results)
  }

  invisible(tab)
}
