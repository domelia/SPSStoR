#' Bivariate table for multiple response questions
#'
#'This function is aimed to make crosstab with multiple response question, coded in binary variables. Output table is saved in global environment under the name "table_factor_main_question".
#'
#' @param df Dataframe imported from SPSS file through `haven::read_sav()` function.
#' @param q Prefix for multiple response variable (ex: if there are 5 subquestions V1_1, v1_2, V1_3 etc. "V1" should be entered into the function).
#' @param f Factor variable entered as a string ("V2")
#' @return Function returns crosstable saved into global environment, shows optionnally in console the results of chi-square tests and saves the table in excel format into working directory.

cross_multi <- function(df, q, f, chi=F, excel=F){
  if (is.data.frame(df)==FALSE) {stop("df is not a dataframe")}
  df[[f]]<-sjlabelled::as_label(df[[f]])
  set<-df %>%
    select(contains(q)) %>%
    select_if(is.numeric)
  labs<-set %>%sjlabelled::get_label()
  labs<-as_tibble(labs)
  tab<-cross.multi.table(set, df[[f]], true.codes=list("1"), tfreq = "col", freq=TRUE)
  tab<-tab %>%
    as.data.frame %>%
    rownames_to_column(var="var")
  tab$var<-labs$value
  if (excel==T){
    xlsx::write.xlsx(tab, paste0("tab_", f, "_", q, ".xlsx"))
  }
  assign(paste0("tab_", f, "_", q), tab, .GlobalEnv)
  if(chi==T){
    CHIS <- lapply(set[,-ncol(set)], function(x) chisq.test(df[[f]], x))
    do.call(rbind, CHIS,2)[,c(1,3)]
  }
}
