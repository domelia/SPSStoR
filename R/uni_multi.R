#' Univariate table for multiple response questions
#'
#'This function is aimed to make sorted univarate table for multiple response question, coded in binary variables. Output table is saved into global environment under the name "tab_question".
#'
#' @param df Dataframe imported from SPSS file by means of the `haven::read_sav()` function.
#' @param q Prefix for multiple response variable (ex: if there are 5 subquestions V1_1, v1_2, V1_3 etc. "V1" should be entered into the function).
#' @return Function returns crosstable saved into global environment, shows optionnally in console the results of chi-square tests and saves the table in excel format into working directory.

uni_multi<-function(df, q, sort=F, excel=F){
  if (is.data.frame(df)==FALSE) {stop("df is not a dataframe")}
  set<-df %>%
    select(contains(q))%>%
    select_if(is.numeric)
  labs<-set %>%sjlabelled::get_label()
  labs<-as_tibble(labs)
  tab<-multi.table(set, freq = T)
  tab<-as_tibble(tab)
  tab<-tab %>%
    select(-n) |>
    rename(`%`=`%multi`) |>
    mutate(Показатель=labs$value) |>
    relocate(Показатель) |>
    filter(`%`!=0)

  if (sort==T){
    tab<-tab %>%
      arrange(desc(`%`))
  }
  if(excel==T){
    xlsx::write.xlsx(tab, paste0(q,".xlsx"))
  }
  assign(paste0("tab_", q), tab, .GlobalEnv)
}

