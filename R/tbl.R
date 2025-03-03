#' Simple frequency and contngency table
#'
#'This function is aimed to make either simple percentage table for variable with one choice or cross-tabulation with factor, whch may be accompagnied by chi square statistics.
#'
#' @param df Dataframe imported from SPSS file through `haven::read_sav()` function.
#' @param var Principal variable to create a table with.
#' @param factor Factor variable entered as a string ("V2"). Default value is NULL.
#' @param chi Optional. When TRUE chi-squared test is applied to test hypothesis about independence of var and factor.
#' @return Function returns table or crosstable, and optionnaly, ch-squared statistics.

tbl<-function(df, var, factor=NULL, chi=F){
  df[[var]]<-sjlabelled::as_label(df[[var]])
  tab <- round(prop.table(table(df[[var]]))*100,1)

  if(!is.null(factor)){
    df[[factor]]<-sjlabelled::as_label(df[[factor]])
    var1<-df[[var]]
    var2<-df[[factor]]
    tab <- with(df, table(var1, var2)) |>
      prop.table(margin=2)*100
    tab<-tab |>
      as.data.frame() |>
      pivot_wider(names_from = var2, values_from = Freq, values_fn = ~ round(.x, 1))

  }
  if(chi==T){
    df[[factor]]<-sjlabelled::as_label(df[[factor]])
    chisq.test(df[[var]], df[[factor]])
  }
  tab
}
