#' Table for likert-like questions
#'
#' The function creates a table with proportions for all answers from a set of questions.
#'
#' @param set Dataframe with selected questions for which a table should be created..

likert_table<-function(set){
  tab<-lapply(set, function(x) round(prop.table(table(x))*100, 2))
  names<-as.vector(levels(sjlabelled::as_label(set[1])))
  tab<-tab %>%
    bind_rows(.id = "Показатель")
  tab$Показатель<-sjlabelled::get_label(set)
  colnames(tab)<-c("Показатель", names)
  tab
}

