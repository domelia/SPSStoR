#' Bivariate table for multiple response questions
#'
#'This function is aimed to make crosstab with multiple response question, coded in binary variables. Output table is saved in global environment under the name "table_factor_main_question".
#'
#' @param df Dataframe imported from SPSS file through `haven::read_sav()` function.
#' @param q Prefix for multiple response variable (ex: if there are 5 subquestions V1_1, v1_2, V1_3 etc. "V1" should be entered into the function).
#' @param f Factor variable entered as a string ("V2")
#' @return Function returns crosstable saved into global environment, shows optionnally in console the results of chi-square tests and saves the table in excel format into working directory.

cross_multi <- function(df, q, f,
                        chi   = FALSE,
                        excel = FALSE) {

  if (!is.data.frame(df)) stop("df is not a dataframe")

  # фактор-группа (пол, возраст и т.п.)
  df[[f]] <- sjlabelled::as_label(df[[f]])

  # блок множественного ответа
  set <- df %>%
    dplyr::select(dplyr::contains(q)) %>%
    dplyr::select_if(is.numeric)

  # подписи к подвопросам
  labs <- set %>% sjlabelled::get_label()
  labs <- tibble::as_tibble(labs)

  ## 1) Проценты как обычно (questionr::cross.multi.table)
  tab_raw <- questionr::cross.multi.table(set,
                                          df[[f]],
                                          true.codes = list("1"),
                                          tfreq      = "col",
                                          freq       = TRUE)

  tab <- tab_raw %>%
    as.data.frame() %>%
    tibble::rownames_to_column(var = "var")
  tab$var <- labs$value

  assign(paste0("tab_", f, "_", q), tab, .GlobalEnv)

  ## 2) Буквы по РЕСПОНДЕНТАМ (freq_tab_to_letters_resp)
  letters_tab <- NULL
  if (chi) {
    letters_tab <- freq_tab_to_letters_resp(
      df        = df,
      group_var = f,
      q_prefix  = q,
      alpha     = 0.05,
      p.adjust.method = "BH"
    )

    # заменим технические имена на метки подвопросов
    letters_tab <- letters_tab %>%
      tibble::rownames_to_column(var = "group")  # если rownames есть
    names_map <- tibble::tibble(group = colnames(set),
                                var   = labs$value)
    letters_tab <- letters_tab %>%
      dplyr::left_join(names_map, by = "group") %>%
      dplyr::select(var, dplyr::everything(), -group)

    assign(paste0("letters_", f, "_", q), letters_tab, .GlobalEnv)
  }

  ## 3) Excel
  if (excel) {
    file_name <- paste0("tab_", f, "_", q, ".xlsx")

    # лист с процентами
    xlsx::write.xlsx(tab, file_name,
                     sheetName = "freq",
                     row.names = FALSE)

    # лист с буквами
    if (!is.null(letters_tab)) {
      xlsx::write.xlsx(letters_tab, file_name,
                       sheetName = "letters_resp",
                       row.names = FALSE,
                       append    = TRUE)
    }
  }

  if (chi) {
    return(letters_tab)
  }

  invisible(tab)
}
