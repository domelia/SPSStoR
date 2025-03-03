#' Barplot for crosstab in Datawrapper
#'
#'This function is aimed to make a barplot for a crosstab created with cross_multi function.
#'
#' @param tab Crosstable having the first column "var" and next column.
#' @param type A string in the list c("split", "grouped").
#' @param mirror When type is "split" (default) and mirror=T, the plot with mirrored bars is created.
#' @return The function creates a barplot in author's Datawrapper's account dashboard and imports a (.png) file, saving it into working directory.

dw_bar<-function(tab, mirror=T){
  plot="d3-bars-split"
  color_key=F
  mirror=mirror
  new_chart_dw<-dw_create_chart()
  id<-new_chart_dw$id
  dw_data_to_chart(tab, id)
  dw_edit_chart(chart_id = id,  type=plot, language="ru-RU", visualize=list("value-label-format"="0.0","background"=F, "show-color-key"=color_key, "mirror-bars"=mirror))
  id_print<-dw_export_chart(id)
  image_write(id_print, path = paste0(id, ".png"), format = "png")
}
