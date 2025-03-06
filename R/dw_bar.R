#' Barplot for crosstab in Datawrapper
#'
#'This function is aimed to make a barplot for a crosstab created with cross_multi function.
#'
#' @param tab Crosstable having the first column "var" and next column.
#' @param type A string in the list c("split", "grouped").
#' @param mirror When type is "split" (default) and mirror=T, the plot with mirrored bars is created.
#' @return The function creates a barplot in author's Datawrapper's account dashboard and imports a (.png) file, saving it into working directory.

dw_bar<-function(tab, type="split", mirror=T, colors=c("#003f5c", "#fa8c00"), save=T){
  cats<-colnames(tab[,-1])
  cat_col<- setNames(as.list(colors), paste0("cat", seq_along(colors)))
  names(cat_col) <- cats
  plot="d3-bars-split"
  color_key=F
  if(type=="grouped"){
    plot="d3-bars-grouped"
    color_key=T
  }
  if(type=="donut"){
    plot="d3-donuts"
    color_key=T
  }
  if(type=="pie"){
    plot="d3-pies"
    color_key=T
  }

  mirror=mirror
  new_chart_dw<-dw_create_chart()
  id<-new_chart_dw$id
  dw_data_to_chart(tab, id)
  dw_edit_chart(chart_id = id,  type=plot, language="ru-RU", visualize=list("value-label-format"="0.0","background"=F, "show-color-key"=color_key, "mirror-bars"=mirror, "custom-colors"=cat_col, "color-category"=cat_col))
  id_print<-dw_export_chart(id)
  if(save==T){
    image_write(id_print, path = paste0(id, ".png"), format = "png")
  }
}


