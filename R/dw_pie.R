#' Pie and dnut plot in Datawrapper
#'
#'This function is aimed to make a pie plot for a table created with tbl function.
#'
#' @param tab Table having the first column "var1" and next column.
#' @param type Type of the plot - "pie" or "donut".
#' @param colors Minimal color palette is provided.
#' @param save Logical, default is TRUEmm meaning that the plot created will be saved in the working directory.
#' @return The function creates a barplot in author's Datawrapper's account dashboard and imports a (.png) file, saving it into working directory.

dw_pie<-function(tab, type="pie", colors=c("#003f5c", "#fa8c00"),  save=T){
  cats<-tab[,1]
  cat_col<- setNames(as.list(colors), paste0("cat", seq_along(colors)))
  names(cat_col) <- cats
  plot="d3-pies"
  color_key=F

  if(type=="donut"){
    plot="d3-donuts"
    color_key=F
  }
  if(type=="pie"){
    plot="d3-pies"
    color_key=F
  }

  new_chart_dw<-dw_create_chart()
  id<-new_chart_dw$id
  dw_data_to_chart(tab, id)
  dw_edit_chart(chart_id = id,  type=plot, language="ru-RU", visualize=list("value-label-format"="0.0", "small_multiples"=list("enabled"=T), "background"=F, "slice_order"="original","inside_labels"= list("enabled"=F), "outside_labels"=list("enabled"=T), "show-color-key"=color_key, "mirror-bars"=mirror, "custom-colors"=cat_col, "color-category"=cat_col), publish=list("embed-width"=400))
  id_print<-dw_export_chart(id)
  if(save==T){
    image_write(id_print, path = paste0(id, ".png"), format = "png")
  }
}


