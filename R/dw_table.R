dw_table<-function(tab){
  new_chart_dw<-dw_create_chart(type=tables)
  id<-new_chart_dw$id
  dw_data_to_chart(tab, id)
  dw_edit_chart(chart_id = id, language="ru-RU")
  id_print<-dw_export_chart(id)
  image_write(id_print, path = paste0(id, ".png"), format = "png")
}
