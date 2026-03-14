bar_plot1<-function(data, fill_colors = colors, label_width=50){
    data$Var1 <- factor(data$Var1, levels = rev(data$Var1))
    max_width <- max(data$Freq)
    max_text_width <- max(stringr::str_length(data$Var1))
    left_margin <- 0  # 3.5 pt на символ + минимум 40

    ggplot(data=data, aes(.data$Var1, y = .data$Freq, fill = .data$Var1)) +
      geom_chicklet(stat="identity", width = 0.6)+
      geom_text(aes(
        label = scales::number(.data$Freq, accuracy = 0.1, decimal.mark = ","),
        hjust = -0.3, colour = Var1, size = 5, fontface = "bold"
      ))+
      expand_limits(y=c(0,75))+
      coord_flip(clip = "off")+
      theme_void()+
      theme(
        axis.text.y = element_text(size = 11, hjust = 1),
        legend.position = "none",
        axis.text.x = element_blank(),
        axis.ticks = element_blank(),
        axis.title=element_blank(),
        plot.margin = margin(t=0, r=0, b=0, l = 0, unit = "pt")
      )+
      scale_x_discrete(labels = function(x) str_wrap(x, width = label_width))+
      scale_fill_manual(values = fill_colors)+
      scale_colour_manual(values = fill_colors)
}
