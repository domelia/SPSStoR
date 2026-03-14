bar_plot2 <- function(data,
                      factor      = "reg",
                      var         = "var1",
                      count       = "count",
                      color_by    = c("row", "factor"),
                      color_factor = NULL,
                      position_type = c("dodge", "stack"),  # Новый параметр!
                      fill_colors = c("#006d2c","#74c476","#e18961","#b15f3a","grey"),
                      label_wrap_width   = 30,
                      y_label_wrap_width = 50,
                      text_size          = 11,
                      strip_size         = 11,
                      axis_text_y_size   = 11,
                      small_freq_threshold = 10,
                      label_size         = 4,
                      strip_wrap_width   = 11,
                      reverse_y_order    = TRUE)
{

  color_by <- match.arg(color_by)
  position_type <- match.arg(position_type)

  # Сохраняем исходный порядок var БЕЗ изменений
  data[[var]] <- factor(data[[var]], levels = unique(data[[var]]))

  if (reverse_y_order) {
    data[[var]] <- forcats::fct_rev(data[[var]])
  }

  if (color_by == "factor" && is.null(color_factor)) {
    color_factor <- factor
  }

  required_cols <- unique(c(count, var, factor, color_factor))
  if (!all(required_cols %in% names(data))) {
    stop("В data должны быть колонки: ", paste(required_cols, collapse = ", "))
  }

  # factor оставляем в исходном порядке
  data[[factor]] <- as.factor(data[[factor]])

  color_var_name <- if (color_by == "row") var else color_factor
  color_var      <- data[[color_var_name]]

  n_levels <- length(unique(color_var))
  if (length(fill_colors) < n_levels) {
    warning("Мало цветов для уровней ", color_var_name, ". Используются повторяющиеся цвета.")
    fill_colors <- rep(fill_colors, length.out = n_levels)
  }

  color_levels <- levels(as.factor(color_var))
  fill_colors  <- fill_colors[seq_along(color_levels)]
  names(fill_colors) <- color_levels

  hcl <- farver::decode_colour(fill_colors, "rgb", "hcl")
  label_col <- ifelse(hcl[, "l"] > 80, "black", "white")
  names(label_col) <- names(fill_colors)

  # Создаем лабелер для переносов в strip
  strip_labeller <- labeller(
    !!factor := label_wrap_gen(strip_wrap_width)
  )
  # Выбираем позицию в зависимости от параметра
  col_position <- if (position_type == "dodge") {
    position_dodge()
  } else {
    position_stack()
  }
  p <- ggplot(
    data,
    aes(
      x    = .data[[count]],
      y    = .data[[var]],
      group = .data[[factor]],
      fill = .data[[color_var_name]]
    )
  ) +
    geom_col(position = col_position) +
    facet_wrap(vars(.data[[factor]]), nrow=1, labeller = strip_labeller) +
    scale_fill_manual(values = fill_colors) +
    scale_color_manual(values = label_col) +
    scale_y_discrete(labels = label_wrap(y_label_wrap_width)) +  # без limits!
    geom_text(
      data = dplyr::filter(data, .data[[count]] >= small_freq_threshold),
      aes(colour = .data[[color_var_name]], group = .data[[factor]],
          label = format(.data[[count]], nsmall = 1)),
      position = position_stack(vjust = 0.5), size = label_size
    ) +
    geom_text(
      data = dplyr::filter(data, .data[[count]] < small_freq_threshold),
      aes(x = .data[[count]] + 0.7, y = .data[[var]],
          label = format(.data[[count]], nsmall = 1)),
      color = "black", size = label_size, hjust = 0, inherit.aes = FALSE
    ) +
    theme_minimal() +
    theme(
      legend.position = "none", axis.title.x = element_blank(),
      axis.title.y = element_blank(), axis.text.x = element_blank(),
      axis.ticks.x = element_blank(), axis.text.y = element_text(colour = "black", size = axis_text_y_size),
      panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
      panel.background = element_blank(),
      strip.text = element_text(hjust = 0, size = strip_size, lineheight = 0.8),
      strip.background = element_blank()
    )

  return(p)
}
