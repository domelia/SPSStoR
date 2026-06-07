split_bars <- function(data,
                               category_col = 1,
                               value_cols = NULL,
                               bar_color = "#5BB3E6",
                               col_colors = NULL,
                               row_colors = NULL,
                               value_colors = NULL,
                               category_text_color = "grey20",
                               highlight_value_gt = NULL,
                               highlight_value_color = "red",
                               show_col_titles = TRUE,
                               show_labels = TRUE,
                               label_digits = 1,
                               label_size = 5,
                               category_size = 7,
                               title_size = 6.2,
                               bar_height = 0.72,
                               col_gap = 8,
                               label_inside_pad = 0.8,
                               label_outside_pad = 0.8,
                               outside_threshold = 0.15,
                               left_label_width = 34,
                               right_pad = 3,
                               row_box = FALSE,
                               col_labels = NULL,
                               outline_rows = NULL,
                               row_box_linewidth = 0.9,
                               row_box_pad_y = 0.18,
                               wrap_labels = TRUE,
                               wrap_width = 26,
                               base_family = NULL) {

  if (!requireNamespace("ggplot2", quietly = TRUE)) stop("Нужен пакет ggplot2")
  if (!requireNamespace("dplyr", quietly = TRUE)) stop("Нужен пакет dplyr")
  if (!requireNamespace("tidyr", quietly = TRUE)) stop("Нужен пакет tidyr")

  df <- as.data.frame(data)

  if (is.numeric(category_col)) {
    cat_name <- names(df)[category_col]
  } else {
    cat_name <- category_col
  }

  if (is.null(value_cols)) {
    value_cols <- setdiff(names(df), cat_name)
  } else if (is.numeric(value_cols)) {
    value_cols <- names(df)[value_cols]
  }

  if (is.null(col_labels)) {
    col_labels <- value_cols
  } else {
    if (length(col_labels) != length(value_cols)) {
      stop("col_labels должен быть той же длины, что и value_cols")
    }
  }
  df$.category_raw <- as.character(df[[cat_name]])

  if (wrap_labels) {
    if (!requireNamespace("stringr", quietly = TRUE)) stop("Нужен пакет stringr")
    df$.category_lab <- stringr::str_wrap(df$.category_raw, width = wrap_width)
  } else {
    df$.category_lab <- df$.category_raw
  }

  df$.row_id <- seq_len(nrow(df))
  df$.y <- rev(df$.row_id)

  long_df <- df |>
    tidyr::pivot_longer(
      cols = dplyr::all_of(value_cols),
      names_to = "panel",
      values_to = "value"
    )

  long_df$panel <- factor(long_df$panel, levels = value_cols)
  long_df$.panel_id <- as.integer(long_df$panel)

  panel_max <- long_df |>
    dplyr::group_by(panel) |>
    dplyr::summarise(panel_max = max(value, na.rm = TRUE), .groups = "drop")

  long_df <- long_df |>
    dplyr::left_join(panel_max, by = "panel")

  panel_offsets <- data.frame(
    panel = factor(value_cols, levels = value_cols),
    panel_offset = cumsum(c(0, head(panel_max$panel_max + col_gap, -1)))
  )

  long_df <- long_df |>
    dplyr::left_join(panel_offsets, by = "panel")

  resolve_col_colors <- function(value_cols, col_colors, bar_color) {
    if (is.null(col_colors)) {
      out <- rep(bar_color, length(value_cols))
      names(out) <- value_cols
      return(out)
    }

    if (is.null(names(col_colors))) {
      if (length(col_colors) == 1) {
        out <- rep(col_colors, length(value_cols))
        names(out) <- value_cols
      } else if (length(col_colors) == length(value_cols)) {
        out <- col_colors
        names(out) <- value_cols
      } else {
        stop("col_colors должен быть длины 1, длины числа value_cols, или именованным вектором")
      }
    } else {
      miss <- setdiff(value_cols, names(col_colors))
      if (length(miss) > 0) {
        stop("В col_colors не хватает цветов для: ", paste(miss, collapse = ", "))
      }
      out <- col_colors[value_cols]
    }
    out
  }

  resolve_row_fill <- function(cat_raw, row_colors, default_fill) {
    if (is.null(row_colors)) return(default_fill)

    if (!is.null(names(row_colors))) {
      miss <- setdiff(unique(cat_raw), names(row_colors))
      if (length(miss) > 0) {
        stop("В row_colors не хватает цветов для: ", paste(miss, collapse = ", "))
      }
      return(unname(row_colors[cat_raw]))
    }

    if (length(row_colors) == 1) return(rep(row_colors, length(cat_raw)))
    if (length(row_colors) == length(unique(cat_raw))) {
      map <- stats::setNames(row_colors, unique(cat_raw))
      return(unname(map[cat_raw]))
    }

    stop("row_colors должен быть длины 1, длины числа строк, или именованным вектором")
  }

  text_color_for_fill <- function(hex) {
    if (length(hex) == 0 || is.na(hex) || !nzchar(hex)) return("black")
    rgb <- tryCatch(grDevices::col2rgb(hex)[, 1], error = function(e) c(NA, NA, NA))
    if (any(is.na(rgb))) return("black")
    brightness <- 0.299 * rgb[1] + 0.587 * rgb[2] + 0.114 * rgb[3]
    if (isTRUE(brightness >= 145)) "black" else "white"
  }

  panel_fill_map <- resolve_col_colors(value_cols, col_colors, bar_color)
  long_df$.panel_fill <- unname(panel_fill_map[as.character(long_df$panel)])
  long_df$.fill <- resolve_row_fill(long_df$.category_raw, row_colors, long_df$.panel_fill)

  fmt_lab <- function(x) {
    if (is.null(label_digits)) {
      format(x, trim = TRUE, scientific = FALSE)
    } else {
      format(round(x, label_digits), nsmall = label_digits, trim = TRUE, scientific = FALSE)
    }
  }

  long_df$.label <- fmt_lab(long_df$value)
  long_df$.inside_default <- vapply(long_df$.fill, text_color_for_fill, character(1))
  long_df$.outside <- (long_df$value / long_df$panel_max) < outside_threshold

  long_df$.label_x <- ifelse(
    long_df$.outside,
    long_df$panel_offset + long_df$value + label_outside_pad,
    long_df$panel_offset + label_inside_pad
  )

  if (is.null(value_colors)) {
    long_df$.label_col <- ifelse(long_df$.outside, "black", long_df$.inside_default)
  } else if (is.character(value_colors) && length(value_colors) == 1 && is.null(names(value_colors))) {
    long_df$.label_col <- value_colors
  } else if (!is.null(names(value_colors)) && all(value_cols %in% names(value_colors))) {
    long_df$.label_col <- unname(value_colors[as.character(long_df$panel)])
  } else if (!is.null(names(value_colors)) && all(unique(long_df$.category_raw) %in% names(value_colors))) {
    long_df$.label_col <- unname(value_colors[long_df$.category_raw])
  } else {
    stop("value_colors должен быть NULL, одним цветом, именованным вектором по панелям или по строкам")
  }

  if (!is.null(highlight_value_gt)) {
    long_df$.label_col <- ifelse(
      long_df$value > highlight_value_gt,
      highlight_value_color,
      long_df$.label_col
    )
  }

  long_df$xmin <- long_df$panel_offset
  long_df$xmax <- long_df$panel_offset + long_df$value
  long_df$ymin <- long_df$.y - bar_height / 2
  long_df$ymax <- long_df$.y + bar_height / 2

  total_width <- max(long_df$panel_offset + long_df$panel_max)

  title_df <- panel_offsets |>
    dplyr::left_join(panel_max, by = "panel") |>
    dplyr::mutate(
      title_x = panel_offset + panel_max/3,
      title_y = max(long_df$.y) + 0.95
    )

  cat_df <- df |>
    dplyr::transmute(
      .category_raw,
      .category_lab,
      .y,
      label_x = -left_label_width
    )

  box_df <- NULL
  if (isTRUE(row_box) && !is.null(outline_rows)) {
    box_df <- df[df$.category_raw %in% outline_rows, , drop = FALSE]
    if (nrow(box_df) > 0) {
      box_df <- box_df |>
        dplyr::mutate(
          xmin = -left_label_width - 1.5,
          xmax = total_width + right_pad,
          ymin = .y - (bar_height / 2 + row_box_pad_y),
          ymax = .y + (bar_height / 2 + row_box_pad_y),
          box_col = if (!is.null(row_colors) && !is.null(names(row_colors))) {
            unname(row_colors[.category_raw])
          } else {
            bar_color
          }
        )
    }
  }

  p <- ggplot2::ggplot()

  if (!is.null(box_df) && nrow(box_df) > 0) {
    p <- p +
      ggplot2::geom_rect(
        data = box_df,
        ggplot2::aes(
          xmin = xmin,
          xmax = xmax,
          ymin = ymin,
          ymax = ymax
        ),
        inherit.aes = FALSE,
        fill = NA,
        colour = box_df$box_col,
        linewidth = row_box_linewidth
      )
  }

  p <- p +
    ggplot2::geom_rect(
      data = long_df,
      ggplot2::aes(
        xmin = xmin,
        xmax = xmax,
        ymin = ymin,
        ymax = ymax
      ),
      inherit.aes = FALSE,
      fill = long_df$.fill,
      colour = NA
    ) +
    ggplot2::geom_text(
      data = cat_df,
      ggplot2::aes(
        x = label_x,
        y = .y,
        label = .category_lab
      ),
      inherit.aes = FALSE,
      hjust = 0,
      size = category_size,
      colour = category_text_color,
      family = base_family,
      lineheight = 0.95
    )

  if (show_labels) {
    p <- p +
      ggplot2::geom_text(
        data = long_df,
        ggplot2::aes(
          x = .label_x,
          y = .y,
          label = .label
        ),
        inherit.aes = FALSE,
        hjust = 0,
        size = label_size,
        colour = long_df$.label_col,
        family = base_family
      )
  }

  if (show_col_titles) {
    p <- p +
      ggplot2::geom_text(
        data = title_df,
        ggplot2::aes(
          x = title_x,
          y = title_y,
          label = panel
        ),
        inherit.aes = FALSE,
        hjust = 0.5,
        vjust = 0,
        size = title_size,
        family = base_family
      )
  }

  p +
    ggplot2::coord_cartesian(
      xlim = c(-left_label_width, total_width + right_pad),
      ylim = c(0.3, max(long_df$.y) + 1.35),
      clip = "off"
    ) +
    ggplot2::theme_void(base_family = base_family)
}
