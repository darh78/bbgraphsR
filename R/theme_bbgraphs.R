#' My Brand ggplot2 Theme
#'
#' A clean, consistent theme for my plots.
#'
#' @param font_size Base font size.
#' @param font_family Base font family.
#' @return A ggplot2 theme object.
#' @export

theme_bbgraphs <- function(font_size = 12, font_family = "Roboto") {
  # Detect fallback
  available_fonts <- systemfonts::system_fonts()$family
  fallback_font <- if (font_family %in% available_fonts) font_family else "sans"

  theme_minimal(base_size = font_size, base_family = fallback_font) %+replace%
    theme(
      plot.background = element_rect(fill = 'azure1', color = "azure1"),
      panel.grid.minor = element_blank(),
      plot.title = element_text(family = fallback_font, hjust = 0, size = font_size + 5, face = 'bold'),
      plot.subtitle = element_text(family = fallback_font, color = 'gray50', hjust = 0,
                                   margin = margin(2.5, 0, 10, 0), size = font_size - 2),
      plot.caption = element_text(family = fallback_font, color = 'gray25',
                                  margin = margin(-2, 0, 0, 0), hjust = 1, size = font_size - 3),
      axis.title = element_text(family = fallback_font, size = font_size),
      axis.text = element_text(family = fallback_font, size = font_size - 1),
      strip.text = element_text(family = fallback_font, size = font_size, face = "bold")
    )
}
