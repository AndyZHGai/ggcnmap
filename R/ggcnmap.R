#' China map based ggplot2 version 2
#'
#' @param data a data frame containing values to fill the provinces
#' @param fill.variable a numeric vector
#' @param limits the range of fill variable
#' @param midpoint the midpoint of fill variable
#' @param breaks the breaks of fill variable
#' @param low low color
#' @param high high color
#' @param mid the color of midpoint
#'
#' @author ZhonghuiGai
#' @return a ggplot
#' @export
#'
#' @examples
#' data <- ggcnmap::province
#' library(showtext)
#' showtext_auto(enable = TRUE)
#' font_add('Songti', 'Songti.ttc','STKaiti','STXihei')
#' ggcnmap(data = data, fill.variable = "value", fill = 2,
#'         limits = c(40, 600),
#'         midpoint = 5,
#'         breaks = c(50, 80, 110, 140, 600))
ggcnmap <- function(data, fill.variable = "Value", save = FALSE, size = 6,
                    low = "#3c9eff", high = "#ff445d", mid = "#f8d248", fill = 2,
                    limits = c(4000, 19000), midpoint = 12000,
                    breaks = c(5000, 8000, 11000, 14000, 17000)){
  library(ggplot2)
  library(sf)
  map = "china"
  map.theme <- function(data){
    p <- ggplot(data = data) +
      theme(panel.background = element_blank(),
            panel.grid = element_blank(),
            axis.text = element_blank(),
            axis.title = element_blank(),
            axis.ticks = element_blank(),
            legend.background = element_blank(),
            legend.title = element_blank(),
            legend.text = element_text(size = 1.5*size, face = "bold"),
            legend.key.size = unit(0.3, "cm"),
            legend.key.width = unit(0.1, "cm"),
            legend.position = c(0.2, 0.3),
            legend.spacing.x = unit(0.1, 'lines')) +
      xlab(NULL) + ylab(NULL) +
      guides(fill = guide_colorbar(title = "HP",
                                   label.position = "right",
                                   title.position = "top", title.vjust = 1,
                                   frame.colour = "white",
                                   barwidth = 0.6,
                                   barheight = 8)) +
      scale_x_continuous(expand = c(0,0)) +
      scale_y_continuous(expand = c(0,0))
    return(p)
  }
  nine.lines <- ggcnmap::nine.lines
  if (map == "china") {
    china <- ggcnmap::china
    center <- ggcnmap::center
    p <-  dplyr::full_join(china, data, type = "full")  |>
      map.theme() +
      geom_sf(mapping = aes(fill = get(fill.variable)),
                       color = "gray99", size = 0.01,
                       show.legend = TRUE, na.rm = T)  +
      geom_sf(data = nine.lines, color = "black", size = 1) +
      geom_text(data = center, aes(x = jd, y = wd, label = ShortName),
                fontface = "bold", colour = "black", size = size)
    if (fill == 2) {
      p <- p + scale_fill_gradient(high = high, low = low)
    }else if (fill == 3) {
      p <- p + scale_fill_gradient2(name = fill.variable, limits = limits,
                                    low = low, mid = mid, high = high,
                                    midpoint = midpoint, breaks = breaks)
    }
  }else if (map == "guojie") {
    guojie <- ggcnmap::guojie
    p <- map.theme(data = guojie) + geom_sf(show.legend = FALSE)
  }else if (map == "nine.lines") {
    p <- map.theme(data = nine.lines) + geom_sf(show.legend = FALSE)
  }
  if (save == TRUE) {
    ggsave(plot = p, file = "test.pdf", device = cairo_pdf, family = "yahei")
  }
  return(p)
}
