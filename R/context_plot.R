#' Creates dotplot with focus highlighted
#' 
#' Input context_data needs to have column 1 groups and column 2 data
#' @param context_data data.frame where first columns is a factor for
#' y-axis and second is numeric for x-axis
#' @param focus character corresponding to factor level(s) to highlight
#' @param xlab character for x-axis label
#' @param title character for plot title
#' @param state_label character to match to highlight state
#' @param colors colors for the manual color scale
#' @param title_size relative size of the title
#' @param font Character, name of font to use.
#' @export
context_plot <-
    function (context_data, focus = "none", xlab, title = "", state_label = "Washington", 
              colors = portal_colors[c(8, 4, 2)], title_size = 1.2, font = "Frutiger LT Std 45 Light") {
        names(context_data)[1:2] <- c("focus_group", "x_data")
        context_data$focus_indicator <- ifelse(context_data[, 1] %in% 
                                                   focus, 1, ifelse(context_data[, 1] %in% state_label, 
                                                                    2, 0))
        context_data$focus_indicator <- factor(context_data$focus_indicator)
        context_data[, 1] <- as.character(context_data[, 1])
        too_long <- nchar(context_data[, 1]) > 34
        context_data[too_long, 1] <- str_replace(context_data[too_long, 
                                                              1], pattern = fixed("("), replacement = "\n(")
        context_data[, 1] <- factor(context_data[, 1])
        context_data[, 1] <- reorder(x = context_data[, 1], X = context_data[, 
                                                                             2], order = TRUE)
        ggplot(context_data, aes_string(x = "x_data", y = "focus_group", 
                                        color = "focus_indicator")) + geom_point(size = 5, shape = 18) + 
            labs(x = xlab, y = "", title = title) + theme_bw() + 
            scale_x_continuous(limits = c(0, 1.15 * max(context_data$x_data)), 
                               expand = c(0, 0)) + scale_colour_manual(values = colors) + 
            theme(legend.position = "none", text = element_text(family = font), 
                  axis.title = element_text(size = rel(0.8)), axis.text.y = element_text(size = rel(0.8), 
                                                                                         vjust = 0.5), plot.title = element_text(size = rel(title_size), 
                                                                                                                                 hjust = 0), plot.margin = unit(c(1, 1, 1, 1), 
                                                                                                                                                                "lines"))
    }