#' Plots trend charts
#' 
#' Nice barchart of counts, facets on geography.
#' 
#' @param trend_data data frame with **3 columns, in order,**
#' that are date, count, geography.
#' @param type Character, either "ooh", "ia", or "ihs". Used for y-axis label.
#' @param title Character. Title to go at top. Defaults to an appropriate 
#' "Trends in..."
#' @param title_size numeric, relative title size
#' @param font Character, named font to use.
#' 
#' @export
trend_plot <-
    function (trend_data, type = c("ooh", "ia", "ihs"), title = NA, 
              title_size = 1.2, font = "Frutiger LT Std 45 Light") {
        type <- match.arg(type)
        names(trend_data) <- c("date", "count", "geo")
        switch(type, ooh = {
            stock_ylab <- paste0("Total Cases First Day")
            if (is.na(title)) title <- "Trends in Out-of-Home Care"
            levels(trend_data$geo) <- paste(levels(trend_data$geo), 
                                            "County")
        }, ia = {
            stock_ylab <- paste0("Total Cases First Day")
            if (is.na(title)) title <- "Trends in Investigations & Assessments"
            levels(trend_data$geo) <- paste(levels(trend_data$geo), 
                                            "DCFS Office")
        })
        
        trend_data = trend_data %>% group_by(geo) %>%
            mutate(text_pos = ifelse(count > 0, 
                                     count - 0.05 * max(count), 
                                     0.05 * max(count)),
                   gtzero = factor(count > 0, levels = c(TRUE, FALSE)))
        
        tp <- ggplot(trend_data, aes(x = date, y = count)) + 
            facet_wrap(~geo, ncol = 1, scales = "free") + 
            geom_bar(stat = "identity", fill = poc_colors[1], color = NA) + 
            geom_text(aes(label = formatC(count, big.mark = ","), 
                          y = text_pos, 
                          color = gtzero), 
                      size = 2.5, family = font) + 
            scale_colour_manual(values = c("white", "black")) + 
            theme_bw() +
            scale_y_continuous(labels = scales::comma_format()) + 
            labs(x = "", y = stock_ylab, title = title) + 
            theme(text = element_text(family = font), 
                  axis.title.y = element_text(vjust = .5), 
                  plot.margin = grid::unit(c(1, 1, 1, 1), "lines"), 
                  plot.title = element_text(size = rel(title_size), 
                                            vjust = 1, hjust = 0), 
                  strip.background = element_rect(fill = poc_colors[3], 
                                                  size = NA), 
                  legend.position = "none")
        if (length(levels(trend_data$geo)) == 1) {
            tp <- tp + 
                scale_y_continuous(limits = c(0, 1.1 * max(trend_data$count)),
                                   labels = scales::comma_format())
        }
        print(tp)
    }