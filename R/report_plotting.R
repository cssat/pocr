#' Plots revamped trend charts
#' 
#' Produces (in one device) a bar chart of counts and a 
#' scatterplot w/LOESS smooth of entries and exits with
#' absolute change beanpoles.
#' 
#' @param trend_data data frame with first 4 columns, in order
#' that are date, count, opened, and closed.
#' @param type Character, either "ooh", "ia", or "ihs". Used for y-axis label.
#' @param title Character. Title to go at top.
#' @param flow_alpha set transparency for the background of the activity graph.
#' @param stockplot boolean, default true
#' @param flowplot boolean, default false
#' @param title_size numeric, relative title size
#' @export
trend_plot <- function(trend_data,
                       type = c("ooh", "ia", "ihs"),
                       #count_type = c("unduplicated", "first", "all", "none"),
                       title = "",
                       flow_alpha = 0.6,
                       stockplot = TRUE,
                       flowplot = FALSE,
                       title_size = 1.2) {
    names(trend_data) <- c("date", "count", "opened", "closed")
    type <- match.arg(type)
    #count_type <- match.arg(count_type)
    switch (type,
        "ooh" = {
            stock_ylab <- paste0("Total Cases First Day")  #\n(", count_type, " count)")
            flow_ylab <- "Number of Entries (positive)\nand Exits (negative)"
        },
        "ia" = {
            stock_ylab <- paste0("Total Cases First Day")  #\n(", count_type, " count)")
            flow_ylab <- "Number of Opened (positive)\nand Closed (negative) Cases"
        },
        
        "ihs" = {
            stock_ylab <- paste0("Total Cases First Day")  #\n(", count_type, " count)")
            flow_ylab <- "Number of Opened (positive)\nand Closed (negative) Cases"
        }  
    )
    
    if(stockplot) {
        stock <- ggplot(trend_data, aes(x = date, y = count)) +
            geom_bar(stat = "identity", fill = poc_colors[1]) +
            geom_text(aes(label = formatC(count, big.mark = ","),
                          y = count - 0.05 * max(count)),
                      color = "white",
                      size = 2.5,
                      family = "Frutiger LT Std 45 Light") +
            scale_y_continuous(limits = c(0, 1.25 * max(trend_data[, "count"])),
                               expand = c(0, 0)) +
            labs(x = "",
                 y = stock_ylab,
                 title = title) +
            theme_bw() +
            theme(text=element_text(family = "Frutiger LT Std 45 Light"),
                  axis.title = element_text(size = rel(0.8)),
                  plot.title = element_text(size = rel(title_size), vjust = 1, hjust = 0))
    }
    
    if (flowplot) {
        flow <- ggplot(trend_data, aes_string(x = "date")) +
            stat_smooth(aes(y =  opened), method = "loess", span = 0.4, color = portal_colors[2], size = 1, se = FALSE) +
            stat_smooth(aes(y = -closed), method = "loess", span = 0.4, color = portal_colors[3], size = 1, se= FALSE) +
            geom_point( aes(y =  opened), color = alpha(portal_colors[2], flow_alpha)) +
            geom_point( aes(y = -closed), color = alpha(portal_colors[3], flow_alpha)) +
            geom_line(  aes(y =  opened), color = alpha(portal_colors[2], flow_alpha)) +
            geom_line(  aes(y = -closed), color = alpha(portal_colors[3], flow_alpha)) +
            geom_hline(yintercept = 0, size = 0.8, color = "gray50") +
            geom_rect(aes(xmax = date + 1,
                          xmin = date - 1,
                          ymin = 0,
                          ymax = opened - closed),
                      fill = portal_colors[4], colour = portal_colors[4]) +
            labs(y = flow_ylab,
                 x = "") + 
            theme_bw() +
            theme(axis.title = element_text(size = rel(0.8)))
    }
    
    if (stockplot & flowplot) {
        grid.arrange(stock, flow)
    } else if (stockplot) {
        print(stock)
    } else {
        print(flow)
    }
}


#' Plots re-revamped trend charts
#' 
#' Nice barchart of counts
#' 
#' @param trend_data data frame with first 4 columns, in order
#' that are date, count, opened, and closed.
#' @param type Character, either "ooh", "ia", or "ihs". Used for y-axis label.
#' @param title Character. Title to go at top.
#' @param title_size numeric, relative title size
#' @export
trend_plot2 <- function(trend_data,
                        type = c("ooh", "ia", "ihs"),
                        title = "",
                        title_size = 1.2) {
    names(trend_data) <- c("date", "count", "opened", "closed")
    type <- match.arg(type)
    #count_type <- match.arg(count_type)
    switch (type,
            "ooh" = {
                stock_ylab <- paste0("Total Cases First Day")  #\n(", count_type, " count)")
                flow_ylab <- "Number of Entries (positive)\nand Exits (negative)"
            },
            "ia" = {
                stock_ylab <- paste0("Total Cases First Day")  #\n(", count_type, " count)")
                flow_ylab <- "Number of Opened (positive)\nand Closed (negative) Cases"
            },
            
            "ihs" = {
                stock_ylab <- paste0("Total Cases First Day")  #\n(", count_type, " count)")
                flow_ylab <- "Number of Opened (positive)\nand Closed (negative) Cases"
            }  
    )

    stock <- ggplot(trend_data, aes(x = date, y = count)) +
        geom_bar(stat = "identity", fill = poc_colors[1]) +
        geom_text(aes(label = formatC(count, big.mark = ","),
                      y = count - 0.05 * max(count)),
                  color = "white",
                  size = 2.5,
                  family = "Frutiger LT Std 45 Light") +
        scale_y_continuous(limits = c(0, 1.25 * max(trend_data[, "count"])),
                           expand = c(0, 0)) +
        labs(x = "",
             y = stock_ylab,
             title = title) +
        theme_bw() +
        theme(text=element_text(family = "Frutiger LT Std 45 Light"),
              axis.title = element_text(size = rel(0.8)),
              plot.title = element_text(size = rel(title_size), vjust = 1, hjust = 0))
    
    print(stock)
}


#' Plots re-revamped trend charts
#' 
#' Nice barchart of counts, facets on geography.
#' 
#' @param trend_data data frame with **3 columns, in order,**
#' that are date, count, geography.
#' @param type Character, either "ooh", "ia", or "ihs". Used for y-axis label.
#' @param title Character. Title to go at top. Defaults to an appropriate "Trends in..."
#' @param title_size numeric, relative title size
#' @export
trend_plot3 <- function(trend_data,
                        type = c("ooh", "ia", "ihs"),
                        title = NA,
                        title_size = 1.2) {
    type <- match.arg(type)
    names(trend_data) <- c("date", "count", "geo")
    switch (type,
            "ooh" = {
                stock_ylab <- paste0("Total Cases First Day")
                if (is.na(title)) title <- "Trends in Out-of-Home Care"
                levels(trend_data$geo) <- paste(levels(trend_data$geo), "County")
            },
            "ia" = {
                stock_ylab <- paste0("Total Cases First Day")
                if (is.na(title)) title <- "Trends in Investigations & Assessments"
                levels(trend_data$geo) <- paste(levels(trend_data$geo), "DCFS Office")
            },
            
            "ihs" = {
                stock_ylab <- paste0("Total Cases First Day") 
                if (is.na(title)) title <- "Trends in In-Home Service"
                levels(trend_data$geo) <- paste(levels(trend_data$geo), "DCFS Office")
            }  
    )
    
    trend_data <- ddply(trend_data, .variables = "geo",
                   mutate,
                   text_pos = ifelse(count > 0, count - 0.05 * max(count),
                                     0.05 * max(count)),
                   gtzero = factor(count > 0, levels = c(TRUE, FALSE)))
    
    
    tp <- ggplot(trend_data, aes(x = date, y = count)) +
        facet_wrap(~ geo, ncol = 1, scales = "free") +
        geom_bar(stat = "identity", fill = poc_colors[1], color = NA) +
        geom_text(aes(label = formatC(count, big.mark = ","),
                      y = text_pos,
                      color = gtzero),
                  size = 2.5,
                  family = "Frutiger LT Std 45 Light") +
        scale_colour_manual(values = c("white", "black")) +
        theme_bw() + 
        scale_y_continuous(labels = comma_format()) +
        labs(x = "",
             y = stock_ylab,
             title = title) +
        theme(text = element_text(family = "Frutiger LT Std 45 Light"),
              axis.title.y = element_text(vjust = -.05),
              plot.margin = unit(c(1, 1, 1, 1), "lines"),
              plot.title = element_text(size = rel(title_size), vjust = 1, hjust = 0),
              strip.background = element_rect(fill = poc_colors[3], size = NA),
              legend.position = "none")
    if (length(levels(trend_data$geo)) == 1) {tp <- tp + scale_y_continuous(limits = c(0, 1.1 * max(trend_data$count)))}
    print(tp)
}




#' Plots re-revamped trend charts
#' 
#' Nice barchart of counts, facets on geography.
#' 
#' @param trend_data data frame with **2 columns, in order,**
#' that are date, count.
#' @param type Character, either "ooh", "ia", or "ihs". Used for y-axis label.
#' @param title Character. Title to go at top. Defaults to an appropriate "Trends in..."
#' @param title_size numeric, relative title size
#' @export
trend_plot_state <- function(trend_data,
                        type = c("ooh", "ia", "ihs"),
                        title = NA,
                        title_size = 1.2) {
  type <- match.arg(type)
  names(trend_data) <- c("date", "count")
  switch (type,
          "ooh" = {
            stock_ylab <- paste0("Total Cases First Day")
            if (is.na(title)) title <- "Trends in Out-of-Home Care"
           # levels(trend_data$geo) <- paste(levels(trend_data$geo), "County")
          },
          "ia" = {
            stock_ylab <- paste0("Total Cases First Day")
            if (is.na(title)) title <- "Trends in Investigations & Assessments"
           # levels(trend_data$geo) <- paste(levels(trend_data$geo), "DCFS Office")
          },
          
          "ihs" = {
            stock_ylab <- paste0("Total Cases First Day") 
            if (is.na(title)) title <- "Trends in In-Home Service"
            #levels(trend_data$geo) <- paste(levels(trend_data$geo), "DCFS Office")
          }  
  )
  
  trend_data$text_pos <-  ifelse(trend_data$count > 0,
                                 trend_data$count - 0.05 * max(trend_data$count),
                                 0.05 * max(trend_data$count))
  trend_data$gtzero <- factor(trend_data$count > 0, levels = c(TRUE, FALSE))
  
  tp <- ggplot(trend_data, aes(x = date, y = count)) +
    #facet_wrap(~ geo, ncol = 1, scales = "free") +
    geom_bar(stat = "identity", fill = poc_colors[1], color = NA) +
    geom_text(aes(label = formatC(count, big.mark = ","),
                  y = text_pos,
                  color = gtzero),
              size = 2.5,
              family = "Frutiger LT Std 45 Light") +
    scale_colour_manual(values = c("white", "black")) +
    theme_bw() + 
    scale_y_continuous(labels = comma_format()) +
    labs(x = "",
         y = stock_ylab,
         title = title) +
    theme(text = element_text(family = "Frutiger LT Std 45 Light"),
          axis.title.y = element_text(vjust = -.05),
          plot.margin = unit(c(1, 1, 1, 1), "lines"),
          plot.title = element_text(size = rel(title_size), vjust = 1, hjust = 0),
          strip.background = element_rect(fill = poc_colors[3], size = NA),
          legend.position = "none")
  #if (length(levels(trend_data$geo)) == 1) {tp <- tp + scale_y_continuous(limits = c(0, 1.1 * max(trend_data$count)))}
  print(tp)
}





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
#' @export
context_plot <- function(context_data, focus = "none",
                         xlab,
                         title = "",
                         state_label = "Washington",
                         colors = portal_colors[c(8, 4, 2)],
                         title_size = 1.2) {
    names(context_data)[1:2] <- c("focus_group", "x_data")
    context_data$focus_indicator <- ifelse(context_data[, 1] %in% focus, 1, ifelse(context_data[, 1] %in% state_label, 2, 0))
    context_data$focus_indicator <- factor(context_data$focus_indicator)
    context_data[, 1] <- as.character(context_data[, 1])
    too_long <- nchar(context_data[, 1]) > 34
    context_data[too_long, 1] <- str_replace(context_data[too_long, 1], pattern = fixed("("), replacement = "\n(")
    context_data[, 1] <- factor(context_data[, 1])
    context_data[, 1] <- reorder(x = context_data[, 1], X = context_data[, 2], order = TRUE)
    ggplot(context_data, aes_string(x = "x_data", y = "focus_group",
                                    color = "focus_indicator")) +
        geom_point(size = 5, shape = 18) +
        labs(x = xlab,
             y = "",
             title = title) +
        theme_bw() +
        scale_x_continuous(limits = c(0, 1.15 * max(context_data$x_data)), expand = c(0, 0)) +
        scale_colour_manual(values = colors) +
        theme(legend.position = "none",
              text = element_text(family = "Frutiger LT Std 45 Light"),
              axis.title = element_text(size = rel(0.8)),
              axis.text.y = element_text(size = rel(0.8), vjust = 0.5),
              plot.title = element_text(size = rel(title_size), hjust = 0),
              plot.margin = unit(c(1, 1, 1, 1), "lines"))
}


#' Expands a data.frame to include all unique combinations of variables
#' and fills in 0's (or other) for missing values
#' 
#' @param dat data.frame to fill in
#' @param value_name names of columns to be filled in (specify this or \code{value_index})
#' @param value_index indices of columns to be filled in (specify this or \code{value_name})
#' @param fill_value how to fill in holes after expansion. (Defaults to 0.)
#' @export
fill_in <- function(dat, value_name = NULL, value_index = NULL, fill_value = 0) {
    if (any(is.na(dat))) warning("data.frame for filling ing has NAs which were be replaced.")
    if (is.null(value_index) & (is.null(value_name) | value_name %nin% names(dat))) {
        stop("Invalid value column specification for filling in.")
    }
    if (is.null(value_index)) {value_index <- which(names(dat) == value_name)}
    
    uniques <- lapply(dat[, -value_index], FUN = unique)
    dat_full <- do.call(expand.grid, args=uniques)
    dat_full <- merge(x = dat_full, y = dat, all.x = TRUE)
    dat_full[is.na(dat_full)] <- 0
    return(dat_full[, names(dat)])
}
