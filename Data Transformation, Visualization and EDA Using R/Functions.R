# Author: Cyrelle John Domingo

# File path reference
library(here)
library(this.path)

library(cowplot)
library(ggplot2)
library(patchwork)
library(RcppAlgos)

combine_plot = function(patchwork_data, title = NULL, title_theme = custom_title()) {
  result = patchwork_data + 
    plot_annotation(title = title,
                    theme = title_theme)
  
  return(result)
}

custom_title = function(color = "#3F3A39") {
  result = theme(plot.title = element_text(color = color,
                                           face = "bold",
                                           hjust = 0.5,
                                           size = 15))
  return(result)
}

custom_theme = function(default_theme = TRUE, option = NULL) {
  cust_color = "#E7E6E6"
  
  if (default_theme == TRUE) {
    result = theme_gray() +
      theme(axis.title = element_text(color = "#3E3B39"),
            panel.background = element_rect(color = "gray",
                                            fill = "white"),
            plot.title = element_text(color = "#3E3B39",
                                      face = "bold",
                                      hjust = 0.5),
            legend.text = element_text(color = "#3E3B39"),
            legend.title = element_text(color = "#3E3B39"),
            axis.ticks = element_line(color = "#3F3A39"))
  }
  
  else {
    result = theme()
  }
  
  if (option == 1) {
    result = result +
      theme(panel.grid = element_line(color = cust_color),
            panel.grid.major.x = element_blank(),
            panel.grid.minor = element_blank())
  }
  
  else if (option == 2) {
    result = result +
      theme(panel.grid.major.x = element_line(color = cust_color),
            panel.grid.major.y = element_line(color = cust_color))
  }
  
  else if (option == 3) {
    result = result +
      theme(panel.grid = element_line(color = cust_color))
    
    result = result + custom_title(color = "#100C07")
  }
  
  else if (option == 4) {
    result = result + 
      theme(axis.text.x = element_blank(),
            axis.ticks.x = element_blank())
  }
  
  else if (option == 5) {
    result = result +
      theme(axis.text.y = element_blank(),
            axis.ticks.y = element_blank())
  }
  
  else if (option == 6) {
    result = result +
      theme(panel.grid = element_line(color = cust_color),
            legend.position = "none")
  }
  
  return(result)
}

format_dec = function(num, decimal = 2) {
  result = format(round(num, decimal),
                  nsmall = decimal)
  return(result)
}

get_color = function(col) {
  if (col == "Sepal Length") {
    color = "#AF4BCE"
    box_color = "#BE8AD1"
  }
  
  else if (col == "Sepal Width") {
    color = "#7D3AC1"
    box_color = "#A27EC6"
  }
  
  else if (col == "Petal Length") {
    color = "#DB4CB2"
    box_color = "#D88DC3"
  }
  
  else if (col == "Petal Width") {
    color = "#EB548C"
    box_color = "#E292B1"
  }
  
  result = c(color, box_color)
  return(result)
}

get_values = function(data, col, return = NULL) {
  temp_data = data[, col]
  if (return == "kurtosis") {
    kurtosis = kurtosis(temp_data)
    kurtosis = format_dec(kurtosis, 3)
    return(kurtosis)
  }
  
  else if (return == "skewness") {
    skewness = skewness(temp_data)
    skewness = format_dec(skewness, 3)
    return(skewness)
  }
}

jointplot = function(data, x, y, color = NULL, kde = FALSE,
                     remove_xvalue = FALSE, remove_yvalue = FALSE,
                     remove_xylabel = FALSE, show_legend = FALSE,
                     title = NULL) {
  legend_title = color
  xlabel = x
  ylabel = y
  
  if (remove_xylabel == TRUE) {
    xlabel = NULL
    ylabel = NULL
  }
  
  if (is.null(color)) {
    kde = FALSE
    
    plot = ggplot(dataset,
                  aes(x = data[, x],
                      y = data[, y]))
  }
  
  else {
    plot = ggplot(dataset,
                  aes(x = data[, x],
                      y = data[, y],
                      color = data[, color]))
  }
  
  plot = plot + geom_point(show.legend = show_legend) +
    labs(title = title,
         color = legend_title,
         x = xlabel,
         y = ylabel) +
    custom_theme(option = 3)
  
  if (remove_xvalue == TRUE) {
    plot = plot + custom_theme(default_theme = FALSE, option = 4)
  }
  
  if (remove_yvalue == TRUE) {
    plot = plot + custom_theme(default_theme = FALSE, option = 5)
  }
  
  if (kde == TRUE) {
    xdensity = axis_canvas(plot, axis = "x") +
      geom_density(data = data,
                   aes(x = data[, x],
                       fill = data[, color],
                       color = data[, color]),
                   alpha = 0.7, size = 0.2)
    
    ydensity = axis_canvas(plot, axis = "y", coord_flip = TRUE) +
      geom_density(data = data,
                   aes(x = data[, y], 
                       fill = data[, color], 
                       color = data[, color]),
                   alpha = 0.7, size = 0.2) +
      coord_flip()
    
    result = insert_xaxis_grob(plot = plot, 
                               grob = xdensity,
                               position = "top")
    
    result = insert_yaxis_grob(result,
                               ydensity,
                               position = "right")
    result = ggdraw(result)
  }
  
  else if (kde == FALSE) {
    result = plot
  }
  
  return(result)
}

make_boxplot = function(data, x, y, color, manual_color = NULL,
                        out_color = "#545555", out_shape = 18,
                        out_size = 5, title = NULL) {
  box = ggplot(data,
               aes(x = data[, x],
                   y = data[, y],
                   color = data[, color],
                   fill = data[, color])) +
    stat_boxplot(geom = "errorbar",
                 size = 1.5,
                 width = 0.5) +
    geom_boxplot(aes(ymin = ..lower..,
                     ymax = ..upper..),
                 color = "#323332",
                 outlier.color = out_color,
                 outlier.shape = out_shape,
                 outlier.size = out_size) +
    labs(title = title,
         x = x,
         y = y) +
    custom_theme(option = 6)
  
  if (!is.null(manual_color)) {
    box = box + scale_color_manual(values = manual_color,
                                   aesthetics = c("color",
                                                  "fill"))
  }
  
  return(box)
}

ggplot_outlier = function(ggplot_list) {
  result = ggplot_build(ggplot_list)$data[[1]][["outliers"]]
  
  return(result)
}

one_boxplot = function(data, x, box_border_color = "black", color_fill,
                       outlier_color = NULL, outlier_shape = 19,
                       outlier_size = 1.5, ylabel = NULL) {
                        
  box = ggplot(data,
               aes(y = x)) +
    stat_boxplot(color = color_fill,
                 geom = "errorbar",
                 size = 1.5,
                 width = 0.5) +
    geom_boxplot(aes(ymin = ..lower..,
                     ymax = ..upper..),
                 color = box_border_color,
                 fill = color_fill,
                 outlier.color = outlier_color,
                 outlier.shape = outlier_shape,
                 outlier.size = outlier_size) +
    labs(title = "Box Plot",
         y = ylabel) +
    custom_theme(option = 1)
  
  return(box)
}

make_hist = function(data, x, alpha = 1, bin = 9, color = "gray", 
                     fill = "white", kde = 0, kde_color = NULL,
                     xlabel = NULL) {
  bw = (max(x) - min(x)) / bin
  bw = nrow(data) * bw
  
  hist = ggplot(data,
                aes(x = x)) +
    geom_histogram(alpha = alpha,
                   color = color,
                   fill = fill,
                   bins = bin) +
    labs(title = "Histogram Plot",
         x = xlabel,
         y = "Total") +
    custom_theme(option = 1)
  
  if (kde == 1) {
    hist = hist + geom_density(aes(y = after_stat(density * bw)),
                        color = kde_color, size = 1)
  }
  
  return(hist)
}

make_qqplot = function(data, x, color, line_color) {
  qq = ggplot(data,
              aes(sample = x)) +
    geom_qq(color = color) +
    geom_qq_line(color = line_color,
                 size = 1,
                 alpha = 0.7) +
    labs(title = "Q-Q Plot",
         y = "Sample Quantiles",
         x = "Theoritical Quantiles") +
    custom_theme(option = 2)
  
  return(qq)
}

one_plot = function(data, col, hist_bin = 9, return = NULL) {
  color = get_color(col)
  
  if (return == "boxplot" || return == "all") {
    boxplot = one_boxplot(data,
                          x = data[, col],
                          color_fill = color[2],
                          box_border_color = "#784989",
                          outlier_color = "#545555",
                          outlier_shape = 18,
                          outlier_size = 4,
                          ylabel = col)
                           
    result = boxplot
  }
  
  if (return == "histogram" || return == "all") {
    histogram = make_hist(data,
                          x = data[, col],
                          alpha = 0.7,
                          bin = hist_bin,
                          color = "#E5C5EE",
                          fill = color[1],
                          kde = 1,
                          kde_color = color[1],
                          xlabel = col)
    result = histogram
  }
  
  if (return == "qqplot" || return == "all") {
    qqplot = make_qqplot(data,
                         x = data[, col],
                         color = color[1],
                         line_color = "#CF251E")
    result = qqplot
  }
  
  if (return == "all") {
    plot = combine_plot(patchwork_data = boxplot | (histogram / qqplot),
                        title = paste(col, "Distribution"),
                        title_theme = custom_title())
    result = plot
  }
  return(result)
}

pair_plot = function(data, col, color, inc_lower = TRUE, inc_upper = TRUE,
                     loption = "points", uoption = "points",
                     title = NULL, save = FALSE, ext = "png", 
                     path = here("Files"), units = "px") {
  max = 1:ncol(dataset[col])
  all = max * max
  
  if (inc_lower == FALSE) {
    loption = "blank"
  }
  
  else if (inc_upper == FALSE) {
    uoption = "blank"
  }
  
  lower = comboGrid(max, max, "Lower")
  upper = comboGrid(max, max, "Upper", repetition = FALSE)
  upper = upper[, c(2, 1, 3)]
  whole = rbind(lower, upper)
  whole = arrange(whole, whole[, 1])
  c = colnames(lower)
  colnames(upper) = c(c[1], c[2], c[3])
  whole = rbind(lower, upper)
  whole = arrange(whole, whole[, 1:2])

  plot_list = lapply(1:nrow(whole), process_pp, color = color, 
                   max = length(col), data = dataset, select_col = whole,
                   lower_option = loption, upper_option = uoption)
  
  plot = wrap_plots(plot_list,
                    ncol = length(max), 
                    byrow = FALSE, 
                    guides = "collect") &
    theme(legend.position = "bottom")
  
  if (!is.null(title)) {
    plot = plot + plot_annotation(title = title,
                                  theme = custom_title())
  }
  
  if (save == TRUE) {
    filename = paste("Pair Plot.", ext, sep = "")
    
    save(plot, 
         filename = filename,
         path = path,
         width = 2078,
         height = 1765,
         ext = ext,
         units = units)
  }
  
  return(plot)
}

process_pp = function(data, row, color, max, select_col,
                      lower_option, upper_option, lower_design = TRUE,
                      upper_design = FALSE) {
  approve = 0 
  option = 0
  x = select_col[row, 1]
  y = select_col[row, 2]
  position = select_col[row, 3]
  
  if (position == "Lower") {
    approve = "lower"
    option = lower_option
  }
  
  else if (position == "Upper") {
    approve = "upper"
    option = upper_option
  }
  
  if (x == y) {
    option = "diag_text"
  }
  
  if (option == "points") {
    plot = jointplot(data = data,
                     x = x,
                     y = y,
                     color = color,
                     remove_xylabel = TRUE,
                     show_legend = TRUE)
  }
  
  else if (option == "blank") {
    plot = ggplot() + theme_void()
  }
  
  else if (option == "diag_text") {
    diag = colnames(data[x])
    
    plot = ggplot() +
      theme_void() +
      geom_text(aes(x = 0,
                    y = 0,
                    label = diag),
                color = "#3F3A39",
                size = 5)
  }
  
  if (approve == "lower") {
    if (lower_design == FALSE) {
      plot = plot + custom_theme(default_theme = FALSE, option = 4)
      plot = plot + custom_theme(default_theme = FALSE, option = 5)
    }
    
    if (x == 1 && y == max) {
      plot = plot
    }
    
    else if (x == 1) {
      
      if (option == "points") {
        plot = plot + custom_theme(default_theme = FALSE, option = 4)
      }
    }
    
    else if (y == max) {
      
      if (option == "points") {
        plot = plot + custom_theme(default_theme = FALSE, option = 5)
      }
    }
    
    else if (x != 1 && y != max) {
      
      if (option == "points") {
        plot = plot + custom_theme(default_theme = FALSE, option = 4)
        plot = plot + custom_theme(default_theme = FALSE, option = 5)
      }
    }
  }
  
  if (approve == "upper") {
    if (upper_design == FALSE) {
      plot = plot + custom_theme(default_theme = FALSE, option = 4)
      plot = plot + custom_theme(default_theme = FALSE, option = 5)
    }
    
    if (x == max && y == 1) {
      
      if (option == "points") {
        plot = plot + scale_x_continuous(position = "top") +
          scale_y_continuous(position = "right")
      }
    }
    
    else if (x == max) {
      
      if (option == "points") {
        plot = plot + custom_theme(default_theme = FALSE, option = 4) +
          scale_y_continuous(position = "right")
      }
    }
    
    else if (y == 1) {
      
      if (option == "points") {
        plot = plot + custom_theme(default_theme = FALSE, option = 5) +
          scale_x_continuous(position = "top") 
      }
    }
    
    else if (x != 1 && y != max) {
      
      if (option == "points") {
        plot = plot + custom_theme(default_theme = FALSE, option = 4)
        plot = plot + custom_theme(default_theme = FALSE, option = 5)
      }
    }
  }
  
  return(plot)
}

save = function(file, filename, path = here("Files"), ext = "png", width = NA,
                height = NA, units = "px", file_is_list = FALSE) {
  count = 0
  
  if (file_is_list == FALSE) {
    ggsave(filename,
           path = path,
           width = width,
           height = height,
           units = units)
  }
  
  else if (file_is_list == TRUE) {
    for (i in file) {
      count = count + 1
      temp_name = paste(filename, " ", count, ".", ext, sep = "")
      
      plot = file[[count]]
      
      ggsave(temp_name,
             plot = plot,
             path = path,
             width = width,
             height = height,
             units = units)
    }
  }
}

corr_plot = function(data, col, corr, diag_color = "#3F3A39", order = "hclust",
                     upper = "ellipse") {
  corr_result = cor(data[, col])
  
  corrplot.mixed(corr = corr_result,
                 order = order,
                 tl.col = diag_color,
                 upper = upper)
  plot = recordPlot()
  
  return(plot)
}