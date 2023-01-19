# Author: Cyrelle John Domingo

# File path reference
library(here)
library(this.path)

library(corrplot)
library(curl)
library(moments)
library(tidyverse)
source(here("Functions.R"))

# Loading the Data
temp = tempfile()
# bezdekIris.data is used as iris.data have incorrect features according to iris.names
dataset = curl_download("https://archive.ics.uci.edu/ml/machine-learning-databases/iris/bezdekIris.data",
                        temp)
# read.csv also works
dataset = read.table(dataset, header = FALSE, sep = ",")

# Rename columns and factor levels of species
dataset = rename(dataset,
                 "Sepal Length" = "V1",
                 "Sepal Width" = "V2",
                 "Petal Length" = "V3",
                 "Petal Width" = "V4",
                 "Species" = "V5") %>%
  mutate_at("Species", 
            ~recode(.,
                    "Iris-setosa" = "Setosa",
                    "Iris-versicolor" = "Versicolor",
                    "Iris-virginica" = "Virginica"))
# Data Struture
str(dataset)

# Accroding to str(), Species is not a factor so we will change it
dataset$Species = as.factor(dataset$Species)
str(dataset)

# First rows
head(dataset)

# Summary
summary(dataset)

# Checking duplicates
duplicate = dataset[duplicated(dataset), ]
paste0("Number of Duplicates: ", nrow(duplicate))
duplicate


# EDA
col = colnames(dataset[, !colnames(dataset) == "Species"])

skewness = lapply(col, get_values, data = dataset, return = "skewness")
kurtosis = lapply(col, get_values, data = dataset, return = "kurtosis")
plot1 = lapply(col, one_plot, data = dataset, return = "all")
plot1[2] = lapply(col[2], one_plot, data = dataset, hist_bin = 13, return = "all")

save(file = plot1, 
     filename = "One Plot", 
     width = 1846, 
     height = 1765,
     file_is_list = TRUE)
outlier = subset(dataset,
                 `Sepal Width` %in% boxplot(`Sepal Width`,
                                            plot = FALSE)$out)


# Joint Plot

plot2 = list()
plot2[[1]] = jointplot(dataset,
                       x = "Sepal Length",
                       y = "Sepal Width",
                       color = "Species",
                       kde = TRUE,
                       show_legend = TRUE,
                       title = "A")
plot2[[2]] = jointplot(dataset,
                       x = "Petal Length",
                       y = "Petal Width",
                       color = "Species",
                       kde = TRUE,
                       show_legend = TRUE,
                       title = "B")
save(file = plot2, 
     filename = "Joint Plot",
     width = 1938, 
     height = 1362,
     file_is_list = TRUE)


# Pair Plot
plot3 = pair_plot(data = dataset,
                  col = col,
                  color = "Species",
                  title = "Pair Plot of Numerical Variables",
                  save = TRUE)


# Box Plot
m_color = c("#f8776c", "#01bb38", "#619cff")
plot4 = lapply(col, make_boxplot, data = dataset, x = "Species",
               color = "Species", manual_color = m_color,
               out_size = 4)

plot4_dou = list()
plot4_dou[[1]] = wrap_plots(plot4[1:2])
plot4_dou[[1]] = combine_plot(plot4_dou[[1]], 
                              title = "Box Plot of Sepal Characteristics")
plot4_dou[[2]] = wrap_plots(plot4[3:4])
plot4_dou[[2]] = combine_plot(plot4_dou[[2]],
                              title = "Box Plot of Petal Characteristics")
save(file = plot4_dou, 
     filename = "Box Plot Dou", 
     width = 1949, 
     height = 1765,
     file_is_list = TRUE)

plot4_outliers = lapply(plot4, ggplot_outlier)


# Correlation Plot
corr_result = cor(dataset[, col])

plot5 = corr_plot(dataset, col, corr = corr_result)