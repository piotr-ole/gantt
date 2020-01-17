library(lubridate)
detach('package:lubridate', unload = TRUE)
library(ggplot2)
library(ggthemes)

read_task <- function(file, date_format = '%d/%m/%y', delimiter = ';') {
  task_df <- read.csv(file = file, sep = delimiter, header = TRUE, stringsAsFactors = FALSE)
  task_df$start <- as.Date(task_df$start, date_format)
  task_df$end <- as.Date(task_df$end, date_format)
  task_df$duration <- apply(task_df, function(x) { as.Date(x['end']) - as.Date(x['start'])},
                            MARGIN = 1)
  task_df
}

mutate_task <- function(task) {
  df <- data.frame(point = character(), task = character())
  #df$points <- as.Date(df$points)
  for (i in seq(nrow(task))) {
    df <- rbind(df, data.frame(point = task[i, 'start'], task = task[i, 'task']))
    df <- rbind(df, data.frame(point = task[i, 'end'], task = task[i, 'task']))
  }
  df
}

new_task <- mutate_task(task_df)

gantt <- function(task_file = 'task.csv', delimiter = ';', date_format = '%d/%m/%y') {
  task_df <- read_task('task.csv', date_format, delimiter)
  plot_task <- mutate_task(task_df)
  ggplot(data = new_task, aes(x = point, y = task)) + geom_line(size = 5) + theme_fivethirtyeight()
}

s1 <- task_df$start[1]
e1 <- task_df$end[1]

ggplot(data = new_task, aes(x = point, y = task)) + geom_line(size = 5) + theme_fivethirtyeight()
gantt()
