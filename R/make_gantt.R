library(lubridate)
detach('package:lubridate', unload = TRUE)
library(ggplot2)
library(ggthemes)

read_task <- function(file = 'task.csv', date_format = '%d/%m/%y', delimiter = ';') {
  task_df <- read.csv(file = file, sep = delimiter, header = TRUE, stringsAsFactors = FALSE)
  task_df$start <- as.Date(task_df$start, date_format)
  task_df$end <- as.Date(task_df$end, date_format)
  task_df$type <-  as.factor(task_df$type)
  task_df$duration <- apply(task_df, function(x) { as.Date(x['end']) - as.Date(x['start'])},
                            MARGIN = 1)
  task_df$value <- seq(1:nrow(task_df))
  task_df
}

mutate_task <- function(task) {
  df <- data.frame(point = character(), task = character())
  for (i in seq(nrow(task))) {
    df <- rbind(df, data.frame(point = task[i, 'start'], task = task[i, 'task'], type = task[i, 'type']))
    df <- rbind(df, data.frame(point = task[i, 'end'], task = task[i, 'task'], type = task[i, 'type']))
  }
  df
}

gantt <- function(task_file = 'task.csv', summary_file = 'summary.csv',
                  delimiter = ';', date_format = '%d/%m/%y', title = 'Gantt chart') {
  task_df <- read_task(task_file, date_format, delimiter)
  plot_task <- mutate_task(task_df)
  summaries <- create_summaries(task_file, summary_file)
  ggplot(data = plot_task, aes(x = point, y = task)) + 
      geom_line(aes(color = type), size = 5) + 
      geom_segment(aes(x = start, y = value, xend = end, yend = value), data = summaries, 
                   arrow = arrow(length = unit(0.2,'cm'), ends = 'both'), 
                   lineend = 'butt', size = 1) +
      theme_fivethirtyeight() +
      ggtitle(title) + 
      theme(plot.title = element_text(hjust = 0.5)) +
      geom_text(aes(x = middle_date(start, end), y = value + 0.2, label = summary) , data = summaries)
}

middle_date <- function(start, end) {
    as.Date(start) + (as.Date(end) - as.Date(start)) / 2
}
              
create_summaries <- function(task_file = 'task.csv', summary_file = 'summary.csv') {
    task <- read_task(task_file)
    summary <- read.csv2(summary_file, header = TRUE, stringsAsFactors = FALSE)
    l <- list()
    for (i in seq(nrow(summary))) {
        summary_task_names <- unlist(strsplit(summary[i, 'tasks'], split = '|', fixed = TRUE))
        summary_tasks <- task[task$task %in% summary_task_names, ]
        l <- add_to_list(l, find_summary_position(summary_tasks, summary[i, 'summary']))
    } 
    l[[1]] <- l[[1]][[1]]
    as.data.frame(data.table::rbindlist(l))
}

find_summary_position <- function(summary_tasks, summary_name) {
    min_start <- summary_tasks[1, 'start']
    max_end <- summary_tasks[1, 'end']
    value <- summary_tasks[1, 'value']
    #browser()
    for (i in seq(nrow(summary_tasks))) {
        if (as.Date(min_start) > as.Date(summary_tasks[i, 'start'])) {
            min_start <-  summary_tasks[i, 'start']
        }
        if (as.Date(max_end) < as.Date(summary_tasks[i, 'end'])) {
            max_end <- summary_tasks[i, 'end']
        }
        if (value < summary_tasks[i, 'value']) {
            value <- summary_tasks[i, 'value']
        }
    }
    data.frame(start = min_start, end = max_end, summary = summary_name, value = value + 0.3, type = 'summary')
}

add_to_list <- function(l, obj) {
    if (length(l) == 0) return(list(obj))
    else return(list(l, obj))
}

gantt()
