##
# Set of functions to reproduce UpSetR style graph with binary encoding for combinations
#

#' Create a binary mask for each value
#' @param values character
#' @family upset
#' @return vector of integer value as binary mask for each input values, named by input value
#' @export
create_binary_mask = function(values) {
  values.mask = c() # Binary mask for each values
  for(i in seq_along(values)) {
    n = values[i]
    values.mask = values.mask * 2L
    values.mask[n] = 1L
  }
  values.mask
}

#' Get corresponding label from a binary group value
#' @param data data.frame
#' @param mask integer vector binary mask for individual labels
#' @param group name of the column in data containing binary group value
#' @param trans translate name if TRUE use \code{\link{i18n}}, if function use it a translator
#' @family upset
#' @export
get_labels_from_binary = function(data, mask, group="g", trans=FALSE) {
  nn = names(mask)
  if(isFALSE(trans)) {
    tr = identity
  } else {
    if(is.function(trans)) {
      tr = trans
    } else {
      tr = i18n
    }
  }
  for(n in nn) {
    m = mask[n]
    data[[n]] = bitwAnd(data[[group]], m) > 0
  }
  data$label = apply(data[, nn], 1, function(x) {
    paste(tr(nn[which(x)]), collapse = '+')
  })
  data
}

#' Create a binary group value from a set of logical columns.
#'
#' Intersection of columns will be encoded as a binary value (1 column => 1 bit)
#'
#' @param data data.frame
#' @param mask binary mask for each column (see \code{\link{create_binary_mask}})
#' @param column name of the column that will contain the group value
#' @family upset
#' @export
apply_binary_mask = function(data, mask, column) {
  data[[column]] = 0L
  nn = names(mask)
  for(i in seq_along(nn)) {
    n = nn[i]
    m = mask[n]
    y = as.integer(data[[n]]) * m
    y[is.na(y)] = 0L
    data[[column]] = data[[column]] + y
  }
  data
}

#' Create new groups from a set of columns
#'
#' Names of new groups will be the name of groups entry
#'
#' @param data data.frame
#' @param groups list of group, each group is a vector of names of columns belonging to this group
#' @param trans translate label if TRUE see \code{\link{get_labels_from_binary}}
#' @family upset
#' @export
create_binary_groups = function(data, groups, trans=FALSE) {
  d = data.frame(count=data$count)
  for(i in seq_along(groups)) {
    name = names(groups[i])
    ss = groups[[i]]
    g = apply(data[, ss], 1, sum, na.rm=TRUE) > 0
    d[[name]] = g
  }
  nn = names(groups)
  mask = create_binary_mask(nn)
  d = apply_binary_mask(d, mask, 'g')
  d = d %>% group_by(g) %>% summarise(count=sum(count))
  d = get_labels_from_binary(d, mask, group="g", trans=trans)
  d
}

#' Create an upset graph, inspired from an UpsetR package, show associations
#'
#' re-implementation of the graph to separate association computation and vizualization
#'
#' @param data data.frame() with data for each sets
#' @param sets list of column names for each variable in sets
#' @param n.max number of top obs to keep, (all if na)
#' @param point_size size of point in intersection plot
#' @param name_size_scale scale of labels size in intersection plot
#' @param title title of the plot
#' @param subtitle subtitle of the plot
#' @param caption caption of the plot
#' @param opts options for freq plot
#'
#' @section Options:
#'
#' General options :
#' - theme: add the theme to the plot
#' - freq: frequency plot option
#' - matrix: matrix plot option
#'
#' @section Frequency plot options:
#' \describe{
#'   \itemize{bar.color}{}
#'   \itemize{bar.fill}{fill color}
#'   \itemize{label}{label of the axis}
#'   \itemize{prop}{list, Compute cumulative proportion options}
#' }
#'
#' @section Matrix plot options:
#' \describe{
#'   \itemize{point.color}{Color of point}
#'   \itemize{point.empty}{Color if set size is empty}
#'   \itemize{point.size}{Size of the point}
#'   \itemize{name.size}{Size of the name}
#'   \itemize{grid.color}{Color of the grid}
#'   \itemize{line.size}{Size of the line connecting association}
#'   \itemize{line.color}{color of the line connecting association}
#'   \itemize{set.labels}{vector of labels of each set name or function(lab) to get set label from set name}
#'   \itemize{order.freq}{reorder set list by frequency}
#'  }
#' @export
#' @family upset
#' @importFrom stats reorder
upset_plot <- function(data, sets, n.max=40, point_size=3, name_size_scale=1, title=NULL, subtitle=NULL, caption=NULL, opts=list()) {
  requireNamespace("cowplot")
  requireNamespace("swMisc")
  data = data %>% arrange(desc(count))
  data$label = reorder(data$label, order(data$count, decreasing=TRUE))

  left_margin = 1

  if(!is.null(opts$freq)) {
    freq_opts = opts$freq
  } else {
    freq_opts = list()
  }

  freq_opts = swMisc::merge_list(freq_opts, list(
    bar.color=NA,
    bar.fill="black",
    label="Frequency",
    prop = NULL # Compute cumulative proportion
  ))

  show_prop = !is.null(freq_opts[['prop']])

  if(show_prop) {
    prop_opts = swMisc::merge_list(freq_opts[['prop']], list(
      breaks=seq(.1, 1, by=.1),
      line.color="grey",
      label.size=2
    )
    )
  }

  if(show_prop) {
    # Compute prop on all combinations, before row selection
    data[['.up_prop']] = data$count / sum(data$count)
  }

  if(!is.na(n.max)) {
    d = data[1:n.max, ]
  } else {
    d = data
  }

  # freq_plot
  d$row = 1:nrow(d)

  if(show_prop) {
    d[['.up_prop_cum']] = cumsum(d[['.up_prop']])
    i = sapply(prop_opts$breaks, function(b) {
      min(which(d[['.up_prop_cum']] >= b))
    })
    # Flag where to show the line & the label
    d[['.up_prop_break']] = FALSE
    d[['.up_prop_break']][i] = TRUE
  }

  freq_plot = ggplot(d, aes(x=row, y=count))

  if(show_prop) {
    ymax = max(d$count)
    freq_plot = freq_plot +
      geom_segment(data=~filter(., .up_prop_break), aes(x=row, xend=row, y=0, yend=!!ymax), color=prop_opts$line.color) +
      geom_text(data=~filter(., .up_prop_break), aes(label=paste0(round(100 * .up_prop_cum),"%"), y=!!ymax), size=prop_opts$label.size)
  }

  freq_plot = freq_plot + geom_bar(stat="identity", color=freq_opts$bar.color, fill=freq_opts$bar.fill)

  freq_plot = freq_plot  +
    theme(
      panel.background = element_rect(fill = "white", color="white"),
      panel.border = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_blank(),
      plot.background = element_rect(fill="white", color="white"),
      plot.title = element_text(vjust = 1.5),
      plot.margin=unit(c(0.5, 0, 0, left_margin), "cm"),
      axis.title.y = element_text(vjust = 1.3, size = 8.3),
      axis.title.x = element_text(size = 8.3),
      axis.text.x = element_blank()
    ) +
    scale_x_continuous(limits = c(0,(nrow(d)+1 )), expand = c(0,0), breaks = NULL) +
    labs(x=NULL, y =freq_opts$label, title=title, subtitle = subtitle)

  if(!is.null(freq_opts$theme)) {
    freq_plot = freq_plot + freq_opts$theme
  }

  if(!is.null(opts$matrix)) {
    mat_opts = opts$matrix
  } else {
    mat_opts = list()
  }

  mat_opts = swMisc::merge_list(mat_opts, list(
    point.color ="grey20",
    point.empty = "gray70",
    point.size = 3,
    name.size = 1,
    grid.color="gray95",
    line.size = 1,
    line.color = "grey30",
    order.freq = TRUE
  ))


  # Get symptoms with any associations
  n = colSums(d[, sets], na.rm = TRUE)
  if(mat_opts$order.freq) {
    n = n[order(n)]
  }
  n = names(n[n > 0])
  name_size_scale = mat_opts$name.size
  point_empty = mat_opts$point.empty
  point_size = mat_opts$point.size
  point_color = mat_opts$point.color

  set_labels = n
  if(!is.null(mat_opts$set.labels)) {
    if(is.function(mat_opts$set.labels)) {
      set_labels = mat_opts$set.labels(set_labels)
    }
  }
  mat = as.matrix(d[, n])
  mm <- expand.grid(group=seq(nrow(mat)), symptom=seq(ncol(mat)))
  mm <- data.frame(mm, intersection = as.vector(mat))
  rr = data.frame(symptom=seq(ncol(mat)))
  rr$color = rr$symptom %% 2 == 0
  matrix_plot = ggplot(mm) +
    geom_rect(data=rr, aes(fill=color, ymin=symptom - .5, ymax=symptom + .5), xmin=0, xmax=nrow(mat) + .5, show.legend = FALSE) +
    geom_point(aes(x=group, y=symptom), color=point_empty, size=point_size, shape=16) +
    geom_point(aes(x=group, y=symptom), data=~filter(., intersection), size=point_size, shape=16, color=point_color) +
    geom_line(data=~filter(., intersection), aes(x=group, y=symptom, group=group), size = mat_opts$line.size, color=mat_opts$line.color) +
    scale_y_continuous(breaks = 1:length(n), limits = c(0.5, length(n) + 0.5), labels = set_labels, expand = c(0,0)) +
    scale_x_continuous(limits = c(0, nrow(d) + 1), expand = c(0, 0))  +
    scale_fill_manual(values=c('TRUE'="white",'FALSE'=mat_opts$grid.color)) +
    theme(
      panel.background = element_rect(fill = "white", color="white"),
      panel.border=element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      plot.margin=unit(c(0, 0.5, 0.5, 1), "cm"),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      axis.ticks.y = element_blank(),
      axis.text.y = element_text(colour = "gray0", size = 7 * name_size_scale, hjust = 0.4),
      plot.background = element_rect(fill="white", color = "white")
    ) +
    labs(x=NULL, y=mat_opts$label, caption=caption)

  if(!is.null(mat_opts$theme)) {
    matrix_plot = matrix_plot + mat_opts$theme
  }

  cowplot::plot_grid(freq_plot, matrix_plot, align="v", ncol=1)

}

