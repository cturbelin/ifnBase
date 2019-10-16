##
# Graphical related functions
##

#' Init ggplot2 base theme.
#'
#' Recall it with another theme
#'
#' @param theme theme() value to use as default theme
#' @export
#' @importFrom ggplot2 element_line theme element_rect element_text theme_set
with_ggplot = function(theme=NULL) {
  require(ggplot2)

  if( is.null(theme) ) {
    theme = theme_minimal(base_size=8)
  }

  color="grey70"

  theme = theme + theme(
    line = element_line(size=.1),
    rect = element_rect(size=.1),
    panel.border=element_rect(linetype="solid", color=color, fill="transparent"),
    axis.ticks = element_line(color=color),
    axis.ticks.x = element_line(),
    axis.ticks.y = element_line(),
    axis.ticks.length = unit(3,"pt"),
    plot.title=element_text(hjust=.5, size=rel(1)),
    axis.text = element_text(size=rel(.9)),
    axis.title = element_text(size=rel(.9))
  )

  if( !is.null(theme$plot.caption) ) {
    theme = theme + theme(
      plot.caption=element_text(size=rel(0.7), hjust=0.5),
      plot.subtitle=element_text(size=rel(0.8), hjust=0.5)
    )
  }

  theme_set(theme)
}

#' Get default color
#'
#' Default color for a given platform can be configured in platform environment, a vector of at least 2 colors are expected
#'
#' @param which index of default color (1 or 2)
get_default_color = function(which=1) {
  colors = platform_env("colors")
  if(is.null(colors)) {
    colors = c("#007AB8", "#7AB800")
  }
  return(colors[which])
}

#' Percentage barplot
#' @param values vector
#' @param order if "desc" use decreasing order
#' @param label.size size of the labels (can use relative size using ggplot2::rel())
#' @param axis.label if TRUE show axis label
#' @param color fill color default will use color
#' @param scale_color use this scale for fill colors, TRUE will use brewer palette
#' @param x.rotate rotation angle for labels
#' @param x.vjust vertical justification for x axis
#' @param label.nudge nudge for y-axis labels
#' @importFrom ggplot2 rel aes ggplot geom_bar scale_fill_brewer geom_text
#' @export
gg_barplot_percent = function(values, order="desc", label.size=rel(.9), axis.label=F, color=NULL, scale_color=NULL, x.rotate=90, x.vjust=NULL, label.nudge=.3)  {

  if(is.null(color)) {
    color = get_default_color(1)
  }

  if ("mfreq" %in% class(values)) {
    tt = values["count", ]
    i = !is.na(tt)
    tt = tt[i]
    tt.prop = values["prop", ] / 100
    tt.prop = tt.prop[i]
  } else {
    tt = table(values)
    if(order %in% c("desc","asc")) {
      decreasing = order == "desc"
      tt = tt[order(tt, decreasing = decreasing)]
    }
    tt.prop = prop.table(tt)
  }
  ll = names(tt)
  ll = factor(ll, levels = ll, ordered = TRUE)
  data = data.frame(label=ll, value=as.vector(tt), prop=round(as.vector(tt.prop) * 100, 1))
  g = ggplot(data, aes(x=label, y=prop))
  if(!is.null(color)) {
    g = g + geom_bar(stat="identity", fill=color)
  } else {
    g = g + geom_bar(stat="identity")
  }
  if( !is.null(scale_color) ) {
    if( isTRUE(scale_color) ) {
      g  = g + aes(fill=label) + scale_fill_brewer(palette="Dark2")
    } else {
      g = g + scale_color
    }
  }
  g = g + geom_text(nudge_y = label.nudge, aes(label=paste0(value, " (", prop,"%)")), size=label.size, vjust="bottom")
  if( !is.na(x.rotate) ) {
    g = g + theme(axis.text.x=element_text(angle=x.rotate, vjust = x.vjust))
  }
  g
}

#' Plot Age Pyramid
#' @param data data.frame with (age.cat, gender, pop, prop)
#' @param female bool female rows
#' @param prop bool, if TRUE proportion are [0;1], if FALSE are percent [0;100]
#' @param w width factor
#' @param scales list of named char vector for "pop" & "cohort" names
#' @export
plot_age_pyramid = function(data, female, prop=T, w=.5, scales=list()) {

  labels = if(is.null(scales$label) ) c(pop='pop', cohort='cohort') else scales$label
  colors = if(is.null(scales$color) ) c(pop='darkgrey', cohort='blue') else scales$color
  alphas = if(is.null(scales$alpha) ) c(pop=.5, cohort=1) else scales$alpha

  if(prop) {
    y_scale = seq(-1, 1, .1)
    y_factor = 100
  } else {
    y_scale = seq(-100, 100, 10)
    y_factor = 1
  }
  y_labels = paste(round(y_factor * abs(y_scale)), "%")

  ymax = ceiling(max(abs(data$prop)))

  xmax = as.numeric(max(data$age.cat))

  data$prop[female] = -data$prop[female]
  data$w = ifelse(data$pop == "pop", w * .8, .8)
  ggplot(data, ggplot2::aes(x = age.cat, group=pop, fill=pop, y=prop, width=w, alpha=pop)) +
    ggplot2::geom_bar(stat = "identity") +
    ggplot2::geom_hline(aes(yintercept = 0), color="darkgrey") +
    ggplot2::scale_fill_manual(values=colors, labels=labels) +
    ggplot2::scale_alpha_manual(values=alphas, labels=labels) +
    ggplot2::geom_text(y=-ymax, x=xmax, label=i18n("female"), hjust="left", show.legend = FALSE) +
    ggplot2::geom_text(y=ymax, x=xmax, label=i18n("male"), hjust="right", show.legend = FALSE) +
    ggplot2::scale_y_continuous(breaks = y_scale, labels=y_labels, limits=c(-ymax, ymax)) +
    ggplot2::coord_flip()

}

#' Scale from wid to Yearweek values
#'
#' Yearweek values are generated by \code{ISOYearWeek()} function
#'
#' @param ww data.frame() associating wid to yearweek
#' @param week.sep separator for yearweek week label
#' @param breaks start|auto|all|by|year|week breaks strategy or a function(limits) returing breaks
#' @param col.yw yearweek values column in [ww]
#' @param label labelling strategy (yw or "year" for year label), or function(yw)
#' @param by number of week step to use if breaks="by"
#' @param week number to use if breaks=week
#' @param complete force the ww data.frame to be complete (recompute wid) from yearweek range
#' @param n.pretty number ticks to generate (breaks=auto)
#' @param n.pretty number of pretty values to use
#' @details
#' start, year : tick is first week of the year
#' @export
scale_x_wid = function(ww, week.sep="s", breaks="auto", col.yw="yw", label="yw", by=NULL, week=NULL, complete=FALSE, n.pretty=5) {

  if(complete) {
    ww = range(ww[, col.yw])
    ww = WeekStart(ww)
    ww = ISOYearWeek(seq(ww[1], ww[2], by=7))
    ww = data.frame(yw=ww, wid=WeekStamp(ww))
    col.yw = "yw"
  }

  if(is.function(label)) {
    label_formatter = label
  } else {
    label_formatter = switch(
      label,
      "yw" = function(yw) {
        format_week(yw, sep = week.sep)
      },
      "year" = function(yw) {
        floor(yw / 100)
      },
      stop(paste0("Unknown label formatter '", label, "'"))
    )
  }

  label_wid = function(wid) {
    yw = ww[match(wid, ww$wid), col.yw]
    ifelse(is.na(yw), "", label_formatter(yw))
  }


  # found yw with week number = [week]
  breaks_week = function(limits, week) {
    yw = ww[[col.yw]]
    w = ww$wid[yw %% 100 %in% week]
    w = w[ w >= limits[1] & w <= limits[2]]
    unique(w)
  }

  breaks_startyear = function(limits) {
    b = breaks_week(limits, week=1)
    if( !is.null(by) ) {
      b = b[seq(1, length(b), by=by)]
    }
    unique(b)
  }


  breaks_checker = function(b) {
    b[ b %in% ww$wid ]
  }

  extend_limits = function(limits) {
    c(floor(limits[1]), ceiling(limits[2]))
  }

  breaks.fun = ggplot2::waiver()

  if( is.character(breaks) ) {
    if(!breaks %in% c("auto","start", 'all', 'by','year','week')) {
      stop(paste0("Unknown breaks strategy '",breaks,"', known : auto, start"))
    }

    if(breaks == "auto") {
      breaks.fun <- function(limits) {
        limits = extend_limits(limits)
        d = diff(limits)
        if(d < 104) {
          if(d < 15) {
            b =  seq(limits[1], limits[2], by=1)
          } else {
            b = scales::extended_breaks(n.pretty)
            b = unique(round(b(limits)))
          }
        } else {
          b = breaks_startyear(limits)
        }
        breaks_checker(b)
      }
    }
    if(breaks %in% c('year', "start")) {
      breaks.fun = breaks_startyear
    }
    if(breaks == "by") {
      if( is.null(by) ) {
        stop("parameter by is needed if breaks='by'")
      }
      breaks.fun = function(limits) {
        limits = extend_limits(limits)
        breaks_checker(seq(limits[1], limits[2], by=by))
      }
    }
    if(breaks == "week") {
      if( is.null(week) ) {
        stop("parameter by is needed if breaks='week'")
      }
      breaks.fun = function(limits) {
        breaks_week(limits, week) # use the week in definition env, so here in param of scale_x_wid
      }
    }

    if(breaks == "all") {
      breaks.fun = function(limits) {
        breaks_checker(seq(limits[1], limits[2], by=1))
      }
    }
  }
  if( is.function(breaks) ) {
    breaks.fun = function(limits) {
      limits = extend_limits(limits)
      breaks_checker(breaks(limits))
    }
  }
  ggplot2::scale_x_continuous(labels=label_wid, breaks=breaks.fun)

}
