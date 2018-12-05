# Graph Library prototype
#
# graph.open(file, width, height, pitch)
# ...
# graph.close()
#
# Params
# file : name of the graphic file (without extension, it can change with the config)

#' End a graphic export to a file. Use it instead of dev.off().
#' this function also handle post export hooks
#' @export
graph.close <- function() {
 dev.off()
 .graph_run_hooks()
}

#' get last output graph
#' @export
graph_get_last <- function() {
  .Share$graph.last
}


#' Export a ggplot2 and run graph hooks. To be used with ggplot2 graph instead of graph.close()
#' @param filename filename of the graph
#' @param type type of device to use
#' @param ... others arguments passed to ggsave()
#' @export
#' @importFrom tools file_ext
graph.save <- function(filename, type=NULL, ...) {
    device = NULL
    if( !is.null(type) ) {
        fn = paste0(filename, '.', type)
    } else {
        fn = filename
        ext = tools::file_ext(filename)
        if(ext == "") {
            device = "png"
            fn = paste0(filename, ".", device)
        }
    }
    for(f in fn) {
        ggplot2::ggsave(filename=f, device=device, ...)
    }
    .Share$graph.last <- tail(fn, n=1)
    .graph_run_hooks()
}


#' Declare a hook function called when graph.close() is called
#' @param fn function to run as hook
#' @param name unique name for the hook to prevent multiple registration
#' @export
graph.hook <- function (fn, name=NULL) {
  if ( is.null(.Share$graph.hook) ) {
    hooks = list()
  } else {
    hooks = .Share$graph.hook
  }
  do.register = TRUE
  if( !is.null(name) ) {
      registred = .Share$graph.hook.registred
      if ( is.null(registred) ) {
          registred = c()
      }
      if(name %in% registred) {
          do.register = FALSE
      }
      registred = c(registred, name)
      .Share$graph.hook.registred <- registred
  }
  if(do.register) {
    hooks[ length(hooks) + 1 ] = as.call(list(fn))
    .Share$graph.hook <- hooks
  }
}

#' @noRd
.graph_run_hooks <- function() {
    hooks = .Share$graph.hook
    if( is.null(hooks) || length(hooks) == 0) {
        return()
    }
    for(h in hooks) {
        h()
    }
    invisible()
}

#' Open a graph using standard library
#' @param file filename (without extension)
#' @param width width (pixel)
#' @param height height in px
#' @param pitch pointsize value
#' @param type type of output to use
#' @param ... extra parameted
#' @export
graph.open <- function(file, width=NA, height=NA, pitch=12, type="png",...) {
  f = paste0(file, ".", type)
  graph = get_option("graph")
  if( is.na(width) ) {
    width = graph$width
  }
  if( is.na(height) ) {
    height = graph$height
  }
  switch(type,
    png = png(f, width=width, height=height, pointsize=pitch,...),
    ps = postscript(f, width=width, height=height, pointsize=pitch, ...)
  )
  .Share$graph.last <- f
}

#' Return a list of n colors by the "best" color generator available
#'
#' Try to use RColorBrewer if available, if not uses rainbow
#'
#' @param n number of colors to get (caution, it can be fewer than expected for some palettes)
#' @param pal name of the palette to use
#' @export
graph_colors = function(n, pal=NULL) {
  if( is.null(.Share$graph.brewer) ) {
    .Share$graph.brewer = requireNamespace("RColorBrewer", logical.return=T)
  }
  if(.Share$graph.brewer || isTRUE(pal == "rainbow")) {
    if( is.null(pal) ) {
      if(n < 3) {
        pal = "Set1"
      } else {
        pal = ifelse(n > 9, "Set3","Set1")
      }
    }
    cc = RColorBrewer::brewer.pal(n, pal)
    if(length(cc) > n) {
      cc = cc[1:n]
    }
    return(cc)
  } else {
    rainbow(n)
  }
}


#' Draw confidence interval with polygon
#' @param ii data.frame
#' @param col.x name of x column
#' @param col.up name of upper value column
#' @param col.low name of lower value column
#' @param col color
#' @param ... extra parameters passed to polygon
#' @export
polygon_ic = function(ii, col.x, col.up, col.low, col, ...) {
   x = c(ii[, col.x] , rev(ii[, col.x]))
   y = c(ii[, col.up], rev(ii[, col.low]) )
   polygon(x, y, col=col, border=col, ...)
}


#' Axis for yearweek data
#'
#' Create an axis from weekly data (when the plot use week index, and you want to label axe with classical week nubmers)
#'
#' @param side side of the axeis
#' @param ww data.frame(), with cols wid=week index, [col.yw]=yearweek value (@see makeWeekIndex)
#' @param mode "ticks" follow ticks, "year" each 1st week of each year, "week" (or weeks) some given weeks, "\%\%" root of modulo
#' @param format "yw" pretty week format, "w"=only week number, NULL=disable
#' @param col.yw name of the yearweek value colum in ww
#' @param sep separator between year and week number
#' @param century use century for year number
#' @param ticks if mode=weeks vector of week (1-53) number, if mode="\%\%" modulo to use
#' @param ... extra parameters passed to axis
#' @export
#' @importFrom graphics axTicks axis
axis_week <- function(side, ww, mode=c("ticks","year",'week','weeks','%%'), format="yw", col.yw="yw", sep='s', century=T, ticks=NULL, ...) {
  mode = match.arg(mode)
  if(mode == "ticks") {
    wid = axTicks(side)
    wid = wid[ wid %in% ww$wid ] # Only available weeks
  }
  if(mode %in% c('week','weeks','%%','year')) {
      w = ww[, col.yw] %% 100
  }
  if(mode == "year") {
      wid = ww$wid[ w == 1 ]
  }
  if( mode == "week" || mode == "weeks") {
      if( is.null(ticks) ) {
          stop("parameter ticks should be provided with mode=weeks")
      }
      wid = ww$wid[w %in% ticks]
  }
  if( mode == "%%") {
      if( is.null(ticks) ) {
          stop("parameter ticks should be provided with mode=%%")
      }
      if(length(ticks) > 1) {
        stop("parameters ticks should have length 1 when mode=%%")
      }
      wid = ww$wid[(w %% ticks) == 0]
  }
  yw = ww[match(wid, ww$wid), col.yw]
  # Format the week
  if( !is.null(format) ) {
    if(format == "yw") {
      yw = format_week(yw, sep=sep, century=century)
    }
    if(format == "w") {
      yw = yw %% 100
    }
  }
  axis(side, at=wid, yw, ...)
  invisible(list(w=wid, yw=yw))
}


