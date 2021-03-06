% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/path.R
\name{init.path}
\alias{init.path}
\title{Define an path for files, usable by my.path()}
\usage{
init.path(p, full.path = F)
}
\arguments{
\item{p}{chr path}

\item{full.path}{logical if TRUE p is considered as an absolute path, replace all the current path}
}
\description{
\code{init.path()} and \code{\link{my.path}()} are dedicated to manage path to files used by scripts to make them independant from the
actual physical location of the files, which depends on where (on which machine, account) the script is running.
}
\details{
by default, the path is added to the global output path (`base.out.path` option, see \code{\link{share.option}} ),
unless the parameter full.path is TRUE
}
\seealso{
Other path-functions: 
\code{\link{add_path_prefix}()},
\code{\link{create_path}()},
\code{\link{get_current_paths}()},
\code{\link{my.path}()},
\code{\link{platform_path}()},
\code{\link{print.paths_definition}()},
\code{\link{share.cache.path}()},
\code{\link{share.data.path}()},
\code{\link{share.lib.path}()},
\code{\link{share_path}()}
}
\concept{path-functions}
