.onLoad <- function(libname, pkgname) {

  defaults = list(
    graph = list(type="png", height=400, width=300),
    autoload.platform = FALSE,
    autoconnect = TRUE
  )

  env = .GlobalEnv

  # Historical framework used variables defined in Globalenv
  # Import those variables variables from the GlobalEnv to create options entry
  imports = list(
    "db_dsn"="DB_DSN",
    "db_driver"="DB_DRIVER",
    base.out.path = "OUT_PATH"
  )

  # Already defined options
  oo = get_option()

  for(i in seq_along(imports)) {
    name = names(imports[i])
    old.name = imports[[i]]
    if( isTRUE(old.name) ) {
      old.name = name
    }
    if(is.null(oo[[name]]) && exists(old.name, envir=env)) {
      packageStartupMessage(paste("Importing ", old.name," from global env"))
      oo[[name]] <- get(old.name, envir = env)
    }
  }

  # BASE_PATH
  # In case of base path defined, it is assumed the workspace is organized
  # with a share/ directory holding
  # See vignettes workspace
  if(exists("BASE_PATH", envir = env)) {
    # If BASE_PATH exists, recreate workspace paths, as expected
    packageStartupMessage("Importing BASE_PATH from global environment")
    base.path = ensureEndingSlash(get("BASE_PATH", envir = env))
    oo$share.path = paste0(base.path, "share/")
    oo$share.lib.path = paste0(oo$share.path, "lib/")
    oo$platform.path = paste0(oo$share.path, "platform/")
    oo$share.cache = paste0(oo$share.path, "cache/")
    oo$share.data = paste0(oo$share.path, "data/")
  }

  oo = merge.list(oo, defaults)

  do.call(share.option, oo)

  # Old libraries are now loaded by default, do not reload them
  # @todo allow to redefine them, so remove old share.lib import first
  .Share$loaded.libs = c()

  if( isTRUE(oo$autoload.platform) ) {
     load_platform()
  }

  if( isTRUE(oo$autoconnect) ) {
    dbConnect()
  }

  invisible()
}