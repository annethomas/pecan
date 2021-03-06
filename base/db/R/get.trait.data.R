#-------------------------------------------------------------------------------
# Copyright (c) 2012 University of Illinois, NCSA.
# All rights reserved. This program and the accompanying materials
# are made available under the terms of the
# University of Illinois/NCSA Open Source License
# which accompanies this distribution, and is available at
# http://opensource.ncsa.illinois.edu/license.html
#-------------------------------------------------------------------------------

##--------------------------------------------------------------------------------------------------#
##' Check two lists. Identical does not work since one can be loaded
##' from the database and the other from a CSV file.
##'
##' @name check.lists
##' @title Compares two lists
##' @param x first list
##' @param y second list
##' @param filename one of "species.csv" or "cultivars.csv"
##' @return true if two list are the same
##' @author Rob Kooper
##'
check.lists <- function(x, y, filename = "species.csv") {
  if (nrow(x) != nrow(y)) {
    return(FALSE)
  }
  if(filename == "species.csv"){
    cols <- c('id', 'genus', 'species', 'scientificname')
  } else if (filename == "cultivars.csv") {
    cols <- c('id', 'specie_id', 'species_name', 'cultivar_name')
  } else {
    return(FALSE)
  }
  xy_match <- vapply(cols, function(i) identical(as.character(x[[i]]), as.character(y[[i]])), logical(1))
  return(all(unlist(xy_match)))
}

##--------------------------------------------------------------------------------------------------#
##' Get trait data from the database for a single pft
##'
##' @name get.trait.data.pft
##' @title Gets trait data from the database
##' @details \code{pft} should be a list containing at least `name` and `outdir`, and optionally `posteriorid` and `constants`. BEWARE: All existing files in \code{outir} will be deleted!
##' @param pft list of settings for the pft whos traits to retrieve. See details
##' @param modeltype type of model that is used, this is used to distinguish between different pfts with the same name.
##' @param dbfiles location where previous results are found
##' @param dbcon database connection
##' @param forceupdate set this to true to force an update, auto will check to see if an update is needed.
##' @param trait.names list of trait names to retrieve
##' @return updated pft with posteriorid
##' @author David LeBauer, Shawn Serbin, Rob Kooper
##' @export
##'
get.trait.data.pft <- function(pft, modeltype, dbfiles, dbcon, trait.names,
                               forceupdate = FALSE) {

  # Create directory if necessary
  if (!file.exists(pft$outdir) && !dir.create(pft$outdir, recursive = TRUE)) {
    PEcAn.logger::logger.error(paste0("Couldn't create PFT output directory: ", pft$outdir))
  }

  ## Remove old files.  Clean up.
  old.files <- list.files(path = pft$outdir, full.names = TRUE, include.dirs = FALSE)
  file.remove(old.files)

  # find appropriate pft
  pftres <- (dplyr::tbl(dbcon, "pfts")
    %>% dplyr::filter(name == pft$name))
  if (!is.null(modeltype)) {
    pftres <- (pftres %>% dplyr::semi_join(
      (dplyr::tbl(dbcon, "modeltypes") %>% dplyr::filter(name == modeltype)),
      by = c("modeltype_id" = "id")))
  }
  pftres <- (pftres
    %>% dplyr::select(.data$id, .data$pft_type)
    %>% dplyr::collect())
  pfttype <- pftres[['pft_type']]
  pftid <- pftres[['id']]

  if(nrow(pftres) > 1){
    PEcAn.logger::logger.severe(
      "Multiple PFTs named", pft$name,  "found,",
      "with ids", PEcAn.utils::vecpaste(pftres$id), ".",
      "Specify modeltype to fix this.")
  }

  if (is.null(pftid)) {
    PEcAn.logger::logger.severe("Could not find pft", pft$name)
    return(NA)
  }

 # get the member species/cultivars, we need to check if anything changed
  if (pfttype == "plant") {
    pft_member_filename = "species.csv"
    pft_members <- PEcAn.DB::query.pft_species(pft$name, modeltype, dbcon)
  } else if (pfttype == "cultivar") {
    pft_member_filename = "cultivars.csv"
    pft_members <- PEcAn.DB::query.pft_cultivars(pft$name, modeltype, dbcon)
  } else {
    PEcAn.logger::logger.severe("Unknown pft type! Expected 'plant' or 'cultivar', got", pfttype)
  }

  # get the priors
  prior.distns <- PEcAn.DB::query.priors(pft = pftid, trstr = PEcAn.utils::vecpaste(trait.names), out = pft$outdir, con = dbcon)
  prior.distns <- prior.distns[which(!rownames(prior.distns) %in% names(pft$constants)),]
  traits <- rownames(prior.distns)

  # get the trait data (don't bother sampling derived traits until after update check)
  trait.data.check <- PEcAn.DB::query.traits(ids = pft_members$id, priors = traits, con = dbcon, update.check.only = TRUE, ids_are_cultivars = (pfttype=="cultivar"))
  traits <- names(trait.data.check)

  # Set forceupdate FALSE if it's a string (backwards compatible with 'AUTO' flag used in the past)
  if (!is.logical(forceupdate)) {
    forceupdate <- FALSE
  }

  # check to see if we need to update
  if (!forceupdate) {
    if (is.null(pft$posteriorid)) {
      pft$posteriorid <- db.query(
        query = paste0(
          "SELECT id FROM posteriors WHERE pft_id=", pftid,
          " ORDER BY created_at DESC LIMIT 1"
        ),
        con = dbcon
      )[['id']]
    }
    if (!is.null(pft$posteriorid)) {
      files <- dbfile.check(type = 'Posterior', container.id = pft$posteriorid, con = dbcon)
      ids <- match(c('trait.data.Rdata', 'prior.distns.Rdata', pft_member_filename), files$file_name)
      if (!any(is.na(ids))) {
        foundallfiles <- TRUE
        for(id in ids) {
          PEcAn.logger::logger.info(files$file_path[[id]], files$file_name[[id]])
          if (!file.exists(file.path(files$file_path[[id]], files$file_name[[id]]))) {
            foundallfiles <- FALSE
            PEcAn.logger::logger.error("can not find posterior file: ", file.path(files$file_path[[id]], files$file_name[[id]]))
          } else if (files$file_name[[id]] == pft_member_filename) {
            PEcAn.logger::logger.debug("Checking if pft membership has changed")
            testme <- utils::read.csv(file = file.path(files$file_path[[id]], files$file_name[[id]]))
            if (!check.lists(pft_members, testme, pft_member_filename)) {
              foundallfiles <- FALSE
              PEcAn.logger::logger.error("pft membership has changed: ", file.path(files$file_path[[id]], files$file_name[[id]]))
            }
            remove(testme)
          } else if (files$file_name[[id]] == "prior.distns.Rdata") {
            PEcAn.logger::logger.debug("Checking if priors have changed")
            prior.distns.tmp <- prior.distns
            if(file.exists(files$file_path[[id]], files$file_name[[id]])){
              load(file.path(files$file_path[[id]], files$file_name[[id]]))#HERE IS THE PROBLEM
            }else{
              PEcAn.logger::logger.debug("Prior file does not exist. If empty (zero-byte) input file error is recived, set forceupdate to TRUE for one run.")
            }
            testme <- prior.distns
            prior.distns <- prior.distns.tmp
            if (!identical(prior.distns, testme)) {
              foundallfiles <- FALSE
              PEcAn.logger::logger.error("priors have changed: ", file.path(files$file_path[[id]], files$file_name[[id]]))
            }
            remove(testme)
          } else if (files$file_name[[id]] == "trait.data.Rdata") {
            PEcAn.logger::logger.debug("Checking if trait data has changed")
            load(file.path(files$file_path[[id]], files$file_name[[id]]))

            # For trait data including converted data, only check unconverted
            converted.stats2na <- function(x) {
              if (all(c("mean", "stat", "mean_unconverted", "stat_unconverted") %in% names(x)))
                x[,c("mean","stat")] <- NA
              return(x)
            }
            trait.data <- lapply(trait.data, converted.stats2na)
            trait.data.check <- lapply(trait.data.check, converted.stats2na)

            if (!identical(trait.data.check, trait.data)) {
              foundallfiles <- FALSE
              PEcAn.logger::logger.error("trait data has changed: ", file.path(files$file_path[[id]], files$file_name[[id]]))
            }
            remove(trait.data, trait.data.check)
          }
        }
        if (foundallfiles) {
          PEcAn.logger::logger.info("Reusing existing files from posterior", pft$posteriorid, "for", pft$name)
          for (id in seq_len(nrow(files))) {
            file.copy(from = file.path(files[[id, 'file_path']], files[[id, 'file_name']]),
                      to = file.path(pft$outdir, files[[id, 'file_name']]))
          }

          # May need to symlink the generic post.distns.Rdata to a specific post.distns.*.Rdata file.
          if (length(dir(pft$outdir, "post.distns.Rdata")) == 0) {
            all.files <- dir(pft$outdir)
            post.distn.file <- all.files[grep("post.distns.*.Rdata", all.files)]
            if (length(post.distn.file) > 1)
              stop("get.trait.data.pft() doesn't know how to handle multiple post.distns.*.Rdata files")
            else if (length(post.distn.file) == 1) {
              # Found exactly one post.distns.*.Rdata file. Use it.
              file.symlink(from = file.path(pft$outdir, post.distn.file),
                           to = file.path(pft$outdir, 'post.distns.Rdata')
              )
            }
          }
          return(pft)
        }
      }
    }
  }

  # get the trait data (including sampling of derived traits, if any)
  trait.data <- query.traits(pft_members$id, traits, con = dbcon, update.check.only = FALSE, ids_are_cultivars=(pfttype=="cultivar"))
  traits <- names(trait.data)

  # get list of existing files so they get ignored saving
  old.files <- list.files(path = pft$outdir)

  # create a new posterior
  now <- format(x = Sys.time(), format = "%Y-%m-%d %H:%M:%S")
  db.query(query = paste0("INSERT INTO posteriors (pft_id, created_at, updated_at) VALUES (", pftid, ", '", now, "', '", now, "')"),
           con = dbcon)
  pft$posteriorid <- db.query(query = paste0("SELECT id FROM posteriors WHERE pft_id=", pftid, " AND created_at='", now, "'"),
                              con = dbcon)[['id']]

  # create path where to store files
  pathname <- file.path(dbfiles, "posterior", pft$posteriorid)
  dir.create(pathname, showWarnings = FALSE, recursive = TRUE)

  ## 1. get species/cultivar list based on pft
  utils::write.csv(pft_members, file.path(pft$outdir, pft_member_filename), row.names = FALSE)

  ## save priors
  save(prior.distns, file = file.path(pft$outdir, "prior.distns.Rdata"))
  utils::write.csv(prior.distns,
            file = file.path(pft$outdir, "prior.distns.csv"), row.names = TRUE)

  ## 3. display info to the console
  PEcAn.logger::logger.info('Summary of Prior distributions for: ', pft$name)
  PEcAn.logger::logger.info(colnames(prior.distns))
  apply(X = cbind(rownames(prior.distns), prior.distns), MARGIN = 1, FUN = PEcAn.logger::logger.info)

  ## traits = variables with prior distributions for this pft
  trait.data.file <- file.path(pft$outdir, "trait.data.Rdata")
  save(trait.data, file = trait.data.file)
  utils::write.csv(plyr::ldply(trait.data),
            file = file.path(pft$outdir, "trait.data.csv"), row.names = FALSE)

  PEcAn.logger::logger.info("number of observations per trait for", pft$name)
  for (t in names(trait.data)) {
    PEcAn.logger::logger.info(nrow(trait.data[[t]]), "observations of", t)
  }


  ### save and store in database all results except those that were there already
  for (file in list.files(path = pft$outdir)) {
    if (file %in% old.files) {
      next
    }
    filename <- file.path(pathname, file)
    file.copy(file.path(pft$outdir, file), filename)
    dbfile.insert(in.path = pathname, in.prefix = file, type = 'Posterior', id = pft$posteriorid, con = dbcon)
  }

  return(pft)
}

##--------------------------------------------------------------------------------------------------#
##' Get trait data from the database.
##'
##' This will use the following items from setings:
##' - settings$pfts
##' - settings$model$type
##' - settings$database$bety
##' - settings$database$dbfiles
##' - settings$meta.analysis$update
##' @name get.trait.data
##' @title Gets trait data from the database
##' @param pfts the list of pfts to get traits for
##' @param modeltype type of model that is used, this is is used to distinguis between different pfts with the same name.
##' @param dbfiles location where previous results are found
##' @param database database connection parameters
##' @param forceupdate set this to true to force an update, false to check to see if an update is needed.
##' @param trait.names list of traits to query. If TRUE, uses trait.dictionary
##' @return list of pfts with update posteriorids
##' @author David LeBauer, Shawn Serbin
##' @export
##'
get.trait.data <- function(pfts, modeltype, dbfiles, database, forceupdate, trait.names=NULL) {
  if (!is.list(pfts)) {
    PEcAn.logger::logger.severe('pfts must be a list')
  }
  # Check that all PFTs have associated outdir entries
  pft_outdirs <- lapply(pfts, '[[', 'outdir')
  if (any(sapply(pft_outdirs, is.null))) {
    PEcAn.logger::logger.severe('At least one pft in settings is missing its "outdir"')
  }
  ##---------------- Load trait dictionary --------------#
  if (is.logical(trait.names)) {
    if (trait.names) {
      trait.names <- as.character(PEcAn.utils::trait.dictionary$id)
    }
  }

  # process all pfts
  dbcon <- db.open(database)
  on.exit(db.close(dbcon))
  result <- lapply(pfts, get.trait.data.pft,
                   modeltype = modeltype,
                   dbfiles = dbfiles,
                   dbcon = dbcon,
                   forceupdate = forceupdate,
                   trait.names = trait.names)

  invisible(result)
}

####################################################################################################
### EOF.  End of R script file.
####################################################################################################
