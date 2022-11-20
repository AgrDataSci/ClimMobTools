#' @rdname getDataCM
#' @param x an object of class \code{CM_list}
#' @param tidynames logical, \code{TRUE} make clean column names
#' @param pivot.wider logical, if \code{TRUE} return a wider object 
#'  where each tricot package is a row
#' @param ... additional arguments passed to methods
#' @importFrom methods as
#' @method as.data.frame CM_list
#' @export
as.data.frame.CM_list <- function(x, 
                                  ...,
                                  tidynames = TRUE,
                                  pivot.wider = FALSE) {
  
  dat <- list()
  # 'specialfields', the assessment questions
  dat[["specialfields"]] <- x[["specialfields"]]
  
  # 'project', the project details
  dat[["project"]] <- x[["project"]]
  
  # 'registry', the questions during participant registration
  dat[["registry"]] <- x[["registry"]]
  
  # importantfields
  dat[["importantfields"]] <- x[["importantfields"]]
  
  # 'assessments', the survey in trial data assessment
  dat[["assessments"]] <- x[["assessments"]]
  
  # 'packages', the packages info
  dat[["packages"]] <- x[["packages"]]
  
  # 'data', the trial data assessment
  dat[["data"]] <- x[["data"]]
  
  # get the project name
  project_name <- dat[["project"]]$project_cod
  
  # get variables names from participant registration
  regs <- dat[["registry"]]
  
  regs <- regs[["fields"]]
  
  regs_name <- paste0("REG_", regs[, "name"])
  
  # get some info on the trial
  trial_tech <- dat$packages$comps[[1]]$technologies[[1]]$tech_name
  trial_pi <- dat$project$project_pi
  trial_country <- dat$project$project_cnty
  
  
  has_data <- length(dat[["data"]]) > 0
  
  if (isTRUE(has_data)) {
    
    ncomp <- dat$project$project_numcom
    
    # get the names of assessments questions
    assess_q <- dat[["specialfields"]]
    
    # check if overall VS local is present
    tricotvslocal <- grepl("Performance", assess_q$type)
    
    # get the strings of ranking questions
    rank_q <- assess_q$name[!tricotvslocal]
    
    # and the overall vs local question
    tricotvslocal <- assess_q$name[tricotvslocal]
    
    # get variables names from assessments
    assess <- dat[["assessments"]]
    
    if (length(assess) > 0) {
      
      assess <- do.call("rbind", assess[, "fields"])
      assess <- data.frame(assess, stringsAsFactors = FALSE)
      assess <- assess[!duplicated(assess[, "name"]), ]
      # the ids for assessments
      assess_id <- paste0("ASS", dat[["assessments"]][, "code"])
      # and the description
      assess_name <- dat[["assessments"]][, "desc"]
      # the names of questions
      looknames <- assess[, "name"]
      # the strings to use to decode values
      rtable <- do.call("rbind", dat$assessments$fields)
      rtable <- rtable[!is.na(rtable$rtable), c("name", "rtable")]
      
      # get codes from multiple choice variables
      assess_lkp <- dat$assessments$lkptables
      lkp <- list()
      for(i in seq_along(assess_lkp)){
        l <- .decode_lkptable(assess_lkp[[i]])
        lkp <- c(lkp, l)
      }
      
      
    } else {
      assess <- data.frame()
      assess_id <- character()
      assess_name <- character()
      looknames <- character()
      rtable <- data.frame()
      lkp <- list()
    }
    
    # add codes from registration
    reg_lkp <- .decode_lkptable(dat$registry$lkptables)
    lkp <- c(reg_lkp, lkp)
    
    # also the table strings
    reg_rtable <- dat$registry$fields[,c("name","rtable")]
    reg_rtable <- reg_rtable[!is.na(reg_rtable$rtable), ]
    rtable <- rbind(rtable, reg_rtable)
    rtable$rtable <- gsub("_lkp","_",rtable$rtable)
    
    # remove tricot questions
    rtable <- rtable[!grepl("char_", rtable$name), ]
    # remove lkp table with farmers names
    rtable <- rtable[!grepl("qst163", rtable$name), ]
    
    # add assessment code
    assesscode <- lapply(strsplit(rtable$rtable, "_"), function(x) x[1])
    rtable$assess <- do.call("rbind", assesscode)
    rtable$name <- paste(rtable$assess, rtable$name, sep = "_")
    
    # trial data
    trial <- dat[["data"]]
    
    # replace codes by labels in multi choice questions 
    n_rtable <- nrow(rtable)
    
    for (i in seq_len(n_rtable)) {
      
      string_i <- rtable[i, "rtable"]
      var_i <- rtable[i, "name"]
      l <- match(string_i, names(lkp))
      l <- lkp[[l]]
      
      trial[var_i] <- gsub(" ", "; ", trial[[var_i]])
      
      for (j in seq_along(l$id)) {
        
        trial[var_i] <- gsub(l$id[j], l$label[j], trial[[var_i]])
        
      }
      
      
      
    }
    
    # split geolocation info
    # check if geographic location is available
    geoTRUE <- grepl("farmgoelocation|geopoint|gps|geotrial|pointofdel", names(trial))
    
    # if is available, then split the vector as lon lat
    if(any(geoTRUE)){
      
      geo_which <- which(geoTRUE)
      
      geo <- trial[geo_which]
      
      trial <- trial[!geoTRUE]
      
      for (i in seq_along(geo_which)){
        newname <- names(geo)[[i]]
        newname <- gsub("farmgoelocation", "_farm_geo", newname)
        newname <- gsub("__", "_", newname)
        newname <- paste0(newname, c("_longitude","_latitude", "_elevation","_gps_precision"))
        
        lonlat <- geo[i]
        
        lonlat[is.na(lonlat)] <- c("NA NA NA NA")
        lonlat[lonlat == "None"] <- c("NA NA NA NA")
        lonlat[lonlat == ""] <- c("NA NA NA NA")
        
        lonlat <- t(apply(lonlat, 1, function(xx) {
          
          strsplit(xx, " ")[[1]][1:4]
          
        }))
        
        lonlat[lonlat == "NA"] <- NA
        
        lonlat <- lonlat[, c(2,1,3,4)]
        
        lonlat <- as.data.frame(lonlat, stringsAsFactors = FALSE)
        
        names(lonlat) <- newname
        
        lonlat <- apply(lonlat, 2, as.numeric)
        
        trial <- cbind(trial, lonlat)
        
      }
    }
    
    # replace numbers in trial results by LETTERS
    if (ncomp == 3) {
      trial[rank_q] <-
        lapply(trial[rank_q], function(x) {
          y <- as.integer(x)
          y <- ifelse(y == 99, "Not observed", y)
          y <- ifelse(y == 98, "Tie", y)
          y <- ifelse(y == 1,  "A", y)
          y <- ifelse(y == 2,  "B", y)
          y <- ifelse(y == 3,  "C", y)
          y
        })
    }
    
    # replace numbers in question about overall vs local 
    if (length(tricotvslocal) > 1) {
      
      if (all(c(1, 2) %in% unlist(trial[tricotvslocal]))) {
        
        trial[tricotvslocal] <-
          lapply(trial[tricotvslocal], function(x) {
            y <- factor(x, levels = c("1", "2"), labels = c("Better", "Worse"))
            as.character(y)
          })
        
      }
      
    }
    
    # reshape it into a long format 
    # put pack id as first colunm
    packid <- grepl("REG_qst162", names(trial))
    
    trial <- cbind(trial[packid], trial[!packid])
    
    trial <- .set_long(trial, "REG_qst162")
    
    trial$moment <- "registration"
    
    # remove possible space in assess name
    assess_name <- gsub(" ", "", assess_name)
    
    # add which moment the data was taken
    for (i in seq_along(assess_id)) {
      trial$moment <- ifelse(grepl(assess_id[i], trial$variable),
                             tolower(assess_name[i]),
                             trial$moment)
      
    }
    
  } else {
    trial <- data.frame()
    assess_id <- 1
    assess_name <- character()
  }
  
  # comparisons and package
  comps <- dat[["packages"]][, "comps"]
  
  if (ncomp == 3) {
    comps <- lapply(comps, function(x) {
      x <- unique(unlist(x$technologies))
      x <- x[-1]
      names(x) <- paste0("item_", LETTERS[1:length(x)])
      x
    })
  }
  
  if (ncomp > 3) {
    comps <- lapply(comps, function(x) {
      x <- unique(unlist(x$technologies))
      x <- x[-1]
      names(x) <- paste0("item_", 1:length(x))
      x
    })
  }
  
  comps <- do.call("rbind", comps)
  
  comps <- as.data.frame(comps, stringsAsFactors = FALSE)
  
  pack <- cbind(dat[["packages"]][, c("package_id","farmername")], comps)
  
  # add project name
  pack$project_name <- project_name
  pack$technology <- trial_tech
  pack$coordinator <- trial_pi
  pack$country <- trial_country
  
  pack <- .set_long(pack, "package_id")
  
  pack[, "moment"] <- "package"
  
  trial <- rbind(pack, trial)
  
  # check if ids from ODK names are required to be removed 
  if (isTRUE(tidynames)) {
    
    trial[, "variable"] <- gsub("REG_", "", trial[, "variable"])
    
    for (i in seq_along(assess_id)) {
      trial[, "variable"] <- gsub(paste0(assess_id[i], "_"), "", 
                                  trial[, "variable"])
    }
    
    ovl <- which(grepl("perf_overallchar", trial[, "variable"]))
    
    trial[ovl, "variable"] <- sapply(trial[ovl, "variable"], function(x) {
      x <- strsplit(x, split = "_")
      x <- paste0("item_", LETTERS[as.integer(x[[1]][3])], "_vs_local")
      x
    })
    
    trial[, "variable"] <- gsub("char_", "", trial[, "variable"])
    
    trial[, "variable"] <- gsub("stmt_", "pos", trial[, "variable"])
    
    trial[, "variable"] <- gsub("clm_", "survey_", trial[, "variable"])
    
    trial[, "variable"] <- gsub("^_","", trial[, "variable"])
    
    i <- trial[, "variable"] == "farmername"
    
    trial[i, "variable"] <- "participant_name"
    
  }
  
  output <- trial[, c("id","moment","variable","value")]
  
  # remove some ODK variables
  output <- output[!grepl("originid|rowuuid|qst163", output[[3]]), ]
  
  # reorder rows and make sure that packages and registration comes first
  assess_name <- sort(tolower(assess_name))
  output$moment <- factor(output$moment, levels = c("package",
                                                    "registration",
                                                    rev(assess_name)))
  
  
  # reorder moment and ids
  o <- order(output$moment)
  output <- output[o, ]
  
  output$id <- as.integer(output$id)
  o <- order(output$id)
  output <- output[o, ]
  
  # if required, put the data in wide format
  if (isTRUE(pivot.wider)) {
    
    output$variable <- paste(output$moment, output$variable, sep = "_")
    
    variable_levels <- unique(output$variable)
    
    output <- output[, -2]
    
    output <- .set_wide(output, "id")
    
    output <- output[,c("id", variable_levels)]
    
    names(output) <- gsub("overallpos", "overall_pos", names(output))
    
    names(output) <- gsub("overallneg", "overall_neg", names(output))
    
  }
  
  row.names(output) <- seq_along(output$id)
  
  class(output) <- union("CM_df", class(output))
  
  return(output)
  
}
