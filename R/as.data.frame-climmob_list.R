#' Decode lkptables
#' @param x a list 
#' @return \code{x} as a data.frame 
#' @noRd
.decode_lkptable = function(x){
  name = x[["name"]]
  name = gsub("lkp", "", name)
  desc = x[["desc"]]
  desc = gsub("Lookup table ", "", desc)
  desc = gsub("[(]", "", desc)
  desc = gsub("[)]", "", desc)
  fields = x[["fields"]]
  values = x[["values"]]
  
  result = list()
  
  for(i in seq_along(name)){
    
    r = cbind(name = name[[i]],
              desc = desc[[i]],
              values[[i]])
    
    names(r) = c("name", "desc", "id", "label")
    
    r$label = .title_case(r$label)
    
    result[[name[[i]]]] = r
    
  }
  
  return(result)
  
}

#' Replace codes by factors
#' @param trial the trial data to apply the replacement 
#' @param x the climmob raw list file to identify the replacement cases
#' @return \code{trial} the input data.frame with the cases replaced 
#' @noRd
.replace_multichoice_codes = function(trial, x) {
  # decode lookup tables
  lkp = c(.decode_lkptable(x$registry$lkptables),
          unlist(lapply(x$assessments$lkptables, .decode_lkptable), recursive = FALSE))
  
  rtable = do.call("rbind", x$assessments$fields)
  rtable = rtable[!is.na(rtable$rtable), c("name", "rtable")]
  
  reg_rtable = x$registry$fields[, c("name", "rtable")]
  reg_rtable = reg_rtable[!is.na(reg_rtable$rtable), ]
  
  rtable = rbind(rtable, reg_rtable)
  rtable$rtable = gsub("_lkp", "_", rtable$rtable)
  rtable = rtable[!grepl("char_|qst163", rtable$name), ]
  
  assesscode = vapply(strsplit(rtable$rtable, "_"), `[`, 1, FUN.VALUE = character(1))
  rtable$assess = assesscode
  rtable$name = paste(rtable$assess, rtable$name, sep = "_")
  
  for (i in seq_len(nrow(rtable))) {
    string_i = rtable[i, "rtable"]
    var_i = rtable[i, "name"]
    if (!var_i %in% names(trial)) next
    l = lkp[[string_i]]
    if (is.null(l)) next
    trial[[var_i]] = gsub(" ", "; ", trial[[var_i]])
    for (j in seq_along(l$id)) {
      trial[[var_i]] = gsub(l$id[j], l$label[j], trial[[var_i]])
    }
  }
  return(trial)
}

#' Replace codes by factors
#' @param trial the trial data to apply the replacement 
#' @return \code{trial} the input data.frame with the new longlat columns 
#' @noRd
.handle_geolocation_columns = function(trial) {
  
  geoTRUE = grepl("farmgoelocation|geopoint|gps|geotrial|pointofdel", names(trial))
  
  if (!any(geoTRUE)) return(trial)
  
  geo = trial[geoTRUE]
  trial = trial[!geoTRUE]
  keep = unlist(lapply(geo, function(x) !all(is.na(x))))
  geo = geo[, keep, drop = FALSE]
  
  if (ncol(geo) == 0) return(trial)
  
  for (i in seq_len(ncol(geo))) {
    newname = gsub("farmgoelocation", "_farm_geo", names(geo)[[i]])
    newname = gsub("__", "_", newname)
    newname = paste0(newname, c("_longitude", "_latitude", "_elevation", "_gps_precision"))
    
    lonlat = geo[[i]]
    lonlat[is.na(lonlat) | lonlat == "None" | lonlat == ""] = "NA NA NA NA"
    lonlat = t(apply(lonlat, 1, function(xx) {
      vals = strsplit(x, " ")[[1]][1:4]
      vals[is.na(vals)] = NA
      vals[c(2,1,3,4)]
    }))
    
    lonlat = as.data.frame(lonlat, stringsAsFactors = FALSE)
    
    names(lonlat) = newname
    
    lonlat = apply(lonlat, 2, as.numeric)
    
    trial = cbind(trial, lonlat)
    
  }
  
  longlat = grep("_longitude|_latitude|_elevation|_precision", names(trial))
  rmv = unlist(lapply(trial[longlat], function(x) all(is.na(x))))
  longlat = longlat[rmv]
  if (length(longlat) > 0) trial = trial[, -longlat]
  
  return(trial)
  
}

#' @rdname getDataCM
#' @param x an object of class \code{CM_list}
#' @param tidynames logical, \code{TRUE} make clean column names
#' @param pivot.wider logical, if \code{TRUE} return a wider object 
#'  where each tricot package is a row
#' @param ... additional arguments passed to methods
#' @importFrom methods as
#' @method as.data.frame CM_list
#' @export
as.data.frame.CM_list = function(x, 
                                 ...,
                                 tidynames = TRUE,
                                 pivot.wider = TRUE) {
  
  dat = x
  
  if (nrow(dat[["data"]]) <= 1) {
    message("Project ", dat$project$project_cod, " has no associated data. \n")
    return(data.frame())
  }
  
  # get variables names from participant registration
  regs = dat[["registry"]]
  
  regs = regs[["fields"]]
  
  regs_name = paste0("REG_", regs[, "name"])
  
  # get some info on the trial
  trial_tech = dat$combination$elements[[1]]$technology_name
  trial_pi = dat$project$project_pi
  trial_country = dat$project$project_cnty
  project_name = dat$project$project_cod
  project_id = dat$project$project_id
  
  if (isTRUE(has_data)) {
    
    ncomp = dat$project$project_numcom
    
    # get the names of assessments questions
    assess_q = dat[["specialfields"]]
    
    # check if overall VS local is present
    tricotvslocal = grepl("Performance", assess_q$type)
    
    # get the strings of ranking questions
    rank_q = assess_q$name[!tricotvslocal]
    
    # and the overall vs local question
    tricotvslocal = assess_q$name[tricotvslocal]
    
    # get variables names from assessments
    assess = dat[["assessments"]]
    
    if (length(assess) > 0) {
      assess = do.call("rbind", assess[, "fields"])
      assess = data.frame(assess, stringsAsFactors = FALSE)
      assess = assess[!duplicated(assess[, "name"]), ]
      # the ids for assessments
      assess_id = paste0("ASS", dat[["assessments"]][, "code"])
      # and the description
      assess_name = dat[["assessments"]][, "desc"]
      # the names of questions
      looknames = assess[, "name"]
      # the strings to use to decode values
      rtable = do.call("rbind", dat$assessments$fields)
      rtable = rtable[!is.na(rtable$rtable), c("name", "rtable")]
      
      # get codes from multiple choice variables
      assess_lkp = dat$assessments$lkptables
      lkp = list()
      for(i in seq_along(assess_lkp)){
        l = .decode_lkptable(assess_lkp[[i]])
        lkp = c(lkp, l)
      }
    } else {
      assess = data.frame()
      assess_id = character()
      assess_name = character()
      looknames = character()
      rtable = data.frame()
      lkp = list()
    }
    
    # add codes from registration
    reg_lkp = .decode_lkptable(dat$registry$lkptables)
    lkp = c(reg_lkp, lkp)
    
    # also the table strings
    reg_rtable = dat$registry$fields[,c("name","rtable")]
    reg_rtable = reg_rtable[!is.na(reg_rtable$rtable), ]
    rtable = rbind(rtable, reg_rtable)
    rtable$rtable = gsub("_lkp","_",rtable$rtable)
    
    # remove tricot questions
    rtable = rtable[!grepl("char_", rtable$name), ]
    # remove lkp table with farmers names
    rtable = rtable[!grepl("qst163", rtable$name), ]
    
    # add assessment code
    assesscode = lapply(strsplit(rtable$rtable, "_"), function(x) x[1])
    rtable$assess = do.call("rbind", assesscode)
    rtable$name = paste(rtable$assess, rtable$name, sep = "_")
    
    # trial data
    trial = dat[["data"]]
    
    # replace codes by labels in multi choice questions 
    n_rtable = nrow(rtable)
    
    for (i in seq_len(n_rtable)) {
      
      string_i = rtable[i, "rtable"]
      var_i = rtable[i, "name"]
      l = match(string_i, names(lkp))
      l = lkp[[l]]
      
      trial[var_i] = gsub(" ", "; ", trial[[var_i]])
      
      for (j in seq_along(l$id)) {
        
        trial[var_i] = gsub(l$id[j], l$label[j], trial[[var_i]])
        
      }
    }
    
    # split geolocation info
    # check if geographic location is available
    geoTRUE = grepl("farmgoelocation|geopoint|gps|geotrial|pointofdel", names(trial))
    
    # if is available, then split the vector as lon lat
    if (any(geoTRUE)) {
      
      geo_which = which(geoTRUE)
      
      geo = trial[geo_which]
      
      trial = trial[!geoTRUE]
      
      keep = unlist(lapply(geo[1:ncol(geo)], function(x) all(is.na(x))))
      
      geo = geo[, !keep]
      
      if(ncol(geo) == 0) geoTRUE = FALSE
      
    }
    
    if(any(geoTRUE)){
      
      for (i in seq_len(ncol(geo))){
        newname = names(geo)[[i]]
        newname = gsub("farmgoelocation", "_farm_geo", newname)
        newname = gsub("__", "_", newname)
        newname = paste0(newname, c("_longitude","_latitude", "_elevation","_gps_precision"))
        
        lonlat = geo[i]
        
        lonlat[is.na(lonlat)] = c("NA NA NA NA")
        lonlat[lonlat == "None"] = c("NA NA NA NA")
        lonlat[lonlat == ""] = c("NA NA NA NA")
        
        lonlat = t(apply(lonlat, 1, function(xx) {
          
          strsplit(xx, " ")[[1]][1:4]
          
        }))
        
        lonlat[lonlat == "NA"] = NA
        
        lonlat = lonlat[, c(2, 1, 3, 4)]
        
        lonlat = as.data.frame(lonlat, stringsAsFactors = FALSE)
        
        names(lonlat) = newname
        
        lonlat = apply(lonlat, 2, as.numeric)
        
        trial = cbind(trial, lonlat)
        
      }
      
      # for some odd reason the json file comes with empty long lat columns 
      # I will remove those just for the sake of not having so much confusion in the data
      longlat = grep("_longitude|_latitude|_elevation|_precision", names(trial))
      
      rmv = unlist(lapply(trial[longlat], function(x) all(is.na(x))))
      
      longlat = longlat[rmv]
      
      # only drop columns if there's something to drop
      if (length(longlat) > 0) {
        trial = trial[, -longlat]
      }
      
    }
    
    # replace numbers in trial results by LETTERS
    if (ncomp == 3) {
      trial[rank_q] =
        lapply(trial[rank_q], function(x) {
          y = as.integer(x)
          y = ifelse(y == 99, "Not observed", y)
          y = ifelse(y == 98, "Tie", y)
          y = ifelse(y == 1,  "A", y)
          y = ifelse(y == 2,  "B", y)
          y = ifelse(y == 3,  "C", y)
          y
        })
    }
    
    # replace numbers in question about overall vs local 
    if (length(tricotvslocal) > 1) {
      
      if (all(c(1, 2) %in% unlist(trial[tricotvslocal]))) {
        
        trial[tricotvslocal] =
          lapply(trial[tricotvslocal], function(x) {
            y = factor(x, levels = c("1", "2"), labels = c("Better", "Worse"))
            as.character(y)
          })
        
      }
      
    }
    
    # reshape it into a long format 
    # put pack id as first colunm
    packid = grepl("REG_qst162", names(trial))
    
    trial = cbind(trial[packid], trial[!packid])
    
    # remove possible space in assess name
    assess_name = tolower(gsub(" |-", "", assess_name))
    
    # add which moment the data was taken
    for (i in seq_along(assess_id)) {
      names(trial) = ifelse(grepl(assess_id[i], names(trial)),
                            gsub(assess_id[i], assess_name[i], names(trial)),
                            names(trial))
      
    }
    
  } 
  
  # comparisons and package
  comps = dat[["packages"]][, "comps"]
  
  comps = lapply(comps, function(x) {
    x = do.call("rbind", x$technologies)[,"alias_name"]
    names(x) = paste0("item_", LETTERS[1:length(x)])
    x
  })
  
  comps = do.call("rbind", comps)
  
  comps = as.data.frame(comps, stringsAsFactors = FALSE)
  
  names(comps) = paste0("package_", names(comps))
  
  pack = cbind(dat[["packages"]][, c("package_id","farmername")], comps)
  
  # add project name
  pack = cbind(project_id = project_id, 
               project_code = project_name, 
               project_technology = trial_tech,
               project_coordinator = trial_pi,
               project_country = trial_country,
               pack)
  
  names(trial)[names(trial) == "REG_qst162"] = "package_id"
  
  trial = merge(pack, trial, by = "package_id", all.x = TRUE)
  
  # check if ids from ODK names are required to be removed 
  if (isTRUE(tidynames)) {
    
    ovl = which(grepl("perf_overallchar|perf_overallper", names(trial)))
    
    names(trial)[ovl] = as.vector(sapply(names(trial)[ovl], function(x) {
      x = strsplit(x, split = "_")
      x = paste0(x[[1]][1], "_item_", LETTERS[as.integer(x[[1]][4])], "_vs_local")
      x
    }))
    
    names(trial) = gsub("REG_", "registration_", names(trial))
    
    names(trial) = gsub("char_", "", names(trial))
    
    names(trial) = gsub("stmt_", "pos", names(trial))
    
    names(trial) = gsub("clm_", "survey_", names(trial))
    
    names(trial) = gsub("^_","", names(trial))
    
    names(trial) = gsub("__","_", names(trial))
    
    names(trial) = gsub("__","_", names(trial))
    
    names(trial)[names(trial) == "farmername"] = "registration_participant_name"
    
  }
  
  output = trial
  
  rmv = "originid|rowuuid|qst163|clc_after|clc_before|instancename|id_string|surveyid|deviceimei|_active"
  
  # remove some ODK variables
  output = output[, !grepl(rmv, names(output)) ]
  
  # reorder columns and make sure that packages and registration comes first
  assess_name = paste0(union(c("project", "package", "registration"), assess_name), "_")
  
  ord = unique(as.vector(unlist(sapply(assess_name, function(x) {
    grep(x, names(output))
  }))))
  
  output = output[, ord]
  
  names(output) = gsub("_qst_", "_", names(output))
  
  if(isFALSE(pivot.wider)){
    trial = .set_long(output, "package_id")
    
    tags = strsplit(trial$variable, "_")
    
    moments = unlist(lapply(tags, function(x) x[[1]]))
    
    variables = unlist(lapply(tags, function(x) {paste(x[-1], collapse = "_")}))
    
    trial$variable = variables
    
    trial$moment = moments
    
    trial = trial[,c("id", "moment", "variable", "value")]
    
    output = trial
  }

  row.names(output) = seq_len(nrow(output))
  
  class(output) = union("CM_df", class(output))
  
  return(output)
  
}
