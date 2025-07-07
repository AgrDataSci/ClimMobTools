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

#' Safely extract a nested list element
#' @param x list object
#' @param path character vector of nested names or indices
#' @param default value to return if path does not exist or is NULL
#' @noRd
.safe_extract = function(x, path, default = NA) {
  
  if (any(!is.na(suppressWarnings(as.integer(path))))) {
    
    index_integer = which(!is.na(suppressWarnings(as.integer(path))))
   
    # path into list to handle unnamed elements in the list
    path = as.list(path)
    
    path[index_integer] = lapply(path[index_integer], as.integer)
    
  }
  
  for (p in path) {
    
    if (is.null(x) || is.null(x[[p]])) return(default)
    
    x = x[[p]]
    
  }
  
  return(if (is.null(x)) default else x)
  
}

#' Replace codes by factors
#' @param trial_dat the trial data to apply the replacement 
#' @param x the climmob raw list file to identify the replacement cases
#' @return \code{trial_dat} the input data.frame with the cases replaced 
#' @noRd
.replace_multichoice_codes = function(trial_dat, x) {
  
  # decode lookup tables
  lkp = c(.decode_lkptable(x$registry$lkptables),
          unlist(lapply(x$assessments$lkptables, .decode_lkptable), recursive = FALSE))
  
  # get variables names from assessments
  assess = x[["assessments"]]
  
  rtable = data.frame()
  
  if (length(assess) > 0) {
    rtable = do.call("rbind", x$assessments$fields)
    rtable = rtable[!is.na(rtable$rtable), c("name", "rtable")]
  }
  
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
    if (!var_i %in% names(trial_dat)) next
    l = lkp[[string_i]]
    if (is.null(l)) next
    trial_dat[[var_i]] = gsub(" ", "; ", trial_dat[[var_i]])
    for (j in seq_along(l$id)) {
      trial_dat[[var_i]] = gsub(l$id[j], l$label[j], trial_dat[[var_i]])
    }
  }
  
  return(trial_dat)
  
}

#' Handle geolocation columns
#' @param trial_dat the trial data to apply the replacement 
#' @param pattern character, vector with the patterns in the columns to find geopoints
#' @return \code{trial_dat} the input data.frame with the new longlat columns 
#' @noRd
.handle_geolocation_columns = function(trial_dat, pattern = c("geotrial","farmgoelocation",
                                                              "geopoint", "gps")) {
  
  pattern = paste(pattern, collapse = "|")
  
  geoTRUE = grepl(pattern, names(trial_dat))
  
  if (!any(geoTRUE)) return(trial_dat)
  
  geo = trial_dat[geoTRUE]
  trial_dat = trial_dat[!geoTRUE]
  keep = unlist(lapply(geo, function(x) !all(is.na(x))))
  geo = geo[, keep, drop = FALSE]
  
  if (ncol(geo) == 0) return(trial_dat)
  
  for (i in seq_len(ncol(geo))) {
    
    newname = gsub("farmgoelocation", "_farm_geo", names(geo)[[i]])
    newname = gsub("__", "_", newname)
    newname = paste0(newname, c("_longitude", "_latitude", "_elevation", "_gps_precision"))
    
    lonlat = geo[i]
    lonlat[is.na(lonlat) | lonlat == "None" | lonlat == ""] = "NA NA NA NA"
    lonlat = t(apply(lonlat, 1, function(xx) {
      strsplit(xx, " ")[[1]][1:4]
    }))
    
    lonlat[lonlat == "NA"] = NA
    
    lonlat = lonlat[, c(2, 1, 3, 4)]
    
    lonlat = as.data.frame(lonlat, stringsAsFactors = FALSE)
    
    names(lonlat) = newname
    
    lonlat = apply(lonlat, 2, as.numeric)
    
    trial_dat = cbind(trial_dat, lonlat)
    
  }
  
  longlat = grep("_longitude|_latitude|_elevation|_precision", names(trial_dat))
  rmv = unlist(lapply(trial_dat[longlat], function(x) all(is.na(x))))
  longlat = longlat[rmv]
  if (length(longlat) > 0) trial_dat = trial_dat[, -longlat]
  
  return(trial_dat)
  
}

#' Replace ranking codes into factors
#' @param trial_dat the trial data to apply the replacement
#' @param ncomp integer with the number of comparisons in the trial_dat design 
#' @param specialfields data.frame with characteristics assessed in the trial_dat 
#' @return \code{trial_dat} the input data.frame with the new recoded values 
#' @noRd
.replace_rankings = function(trial_dat, ncomp, specialfields) {
  tricotvslocal = grepl("Performance", specialfields$type)
  rank_q = specialfields$name[!tricotvslocal]
  tricotvslocal = specialfields$name[tricotvslocal]
  
  if (ncomp == 3 && length(rank_q)) {
    trial_dat[rank_q] = lapply(trial_dat[rank_q], function(x) {
      y = as.integer(x)
      y = ifelse(y == 99, "Not observed", y)
      y = ifelse(y == 98, "Tie", y)
      y = ifelse(y == 1,  "A", y)
      y = ifelse(y == 2,  "B", y)
      y = ifelse(y == 3,  "C", y)
      y
    })
  }
  
  if (length(tricotvslocal) > 1 && all(c(1, 2) %in% unlist(trial_dat[tricotvslocal]))) {
    trial_dat[tricotvslocal] = lapply(trial_dat[tricotvslocal], function(x) {
      y = factor(x, levels = c("1", "2"), labels = c("Better", "Worse"))
      as.character(y)
    })
  }
  
  return(trial_dat)
}

#' Replace assessment ODK code by its names 
#' @param trial_dat the trial data to apply the replacement 
#' @param x the climmob raw list file to identify the replacement cases
#' @return \code{trial_dat} the input data.frame with the cases replaced in column names 
#' @noRd
.decode_assessments = function(trial_dat, x) {
  assess_id = paste0("ASS", x$assessments$code)
  assess_name = tolower(gsub(" |-", "", x$assessments$desc))
  assess_name = gsub("[^a-z0-9]", "", assess_name)
  
  for (i in seq_along(assess_id)) {
    names(trial_dat) = ifelse(grepl(assess_id[i], names(trial_dat)),
                              gsub(assess_id[i], assess_name[i], names(trial_dat)),
                              names(trial_dat))
  }
  
  names(trial_dat) = gsub("__", "_", names(trial_dat))
  
  names(trial_dat) = gsub("REG_", "registration_", names(trial_dat))
  
  return(trial_dat)
}

#' Merge trial data with tricot packages 
#' @param trial_dat the trial data to merge
#' @param x the climmob raw list file 
#' @return \code{trial_dat} the input data.frame with the package info added 
#' @noRd
.merge_package_info = function(trial_dat, x) {
  comps = x$packages$comps
  comps = lapply(comps, function(x) {
    x = do.call("rbind", x$technologies)[, "alias_name"]
    names(x) = paste0("item_", LETTERS[1:length(x)])
    x
  })
  comps = do.call("rbind", comps)
  comps = as.data.frame(comps, stringsAsFactors = FALSE)
  names(comps) = paste0("package_", names(comps))
  
  pack = cbind(
    project_id = .safe_extract(x, c("project", "project_id")),
    project_code = .safe_extract(x, c("project", "project_cod")), 
    project_technology = .safe_extract(x, c("combination", "elements", 1, "technology_name", 1)),
    project_coordinator = .safe_extract(x, c("project", "project_pi")),
    project_country = .safe_extract(x, c("project", "project_cnty")),
    x$packages[, c("package_id", "farmername")],
    comps
  )
  
  names(pack)[names(pack) == "farmername"] = "registration_participant_name"
  
  names(trial_dat)[names(trial_dat) == "registration_qst162"] = "package_id"
  
  trial_dat = merge(pack, trial_dat, by = "package_id", all.x = TRUE)
  
  return(trial_dat)
  
}

#' Clean column names
#' @param trial_dat the trial data to clean
#' @return \code{trial_dat} the input data.frame with the package info added 
#' @noRd
.clean_column_names = function(trial_dat) {
  
  ovl = which(grepl("perf_overallchar|perf_overallper", names(trial_dat)))
  
  names(trial_dat)[ovl] = as.vector(sapply(names(trial_dat)[ovl], function(x) {
    x = strsplit(x, split = "_")
    x = paste0(x[[1]][1], "_item_", LETTERS[as.integer(x[[1]][4])], "_vs_local")
    x
  }))
  
  names(trial_dat) = gsub("REG_", "registration_", names(trial_dat))
  names(trial_dat) = gsub("char_", "_", names(trial_dat))
  names(trial_dat) = gsub("stmt_", "pos", names(trial_dat))
  names(trial_dat) = gsub("clm_", "survey_", names(trial_dat))
  names(trial_dat) = gsub("^_+", "", names(trial_dat))
  names(trial_dat) = gsub("__+", "_", names(trial_dat))
  names(trial_dat) = gsub("__", "_", names(trial_dat))
  return(trial_dat)
}

#' Drop ODK system fields 
#' @param trial_dat the trial data to clean
#' @param pattern character, vector with the patterns in the columns to drop
#' @return \code{trial_dat} the input data.frame with subset data
#' @noRd
.drop_odk_system_fields = function(trial_dat, pattern = c("originid", "rowuuid",
                                                          "qst163", "clc_after", "clc_before",
                                                          "instancename", "id_string", "surveyid", 
                                                          "deviceimei", "_active", "farmername")) {
  rmv = paste(pattern, collapse = "|")
  
  trial_dat = trial_dat[, !grepl(rmv, names(trial_dat))]
  
  return(trial_dat)
}

#' Re-order columns 
#' @param trial_dat the trial data to clean
#' @param x the climmob raw list file
#' @return \code{trial_dat} the input data.frame with the re-ordered columns 
#' @noRd
.reorder_columns = function(trial_dat, x) {
  
  assess_name = tolower(gsub(" |-", "", x$assessments$desc))
  assess_name = gsub("[^a-z0-9]", "", assess_name)
  assess_name = paste0(c("project", "package", "registration", assess_name), "_")
  
  ord = unique(as.vector(unlist(sapply(assess_name, function(x) {
    grep(x, names(trial_dat))
  }))))
  
  trial_dat = trial_dat[, ord]
  
  names(trial_dat) = gsub("_qst_", "_", names(trial_dat))
  
  return(trial_dat)
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
  
  trial = dat[["data"]]
  
  trial = .replace_multichoice_codes(trial, dat)
  trial = .handle_geolocation_columns(trial)
  trial = .replace_rankings(trial, dat$project$project_numcom, dat$specialfields)
  trial = .decode_assessments(trial, dat)
  trial = .merge_package_info(trial, dat)

  if (isTRUE(tidynames)) {
    trial = .clean_column_names(trial)
  }
  
  trial = .drop_odk_system_fields(trial)
  trial = .reorder_columns(trial, dat)
  
  if (isFALSE(pivot.wider)) {
    
    trial = .set_long(trial, "package_id")
    
    tags = strsplit(trial$variable, "_")
    
    moments = unlist(lapply(tags, function(x) x[[1]]))
    
    variables = unlist(lapply(tags, function(x) {paste(x[-1], collapse = "_")}))
    
    trial$variable = variables
    
    trial$moment = moments
    
    trial = trial[,c("id", "moment", "variable", "value")]
    
  }
  
  row.names(trial) = seq_len(nrow(trial))
  
  class(trial) = union("CM_df", class(trial))
  
  return(trial)
  
}
