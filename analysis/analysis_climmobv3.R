# ................................................................
# ................................................................
# Analyse the performance of agricultural technologies from 
# crowdsourcing trials using Plackett-Luce model
# ................................................................
# ................................................................
# Kaue de Sousa 
# Updated 27Mar2019
# ................................................................
# ................................................................
#system("R-script, climmobv3_analysis.R info.json data.json r.json /output/ TRUE TRUE")
#args <- c("d39a3c66-5822-4930-a9d4-50e7da041e77", "chocolate", 
#          "data/data.json", "r4.json", 
#          "/data/.output", TRUE , TRUE)

# get the arguments
args <- commandArgs(trailingOnly = TRUE)
key            <- args[1] # the user key
project_id     <- args[2] # the project id
infoname       <- args[3] # a json file with parameters for the analysis
outputname     <- args[4] # a json file with the results
pathname       <- args[5] # the path where results will be written
overallVSlocal <- as.logical(args[6]) # logical() TRUE FALSE
infosheets     <- as.logical(args[7]) # logical() TRUE FALSE


## Packages
library("tidyverse")
library("svglite")
library("plotly")
library("jsonlite")
library("httr")
library("partykit")
library("qvcalc")
library("psychotools")
library("PlackettLuce")
library("gosset")

# ................................................................
# ................................................................
# Read data #### 
data <- gosset::getDataCM(key = key, 
                          project = project_id,
                          tidynames = FALSE)

# put it in wide format
data <- data[,-2]
data <- tidyr::spread(data, variable, value)

# Read data with selected traits and explanatory variables to be analysed
pars <- jsonlite::read_json(infoname)

# ................................................................
# ................................................................
# Get pars ready for analysis ####

# Number of comparisons in each trial (often 3) 
ncomp <- sum(grepl("item_", names(data)))

# Number of comp questions during assessments
if(ncomp < 4){
  nquest <- ncomp - 1
}else{
  nquest <- ncomp
}

if (ncomp < 3){
  stop("\nComparisons with less than 3 items not supported yet.\n")
}

# Get characteristics to evaluate
chars <- pars[[1]]

chars <- lapply(chars, function(x) {
  x <- unlist(x)
})

chars <- do.call("rbind", chars)

# remove last colunm, odk id
chars <- chars[, -ncol(chars)]

# remove possible space " " in characteristics names
chars[, 1] <- gsub(" ", "", chars[, 1])

nameschars <- chars[, 1]

# Get explanatory variables
expvar <- pars[[2]]

if (length(expvar) == 0) {
  cat("\nNo explanatory variable selected. Fit model with the intercept.\n")
  expvar <- tibble::tibble(P1 = rep(1, nrow(data)))
}
# else
#code to reshape list when length(expvar) > 1
#keep it for latter, since I don't know the struture of this object
####

# Get names of items in data
itemnames <- grepl("item_", names(data))

itemnames <- data[itemnames]

itemnames <- sort(unique(unlist(itemnames)))

# ................................................................
# ................................................................
# Prepare the list for the json file output ####

# List to keep all outputs
# the json file with all results
results <- vector(mode = "list", length = 3 + nrow(chars))
names(results) <- c("Items", "Characteristics", nameschars, "Infosheets")

# list of all items used in this project
Items <- as.data.frame(cbind(techName = project_id, aliasName = itemnames))
results[["Items"]] <- apply(Items, 1, as.list)

# list of characteristics used in this analysis
results[["Characteristics"]] <- as.list(nameschars)

# an array to put all rankings as ordered by each farmer
# used for the infosheets (how you ranked)
R_all <- array(
  "NA",
  dim = c(nrow(data), ncomp, nrow(chars)),
  dimnames = list(1:nrow(data), 1:ncomp , 1:nrow(chars))
)

# check if output dir exists,
# if not, create it
if(!file.exists(pathname)){
  
  x <- unlist(strsplit(pathname, "[/]"))
  
  x <- x[!x %in% ""]
  
  pathname <- paste(x, collapse = "/")
  
  dir.create(pathname, showWarnings = FALSE, recursive = TRUE)
  
}

# ................................................................
# ................................................................
# Run Plackett-Luce model over characteristics ####

for(i in seq_along(nameschars)){
  
  char_i <- nameschars[i]
  cat("\n\nAnalysing the performance for the characteristic:", char_i , "\n")
  
  # get the rankings for the characteristic to be analysed
  rank_i <- data[, sort(chars[i, c(2:ncol(chars))])]
  
  # get the items
  items_i <- data[, grepl("item_", names(data))]
  
  # check if the information is complete
  keep <- apply(cbind(rank_i, expvar), 1, function(x){
    x <- is.na(x)
    x <- !any(x)
  })
  
  # keep checking for missing data when overall vs local is TRUE
  if(char_i == "Overall" & overallVSlocal){
    
    local_rank <- data[grepl("_perf_overallchar_", names(data))]
    
    keep2 <- apply(local_rank, 1, function(x){
      x <- is.na(x)
      x <- !any(x)
    })
    
    keep <- keep & keep2
    
    local_rank <- local_rank[keep, ]
    
  }else{
    
    local_rank <- NULL
  
  }
  
  # now check missing data for explanatory variables
  # first get a logic list of non NA values
  x <- lapply(expvar[1:ncol(expvar)], function(X) !is.na(X)) 
  # then keep those values where the sum is equal to ncomp
  x <- matrix(unlist(x), ncol = ncol(expvar), nrow = nrow(expvar))
  # Refresh keep
  keep <- rowSums(x) == ncol(expvar) & keep
  
  # apply the keep vector
  items_i <- items_i[keep, ]
  rank_i <- rank_i[keep, ]
  expvar_i <- expvar[keep, ]

  # convert this dataframe into a grouped_rankings object
  myrank <- gosset::to_rankings(items = items_i,
                                rankings = rank_i,
                                type = "tricot",
                                add.rank = local_rank, 
                                all.data = TRUE)
  
  # Get the rankings
  R <- myrank[[1]]
  
  # get the grouped rankings 
  G <- myrank[[2]]
  
  # add positions of items to the array for infosheets
  R_all[keep, , i] <- as.matrix(myrank[[3]])
  
  # get the number of valid values
  nR <- sum(keep)
  
  if (nrow(data)-nR > 0) {
    cat("\n",nrow(data)-nR, "observations removed due to incosistent rankings or missing values \n")
    cat("Using", nR, "of", nrow(data), "observations \n" )
  }
  
  # fit the model without explanatory variables 
  mod <- PlackettLuce::PlackettLuce(R)
  
  # get table with coefficients and p values 
  mod_coeff <- qvcalc::qvcalc(mod)$qvframe
  
  # add row names (item names) to column
  mod_coeff <- tibble::rownames_to_column(mod_coeff, var = "Item")
  
  # transform this dataframe into a list
  mod_coeff <- apply(mod_coeff, 1,  as.list)
  
  # combine explanatory variables and grouped rankings
  G <- cbind(G, expvar_i)
  
  #Fit Plackett-Luce tree
  tree <- PlackettLuce::pltree(G ~ . , data = G, 
                               alpha = 0.05)
  
  # make a plot for the tree
  # first using modelparty from partykit
  # write it and keep the path 
  svg(filename = paste0(pathname, "/", char_i,"_tree.svg"),
      width=6.5,
      height=6.5,
      pointsize=12)
  par(mgp=c(2.2,0.45,0), tcl=-0.4, mar=c(0,0,0,0))
  partykit::plot.modelparty(tree) 
  dev.off()
  
  treepath <- paste0(pathname, "/", char_i,"_tree.svg")
  
  # then using quasi-variance from qvcalc 
  # this will genenerate one plot per node
  # write it and keep the path
  # get plots
  plots <- gosset::plot_nodes(tree)
  #define names
  nodepaths <- paste0(pathname, "/", char_i,"_tree", names(plots) ,".svg")
  #put names in a list
  nodepaths <- as.list(nodepaths)
  #define names of each element in this list
  names(nodepaths) <- names(plots)
  
  # export plots 
  h <- length(coef(mod)) + 1
  mapply(function(X, Y){
    
    ggsave(filename = Y, plot = X,
           dpi = 600, width = 7.5, height = h, units = "cm")
    
  }, X = plots, Y = nodepaths )
  
  
  # Take observations with errors
  logs <- cbind( data[!keep,], expvar[!keep, ])
  # Transform this dataframe into a list
  logs <- apply(logs, 1,  as.list)
  
  # Add to list of outputs
  outputs <- list(
    coeff = mod_coeff, # table with coefficients from PL model (estimates, error, z-value),
    vars = as.list(names(expvar_i)), # name of explanatory vars used 
    tree = treepath, # the Plackett-Luce tree produced
    treenode = nodepaths, # the plots for each node produced
    nobs = as.list(nR), # number of observations used to analyse this characteristic
    logs = logs # dataframe with the observations not used for this analysis
  )
  
  results[[char_i]] <- outputs
  
  
}


# ................................................................
# ................................................................
# Prepare infosheet data if required ####

if(infosheets){
  
  sheets <- vector(mode = "list", nrow(data))
  
  cat('Organising data for infosheets \n')
  
  # copy trail data 
  df2 <- data
  
  # get information for the header
  # it shows the participant name and the package id 
  header <- data.frame(cbind(techName = "ParticipantName", 
                             aliasName = data[["farmername"]], 
                             packId = data[["id"]] ),
                       stringsAsFactors = FALSE)
  
  header <- apply(header, 1, as.list)
  
  # get name of given items
  # it shows which items were given to each participant and how they where labeled (A,B,C)
  items <- df2[grepl("item_", names(df2))]
  table1 <- apply(items, 1, function(X){
    
    Y <- data.frame(cbind(Item = colnames(items), Name = X), stringsAsFactors = FALSE)
    as.list(Y)
    
  })
  
  # get table showing how each participant classified items
  # it shows how each participant ranked their given items from best to worst
  # first combine the rows from each third dimension in the array
  table2 <-  apply(R_all, 1, function(X){
    r <- t(X)
    r <- data.frame(r, stringsAsFactors = FALSE)
    rownames(r) <- chars[,1]
    colnames(r) <- paste0("Position", seq_len(ncomp))
    r <- rownames_to_column(r, var = "Characteristic")
    r
  })
  
  # now each dimension in the array corresponds to one observer and 
  # each row to one characteristic
  # convert it into a list 
  table2 <- as.list(table2)
  
  # then into a list again
  table2 <- lapply(table2, function(X){
    apply(X, 1,  as.list)
  })
  
  
  # get the overall rankings 
  # it shows how the items where ranked among the 
  # project considering all rankings provided
  io <- which(grepl("OverallCharacteristic", chars[,1]))
  
  table3 <- results[[io]]$coeff
  table3 <- lapply(table3 , unlist)
  table3 <- do.call(rbind, table3)
  table3 <- cbind(Item = table3[,1], 
                  Position = rank((as.numeric(table3[,2]) -1) * - 1))
  table3 <- data.frame(table3, stringsAsFactors = FALSE)
  
  # drop local variety if exists
  table3 <- table3[!table3[,1] %in% "Local",]
  
  # update position
  table3[,2] <- rank(table3[,2])
  
  # sort values from position 1 to n
  table3 <- arrange(table3, table3[,2])
  table3 <- apply(table3, 1,  as.list)
  
  # combine the lists per observer
  for(i in seq_along(rownames(data))){
    
    sheets[[i]] <- list(header = header[[i]], 
                        table1 = table1[[i]], 
                        table2 = table2[[i]],
                        table3 = table3)
  }
  
  # each element in this list corresponds to an observer
  # package id will be used to label it
  names(sheets) <- df2$id
  
  #add this list to results
  results$Infosheets <- sheets
  
}

# ................................................................
# ................................................................
# Write json files #### 

# convert it into json
output <- jsonlite::toJSON(results, pretty = TRUE, auto_unbox = TRUE)

# write json file
write(output, paste0(pathname, "/", outputname))

cat("\n\nAll analysis done. Check your results here:", paste0(getwd(),"/", pathname), "\n")