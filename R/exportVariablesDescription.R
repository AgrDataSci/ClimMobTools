#load("/Users/kauedesousa/Library/Mobile Documents/com~apple~CloudDocs/Work/Rcode/tricot-data-v1/processing/trial-data.rda")
load('/Users/kauedesousa/Library/Mobile Documents/com~apple~CloudDocs/Work/Rcode/tricot-data-v1/tests/test-data.rda')
library(ClimMobTools)
library(sf)

cmdata

df = cmdata[[1]]

exportTrialMetadata(df)

x = cmdata[[1]]

r = exportTricotRanks(x)

b = exportBlockData(x)

# get question descriptors
questions = x$registry$fields

q2 = list()

for(i in seq_along(x)) {
  q2 = c(q2, x$assessments$fields)
}

questions = c(questions, q2)

keep = unlist(lapply(questions, class)) == "data.frame"

questions = questions[keep]

questions = do.call("rbind", questions)

questions$name = gsub("_char|char_|_pos$|_neg$", "", questions$name)

questions$name


keep = !duplicated(questions$name)

questions = questions[keep, ]



vars = unique(c(names(b), r$trait))

vars

│   │   ├── variables
│   │   │   ├── variable name
│   │   │   ├── description
│   │   │   ├── ontology id
│   │   │   ├── value type
│   │   │   ├── unit
│   │   │   ├── controlled vocabulary




