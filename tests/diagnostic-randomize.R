library("gosset")
library("PlackettLuce")

# run diagnostics to certify that randomization is balanced
# number of interactions should have the lower sd as possible
# this verification may not work well when technologies are 
# tested in different proportions
ncomp = 3
npackages = 20
itemnames = c("apple","banana","grape","mango", "orange")
availability = c(5, 8, 50, 50, 50)

design = randomize(ncomp = ncomp,
                   npackages = npackages,
                   itemnames = itemnames)

design$best = "A"

design$worst = "C"

# number of times each item is tested in the 
# trial design
ntest = table(unlist(design[,c(1:3)]))

ntest

# put into the PlackettLuce structure to check 
# number of interactions between items 
r = gosset::rank_tricot(design, c(1:3), c("best", "worst"))

bn = gosset::set_binomialfreq(r)

bn$interactions = bn$win1 + bn$win2

bn = bn[,c(1,2,5)]

bn
