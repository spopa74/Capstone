## some helpers around TDM processing


## Java stuff
Sys.setenv(JAVA_HOME="")
options(java.parameters="-Xmx14g")

## load RWeka, tm libs
library(RWeka)
library(tm)

## load a previously saved TermDocumentMatrix
load("5.tdm")
tdmMatrix <- as.matrix(tdm)

## get the sums over rows
tf1grams <- rowSums(tdmMatrix)
tf1sorted <- sort(tf1grams, decreasing = TRUE)

## save the vector, has as element names the N-grams (.tf stands for term frequencies)
save(tf1grams, file="5Grams.tf")
save(tf1sorted, file="5Sorted.tf")


## --- CODE 1 ---- load all "no-ones" freq files
## STILL TOO BIG!!! 

## break the sorted vector into 4 different vectors, save all of them
## vector 1, full N-gram (NwordIndex.tf files, e.g. indexesNGramNO)
## vector 2, "prior" = N-1-grams (indexes in the N-1 vectors) (priorNGramNO files)
## vector 3, "posterior" = 1-gram (posteriorNGramNO files)
## vector 4, frequency of the N-gram (freqNGramNO files)

## NO == No Ones (the elements with counts = 1 removed)

## vectors for 1-Grams (no prior, no posterior)
load("3_grams//1noones.tf")
tf1SortedNO <- sort(tf1noones, decreasing = TRUE)
remove(tf1noones)
indexes1GramNO <- 1:length(tf1SortedNO)
names(indexes1GramNO) <- names(tf1SortedNO)
save(indexes1GramNO, file="4_models/indexes1GramNO.tf")
freq1GramNO <- unname(tf1SortedNO)
save(freq1GramNO, file="4_models/freq1GramNO.tf")
remove(tf1SortedNO)


## ------------------------ similar for 2-Grams
load("3_grams//2noones.tf")
tf2SortedNO <- sort(tf2noones, decreasing = TRUE)
remove(tf2noones)
len2 <-length(tf2SortedNO)
indexes2GramNO <- 1:len2
names(indexes2GramNO) <- names(tf2SortedNO)
save(indexes2GramNO, file="4_models/indexes2GramNO.tf")

## find prior, posterior
allNames2 <- names(tf2SortedNO)
freq2GramNO <- unname(tf2SortedNO)
save(freq2GramNO, file="4_models/freq2GramNO.tf")
remove(tf2SortedNO)

allNamesCollapsed2 <- paste(allNames2, collapse = " ")
allNamesVector2 <- unlist(strsplit(allNamesCollapsed2, split = " "))
remove(allNames2)
remove(allNamesCollapsed2)

prior2 <- allNamesVector2[seq.int(1, len2 * 2, 2)]
post2 <- allNamesVector2[seq.int(2, len2 * 2, 2)]
prior2Ix <- indexes1GramNO[prior2]
post2Ix <- indexes1GramNO[post2]
save(prior2Ix, file="4_models/prior2GramNO.tf")
save(post2Ix, file="4_models/posterior2GramNO.tf")
remove(allNamesVector2)
remove(prior2)
remove(post2)
remove(len2)


## ------------------------ similar for 3-Grams
load("3_grams//3noones.tf")
tf3SortedNO <- sort(tf3noones, decreasing = TRUE)
remove(tf3noones)
len3 <-length(tf3SortedNO)
indexes3GramNO <- 1:len3
names(indexes3GramNO) <- names(tf3SortedNO)
save(indexes3GramNO, file="4_models/indexes3GramNO.tf")

## find prior, posterior
allNames3 <- names(tf3SortedNO)
freq3GramNO <- unname(tf3SortedNO)
save(freq3GramNO, file="4_models/freq3GramNO.tf")
remove(tf3SortedNO)

allNamesCollapsed3 <- paste(allNames3, collapse = " ")
allNamesVector3 <- unlist(strsplit(allNamesCollapsed3, split = " "))
remove(allNames3)
remove(allNamesCollapsed3)

prior3_1 <- allNamesVector3[seq.int(1, len3 * 3, 3)]
prior3_2 <- allNamesVector3[seq.int(2, len3 * 3, 3)]
post3 <- allNamesVector3[seq.int(3, len3 * 3, 3)]
prior3Ix <- indexes2GramNO[paste(prior3_1, prior3_2, sep = " ")]
post3Ix <- indexes1GramNO[post3]
save(prior3Ix, file="4_models/prior3GramNO.tf")
save(post3Ix, file="4_models/posterior3GramNO.tf")
remove(allNamesVector3)
remove(prior3_1)
remove(prior3_2)
remove(post3)
remove(len3)


## ----------------- similar for 4-Grams
load("3_grams//4noones.tf")
tf4SortedNO <- sort(tf4noones, decreasing = TRUE)
remove(tf4noones)
len4 <-length(tf4SortedNO)
indexes4GramNO <- 1:len4
names(indexes4GramNO) <- names(tf4SortedNO)
save(indexes4GramNO, file="4_models/indexes4GramNO.tf")

## find prior, posterior
allNames4 <- names(tf4SortedNO)
freq4GramNO <- unname(tf4SortedNO)
save(freq4GramNO, file="4_models/freq4GramNO.tf")
remove(tf4SortedNO)

allNamesCollapsed4 <- paste(allNames4, collapse = " ")
allNamesVector4 <- unlist(strsplit(allNamesCollapsed4, split = " "))
remove(allNames4)
remove(allNamesCollapsed4)

prior4_1 <- allNamesVector4[seq.int(1, len4 * 4, 4)]
prior4_2 <- allNamesVector4[seq.int(2, len4 * 4, 4)]
prior4_3 <- allNamesVector4[seq.int(3, len4 * 4, 4)]
post4 <- allNamesVector4[seq.int(4, len4 * 4, 4)]
prior4Ix <- indexes3GramNO[paste(prior4_1, prior4_2, prior4_3, sep = " ")]
post4Ix <- indexes1GramNO[post4]
save(prior4Ix, file="4_models/prior4GramNO.tf")
save(post4Ix, file="4_models/posterior4GramNO.tf")
remove(allNamesVector4)
remove(prior4_1)
remove(prior4_2)
remove(prior4_3)
remove(post4)
remove(len4)


## ----------------- similar for 5-Grams
load("3_grams//5noones.tf")
tf5SortedNO <- sort(tf5noones, decreasing = TRUE)
remove(tf5noones)
len5 <-length(tf5SortedNO)
indexes5GramNO <- 1:len5
names(indexes5GramNO) <- names(tf5SortedNO)
save(indexes5GramNO, file="4_models/indexes5GramNO.tf")

## find prior, posterior
allNames5 <- names(tf5SortedNO)
freq5GramNO <- unname(tf5SortedNO)
save(freq5GramNO, file="4_models/freq5GramNO.tf")
remove(tf5SortedNO)

allNamesCollapsed5 <- paste(allNames5, collapse = " ")
allNamesVector5 <- unlist(strsplit(allNamesCollapsed5, split = " "))
remove(allNames5)
remove(allNamesCollapsed5)

prior5_1 <- allNamesVector5[seq.int(1, len5 * 5, 5)]
prior5_2 <- allNamesVector5[seq.int(2, len5 * 5, 5)]
prior5_3 <- allNamesVector5[seq.int(3, len5 * 5, 5)]
prior5_4 <- allNamesVector5[seq.int(4, len5 * 5, 5)]
post5 <- allNamesVector5[seq.int(5, len5 * 5, 5)]
prior5Ix <- indexes4GramNO[paste(prior5_1, prior5_2, prior5_3, prior5_4, sep = " ")]
post5Ix <- indexes1GramNO[post5]
save(prior5Ix, file="4_models/prior5GramNO.tf")
save(post5Ix, file="4_models/posterior5GramNO.tf")
remove(allNamesVector5)
remove(prior5_1)
remove(prior5_2)
remove(prior5_3)
remove(prior5_4)
remove(post5)
remove(len5)



## --- CODE 2 ---- load all "no-ones" freq files, then truncates them to 1 million based on counts
## - should be cut based on probability, but for now hopefully more manageable...

## break the sorted vector into 4 different vectors, save all of them
## vector 1, full N-gram (NwordIndex.tf files, e.g. indexesNGramNO)
## vector 2, "prior" = N-1-grams (indexes in the N-1 vectors) (priorNGramNO files)
## vector 3, "posterior" = 1-gram (posteriorNGramNO files)
## vector 4, frequency of the N-gram (freqNGramNO files)

## NO == No Ones (the elements with counts = 1 removed)

## vectors for 1-Grams (no prior, no posterior)
load("3_grams//1noones.tf")
tf1SortedNO <- sort(tf1noones, decreasing = TRUE)
remove(tf1noones)
indexes1GramNO <- 1:length(tf1SortedNO)
names(indexes1GramNO) <- names(tf1SortedNO)
save(indexes1GramNO, file="5_models/indexes1GramNO.tf")
freq1GramNO <- unname(tf1SortedNO)
save(freq1GramNO, file="5_models/freq1GramNO.tf")
remove(tf1SortedNO)

## calculate probabilities, no need to sort them (already sorted)
counts1GramNO <- sum(freq1GramNO)
prob1GramNO <- freq1GramNO / counts1GramNO
names(prob1GramNO) <- names(indexes1GramNO)
save(prob1GramNO, file="5_models/prob1GramNO.tf")
minProbable1Gram <- names(tail(prob1GramNO, 1))



## ------------------------ similar for 2-Grams
load("3_grams//2noones.tf")
tf2SortedNO <- sort(tf2noones, decreasing = TRUE)
tf2SortedNO <- head(tf2SortedNO, 1000000)
remove(tf2noones)
len2 <-length(tf2SortedNO)
indexes2GramNO <- 1:len2
names(indexes2GramNO) <- names(tf2SortedNO)
save(indexes2GramNO, file="5_models/indexes2GramNO.tf")

## find prior, posterior
allNames2 <- names(tf2SortedNO)
freq2GramNO <- unname(tf2SortedNO)
save(freq2GramNO, file="5_models/freq2GramNO.tf")
remove(tf2SortedNO)

allNamesCollapsed2 <- paste(allNames2, collapse = " ")
allNamesVector2 <- unlist(strsplit(allNamesCollapsed2, split = " "))
remove(allNames2)
remove(allNamesCollapsed2)

prior2 <- allNamesVector2[seq.int(1, len2 * 2, 2)]
post2 <- allNamesVector2[seq.int(2, len2 * 2, 2)]
prior2Ix <- indexes1GramNO[prior2]
post2Ix <- indexes1GramNO[post2]
save(prior2Ix, file="5_models/prior2GramNO.tf")
save(post2Ix, file="5_models/posterior2GramNO.tf")
remove(allNamesVector2)
remove(prior2)
remove(post2)
remove(len2)

##calculate probabilities, need to sort them at the end
prob2GramNO <- vector(mode = "integer", length = length(freq2GramNO))
for(i in 1:length(indexes2GramNO)) {
  prob2GramNO[i] <- (freq2GramNO[i] / freq1GramNO[prior2Ix[i]]) * prob1GramNO[prior2Ix[i]]
}



## ------------------------ similar for 3-Grams
load("3_grams//3noones.tf")
tf3SortedNO <- sort(tf3noones, decreasing = TRUE)
tf3SortedNO <- head(tf3SortedNO, 1000000)
remove(tf3noones)
len3 <-length(tf3SortedNO)
indexes3GramNO <- 1:len3
names(indexes3GramNO) <- names(tf3SortedNO)
save(indexes3GramNO, file="5_models/indexes3GramNO.tf")

## find prior, posterior
allNames3 <- names(tf3SortedNO)
freq3GramNO <- unname(tf3SortedNO)
save(freq3GramNO, file="5_models/freq3GramNO.tf")
remove(tf3SortedNO)

allNamesCollapsed3 <- paste(allNames3, collapse = " ")
allNamesVector3 <- unlist(strsplit(allNamesCollapsed3, split = " "))
remove(allNames3)
remove(allNamesCollapsed3)

prior3_1 <- allNamesVector3[seq.int(1, len3 * 3, 3)]
prior3_2 <- allNamesVector3[seq.int(2, len3 * 3, 3)]
post3 <- allNamesVector3[seq.int(3, len3 * 3, 3)]
prior3Ix <- indexes2GramNO[paste(prior3_1, prior3_2, sep = " ")]
post3Ix <- indexes1GramNO[post3]
save(prior3Ix, file="5_models/prior3GramNO.tf")
save(post3Ix, file="5_models/posterior3GramNO.tf")
remove(allNamesVector3)
remove(prior3_1)
remove(prior3_2)
remove(post3)
remove(len3)

prob3GramNO <- vector(mode = "integer", length = length(freq3GramNO))
for(i in 1:length(indexes3GramNO)) {
  prob3GramNO[i] <- (freq3GramNO[i] / freq2GramNO[prior3Ix[i]]) * prob2GramNO[prior3Ix[i]]
}
names(prob3GramNO) <- names(indexes3GramNO)
prob3GramNO <- sort(prob3GramNO, decreasing = TRUE)
save(prob3GramNO, file="5_models/prob3GramSimple.tf")
minProbable3Gram <- names(tail(prob3GramNO, 1))



## ----------------- similar for 4-Grams
load("3_grams//4noones.tf")
tf4SortedNO <- sort(tf4noones, decreasing = TRUE)
tf4SortedNO <- head(tf4SortedNO, 1000000)
remove(tf4noones)
len4 <-length(tf4SortedNO)
indexes4GramNO <- 1:len4
names(indexes4GramNO) <- names(tf4SortedNO)
save(indexes4GramNO, file="5_models/indexes4GramNO.tf")

## find prior, posterior
allNames4 <- names(tf4SortedNO)
freq4GramNO <- unname(tf4SortedNO)
save(freq4GramNO, file="5_models/freq4GramNO.tf")
remove(tf4SortedNO)

allNamesCollapsed4 <- paste(allNames4, collapse = " ")
allNamesVector4 <- unlist(strsplit(allNamesCollapsed4, split = " "))
remove(allNames4)
remove(allNamesCollapsed4)

prior4_1 <- allNamesVector4[seq.int(1, len4 * 4, 4)]
prior4_2 <- allNamesVector4[seq.int(2, len4 * 4, 4)]
prior4_3 <- allNamesVector4[seq.int(3, len4 * 4, 4)]
post4 <- allNamesVector4[seq.int(4, len4 * 4, 4)]

## it's possible that some priors for an N-gram is not in the list of the N-1 grams
## in that case, give it the lowest possible value
prior4 <- paste(prior4_1, prior4_2, prior4_3, sep = " ")
namesPrior4 <- names(prior4)
for (i in 1:length(prior4)) {
  if (! (prior4[i] %in% namesPrior4)) {
    prior4[i] <- minProbable3Gram
  }
}
prior4Ix <- indexes3GramNO[prior4]
post4Ix <- indexes1GramNO[post4]
save(prior4Ix, file="5_models/prior4GramNO.tf")
save(post4Ix, file="5_models/posterior4GramNO.tf")
remove(allNamesVector4)
remove(prior4_1)
remove(prior4_2)
remove(prior4_3)
remove(post4)
remove(len4)

## calculate the probabilities
prob4GramNO <- vector(mode = "integer", length = length(freq4GramNO))
for(i in 1:length(indexes4GramNO)) {
  prob4GramNO[i] <- (freq4GramNO[i] / freq3GramNO[prior4Ix[i]]) * prob3GramNO[prior4Ix[i]]
}
names(prob4GramNO) <- names(indexes4GramNO)
prob4GramNO <- sort(prob4GramNO, decreasing = TRUE)
save(prob4GramNO, file="5_models/prob4GramSimple.tf")
minProbable4Gram <- names(tail(prob4GramNO, 1))



## ----------------- similar for 5-Grams
load("3_grams//5noones.tf")
tf5SortedNO <- sort(tf5noones, decreasing = TRUE)
tf5SortedNO <- head(tf5SortedNO, 1000000)
remove(tf5noones)
len5 <-length(tf5SortedNO)
indexes5GramNO <- 1:len5
names(indexes5GramNO) <- names(tf5SortedNO)
save(indexes5GramNO, file="5_models/indexes5GramNO.tf")

## find prior, posterior
allNames5 <- names(tf5SortedNO)
freq5GramNO <- unname(tf5SortedNO)
save(freq5GramNO, file="5_models/freq5GramNO.tf")
remove(tf5SortedNO)

allNamesCollapsed5 <- paste(allNames5, collapse = " ")
allNamesVector5 <- unlist(strsplit(allNamesCollapsed5, split = " "))
remove(allNames5)
remove(allNamesCollapsed5)

prior5_1 <- allNamesVector5[seq.int(1, len5 * 5, 5)]
prior5_2 <- allNamesVector5[seq.int(2, len5 * 5, 5)]
prior5_3 <- allNamesVector5[seq.int(3, len5 * 5, 5)]
prior5_4 <- allNamesVector5[seq.int(4, len5 * 5, 5)]
post5 <- allNamesVector5[seq.int(5, len5 * 5, 5)]

## it's possible that some priors for an N-gram is not in the list of the N-1 grams
## in that case, give it the lowest possible value
prior5 <- paste(prior5_1, prior5_2, prior5_3, prior5_4, sep = " ")
namesPrior5 <- names(prior5)
for (i in 1:length(prior5)) {
  if (! (prior5[i] %in% namesPrior5)) {
    prior5[i] <- minProbable4Gram
  }
}
prior5Ix <- indexes4GramNO[prior5]
post5Ix <- indexes1GramNO[post5]
save(prior5Ix, file="5_models/prior5GramNO.tf")
save(post5Ix, file="5_models/posterior5GramNO.tf")
remove(allNamesVector5)
remove(prior5_1)
remove(prior5_2)
remove(prior5_3)
remove(prior5_4)
remove(post5)
remove(len5)

## calculate probabilities for these
prob5GramNO <- vector(mode = "integer", length = length(freq5GramNO))
for(i in 1:length(indexes5GramNO)) {
  prob5GramNO[i] <- (freq5GramNO[i] / freq4GramNO[prior5Ix[i]]) * prob4GramNO[prior5Ix[i]]
}
names(prob5GramNO) <- names(indexes5GramNO)
prob5GramNO <- sort(prob5GramNO, decreasing = TRUE)
save(prob5GramNO, file="5_models/prob5GramSimple.tf")
minProbable5Gram <- names(tail(prob5GramNO, 1))
