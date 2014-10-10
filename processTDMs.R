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
