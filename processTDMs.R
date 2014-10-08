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
## vector 1, full N-gram (NwordIndex.tf files, e.g. 1wordIndex.tf)
## vector 2, "prior" = N-1-grams (indexes in the N-1 vectors) (NpriorIx.tf files)
## vector 3, "posterior" = 1-gram (NposteriorIx.tf files)
## vector 4, frequency of the N-gram (Nfreq.tf files)

## vectors for 1-Grams (no prior, no posterior)
wordIndex1Gram <- 1:length(tf1sorted)
names(wordIndex1Gram) <- names(tf1sorted)
save(wordIndex1Gram, file="4_models/1wordIndex.tf")
freq1Gram <- unname(tf1sorted)
save(freq1Gram, file="4_models/1freq.tf")

## ------------------------ similar for 2-Grams
len2 <-length(tf2sorted)
wordIndex2Gram <- 1:len2
names(wordIndex2Gram) <- names(tf2sorted)
save(wordIndex2Gram, file="4_models/2wordIndex.tf")
## find prior, posterior
allNames2 <- names(tf2sorted)
allNamesCollapsed2 <- paste(allNames2, collapse = " ")
allNamesVector2 <- unlist(strsplit(allNamesCollapsed2, split = " "))

prior2 <- allNamesVector2[seq.int(1, len2 * 2, 2)]
post2 <- allNamesVector2[seq.int(2, len2 * 2, 2)]
prior2Ix <- wordIndex1Gram[prior2]
post2Ix <- wordIndex1Gram[post2]
save(prior2Ix, file="4_models/2priorIx.tf")
save(post2Ix, file="4_models/2posteriorIx.tf")

tf2sorted <- unname(tf2sorted)
save(tf2sorted, file="4_models/2freq.tf")


## ----------------- similar for 3-Grams
load("3_grams//3sorted.tf")
len3 <-length(tf3sorted)
wordIndex3Gram <- 1:len3
names(wordIndex3Gram) <- names(tf3sorted)
save(wordIndex3Gram, file="4_models/3wordIndex.tf")
## find prior, posterior
allNames3 <- names(tf3sorted)
allNamesCollapsed3 <- paste(allNames3, collapse = " ")
remove(allNames3)
allNamesVector3 <- unlist(strsplit(allNamesCollapsed3, split = " "))
remove(allNamesCollapsed3)

## calculate prior, post
prior3_1 <- allNamesVector3[seq.int(1, len3 * 3, 3)]
prior3_2 <- allNamesVector3[seq.int(2, len3 * 3, 3)]
post3 <- allNamesVector3[seq.int(3, len3 * 3, 3)]
remove(allNamesVector3)

prior3Ix <- wordIndex2Gram[paste(prior3_1, prior3_2, sep = " ")]
post3Ix <- wordIndex1Gram[post3]
prior3Ix <- unname(prior3Ix)
post3Ix <-unname(post3Ix)
save(prior3Ix, file="4_models/3priorIx.tf")
save(post3Ix, file="4_models/3posteriorIx.tf")

tf3sorted <- unname(tf3sorted)
save(tf3sorted, file="4_models/3freq.tf")


## ----------------- similar for 4-Grams
load("3_grams//4sorted.tf")
len4 <-length(tf4sorted)
wordIndex4Gram <- 1:len4
names(wordIndex4Gram) <- names(tf4sorted)
save(wordIndex4Gram, file="4_models/4wordIndex.tf")
## find prior, posterior
allNames4 <- names(tf4sorted)
allNamesCollapsed4 <- paste(allNames4, collapse = " ")
remove(allNames4)
allNamesVector4 <- unlist(strsplit(allNamesCollapsed4, split = " "))
remove(allNamesCollapsed4)

## calculate prior, post
prior4_1 <- allNamesVector4[seq.int(1, len4 * 4, 4)]
prior4_2 <- allNamesVector4[seq.int(2, len4 * 4, 4)]
prior4_3 <- allNamesVector4[seq.int(3, len4 * 4, 4)]
post4 <- allNamesVector4[seq.int(4, len4 * 4, 4)]
remove(allNamesVector4)

prior4Ix <- wordIndex3Gram[paste(prior4_1, prior4_2, prior4_3, sep = " ")]
post4Ix <- wordIndex1Gram[post4]
prior4Ix <- unname(prior4Ix)
post4Ix <-unname(post4Ix)
save(prior4Ix, file="4_models/4priorIx.tf")
save(post4Ix, file="4_models/4posteriorIx.tf")

tf4sorted <- unname(tf4sorted)
save(tf4sorted, file="4_models/4freq.tf")



## ----------------- similar for 5-Grams
load("3_grams//5sorted.tf")
len5 <-length(tf5sorted)
wordIndex5Gram <- 1:len5
names(wordIndex5Gram) <- names(tf5sorted)
save(wordIndex5Gram, file="4_models/5wordIndex.tf")
tf5sorted <- unname(tf5sorted)
save(tf5sorted, file="4_models/5freq.tf")

## find prior, posterior
allNames5 <- names(wordIndex5Gram)

allNamesCollapsed5 <- paste(allNames5, collapse = " ")
remove(allNames5)
allNamesVector5 <- unlist(strsplit(allNamesCollapsed5, split = " "))
remove(allNamesCollapsed5)

## calculate prior, post
prior5_1 <- allNamesVector5[seq.int(1, len5 * 5, 5)]
prior5_2 <- allNamesVector5[seq.int(2, len5 * 5, 5)]
prior5_3 <- allNamesVector5[seq.int(3, len5 * 5, 5)]
prior5_4 <- allNamesVector5[seq.int(4, len5 * 5, 5)]
post5 <- allNamesVector5[seq.int(5, len5 * 5, 5)]
remove(allNamesVector5)

prior5Ix <- wordIndex4Gram[paste(prior5_1, prior5_2, prior5_3, prior5_4, sep = " ")]
post5Ix <- wordIndex1Gram[post5]
prior5Ix <- unname(prior5Ix)
post5Ix <-unname(post5Ix)
save(prior5Ix, file="4_models/5priorIx.tf")
save(post5Ix, file="4_models/5posteriorIx.tf")
