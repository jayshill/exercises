####################################################################
#   RTI CDS Analytics Exercise 02                                  #
#   NTSB database of aviation accidents                            #
#   Jay Hill                                                       #
#                                                                  #
#   This script performs text analyses on the narrative and        #
#   probable cause descriptions. Term-document matrices are        #
#   created, weighted by term frequency - inverse document         #
#   frequency, and transformed using latent semantic analysis.     #
#   The descriptions are then clustered into groups that are       #
#   different from each other but have internal similarities.      #
####################################################################

library(tm)
library(magrittr)
library(stringi)
library(svdvis)
library(cluster)


##### Clean up the text and create term-document matrices #####
# This function creates clean, single-word tokens.
# Punctuation is removed, chaged to all lower-case, extra white 
# space is removed, and common stop words are removed.
cleaner <- function(txt) {
    packs <- c("tm", "magrittr", "stringi")
    sapply(packs, require, character.only = T)
    
    output <- txt %>%
        stri_trans_tolower %>%
        removeWords(stopwords(kind = "en")) %>%
        stri_replace_all_fixed("--", " ") %>%
        removePunctuation %>%
        stripWhitespace %>%
        stri_trim_both
        
    return(output)
}

narratives$narrative <- cleaner(narratives$narrative)
narratives$probable_cause <- cleaner(narratives$probable_cause)

# Separate the narratives and probable causes into 2 data frames.
probable_causes <- narratives
probable_causes$narrative <- NULL
narratives$probable_cause <- NULL
# remove rows with no narrative or probable cause
narratives <- narratives[stri_length(narratives$narrative) > 0, ]
probable_causes <- probable_causes[stri_length(probable_causes$probable_cause)
                                   > 0, ]

# Create term-document matrices for the narratives and probable causes.
# Weight the TDMs using term frequency - inverse document frequency.
# Remove sparse terms.
TDMer <- function(txt, sparsity) {
    packs <- c("tm", "magrittr")
    sapply(packs, require, character.only = T)
    
    output <- VectorSource(txt) %>% 
        VCorpus %>% 
        TermDocumentMatrix %>%
        weightTfIdf %>%
        removeSparseTerms(sparse = sparsity)
    return(output)
}

narrativeTDM <- TDMer(narratives$narrative, 0.90)  # 97 terms remain
probcauseTDM <- TDMer(probable_causes$probable_cause, 0.98)  # 108 terms remain


##### weight the term-document matrices #####
# Use singular value decompositions to perform latent semantic analyses
narrativeMatrix <- as.matrix(narrativeTDM)
dimnames(narrativeMatrix)[[2]] <- narratives$EventId
narrativeSVD <- svd(narrativeMatrix)
svd.scree(narrativeSVD, subr = 50)  # elbow is around 9 singular values
singvals <- 9
U <- narrativeSVD$u[, 1:singvals]
D <- diag(narrativeSVD$d[1:singvals])
Vt <- t(narrativeSVD$v[, 1:singvals])
prin1nar <- Vt[1, ]
prin2nar <- Vt[2, ]
narrativeLSA <- U %*% D %*% Vt
dimnames(narrativeLSA) <- dimnames(narrativeMatrix)

probcauseMatrix <- as.matrix(probcauseTDM)
dimnames(probcauseMatrix)[[2]] <- probable_causes$EventId
probcauseSVD <- svd(probcauseMatrix)
svd.scree(probcauseSVD, subr = 50)  # elbow is around 10 singular values
singvals <- 10
U <- probcauseSVD$u[, 1:singvals]
D <- diag(probcauseSVD$d[1:singvals])
Vt <- t(probcauseSVD$v[, 1:singvals])
prin1prob <- Vt[1, ]
prin2prob <- Vt[2, ]
probcauseLSA <- U %*% D %*% Vt
dimnames(probcauseLSA) <- dimnames(probcauseMatrix)


##### Hierarchical clustering #####
# Use hierarchical clustering on a sample of the data to find
# possible values for the number of clusters.
hierclust <- function(LSA) {
    set.seed(5678)
    nobs <- ncol(LSA)
    testSample <- sample(x = 1:nobs, size = 1000)
    test <- t(LSA[, testSample])
    output <- agnes(test, diss = F, method = "ward")
    return(output)
}
narAGNES <- hierclust(narrativeLSA)
plot(narAGNES)
    # from the tree, it looks like 3, 5, and 6 are good candidates for k
probcauseAGNES <- hierclust(probcauseLSA)
plot(probcauseAGNES)
    # from the tree, it looks like 2, 4, and 5 are good candidates for k


##### Non-hierarchical clustering #####
# Cluster using the CLARA method
set.seed(1234)
narrative3Clusters <- clara(t(narrativeLSA), k = 3, metric = "euclidean",
                           samples = 50, sampsize = 1000, rngR = T)

# plot as scores along first two principal components
plot(prin1nar, prin2nar, col=narrative3Clusters$cluster)
narrative3Clusters$clusinfo
    # The 3 cluster solution has the least amount of overlap,
    # and the isolation scores are all similar and range from 3.4 to 5.2.

set.seed(1234)
probcause4Clusters <- clara(t(probcauseLSA), k = 4, metric = "euclidean",
                            samples = 50, sampsize = 1000, rngR = T)

# plot as scores along first two principal components
plot(prin1prob, prin2prob, col=probcause4Clusters$cluster)
probcause4Clusters$clusinfo
# The 4 cluster solution has the least amount of overlap,
# and the isolation scores are all similar and range from 3.7 to 5.9.


##### interpret the clusters #####
narClusLSA <- as.data.frame(cbind(t(narrativeLSA), 
                                  cluster=narrative3Clusters$clustering))
narTermMeans <- t(aggregate(narClusLSA, by=list(narClusLSA$cluster), FUN=mean))
narClus1Terms <- sort(narTermMeans[, 1], decreasing = T)
narClus2Terms <- sort(narTermMeans[, 2], decreasing = T)
narClus3Terms <- sort(narTermMeans[, 3], decreasing = T)
names(narClus1Terms)[3:22]
names(narClus2Terms)[3:22]
names(narClus3Terms)[3:22]
    # Narrative Clusters:
    # 1: Mechanical issues at the airport or during takeoff/landing. (runway, 
    #    airport; landing, takeoff, approach; engine, gear, wing, nose)
    # 2: Wind or collision related during takeoff/landing (takeoff, landing; 
    #    gear, nose; collided; wind)
    # 3: Loss of fuel or power during flight (fuel, engine, power; loss, lost, 
    #    found; flight)

probClusLSA <- as.data.frame(cbind(t(probcauseLSA), 
                                   cluster=probcause4Clusters$clustering))
probTermMeans <- t(aggregate(probClusLSA, by=list(probClusLSA$cluster), 
                             FUN=mean))
probClus1Terms <- sort(probTermMeans[, 1], decreasing = T)
probClus2Terms <- sort(probTermMeans[, 2], decreasing = T)
probClus3Terms <- sort(probTermMeans[, 3], decreasing = T)
probClus4Terms <- sort(probTermMeans[, 4], decreasing = T)
names(probClus1Terms)[3:22]
names(probClus2Terms)[3:22]
names(probClus3Terms)[3:22]
names(probClus4Terms)[3:22]
    # Probable Cause clusters:
    # 1: Collision or accident during landing maybe due to pilot error (landing; 
    #    pilot; improper, failure, inadequate; clearance, altitude; collision, 
    #    accident)
    # 2: Human factors and mechanical issues (exhaustion and starvation; 
    #    inadequate and improper; fuel, engine, power)
    # 3: Wind during takeoff/landing, possible student pilot issues (Wind; 
    #    takeoff and landing; student pilot)
    # 4: Undetermined reasons; loss of engine power; terrain; unsuitable 


##### Compare narrative and probable cause clusters #####
narrativeFinal <- data.frame(EventID = colnames(narrativeLSA),
                             NarrativeCluster=narrative3Clusters$clustering)
rownames(narrativeFinal) <- NULL
probable_causeFinal <- data.frame(EventId = colnames(probcauseLSA),
                          ProbableCauseCluster=probcause4Clusters$clustering)
rownames(probable_causeFinal) <- NULL
textFinal <- merge(x = narrativeFinal, y = probable_causeFinal, all = T)

table(textFinal$NarrativeCluster, 
      textFinal$ProbableCauseCluster, 
      useNA = "ifany")
    # Narrative factor 1 and probable cause factor 1 have good agreeement;
    # problems during takeoff/landing.
    # Narrative 1 and probable cause 3: takeoff/landing; student pilot?
    # Narrative 2 and probable cause 1: collsion during takeoff/landing
    # Narrative 3 and probable cause 2: problems during flight away from the
    # airport.
