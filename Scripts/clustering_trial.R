#' Clustering score
#'
#' Computes a score, based on the F-statistic, measuring the balance between
#' separation and compactness for a given grouping structure.
#'
#' @param x data with \code{p} variables as columns and \code{n} observations as
#'   rows.
#' @param membership matrix with cluster membership of the \code{n} observations
#'   (one column for )
#' @param verbose logical indicating if the loading bar should be displayed.
#'
#' @return A list with: \item{ESS}{vector of explained sum of squares for each
#'   grouping structure (rows of \code{membership}), i.e. sum of squared
#'   distances between the cluster centroids and the average point.}
#'   \item{RSS}{vector of residual sum of squares for each grouping structure,
#'   i.e. sum of squared distances between the cluster centroids and the
#'   original variables.} \item{f_stat}{vector of F-statistic for each grouping
#'   structure, i.e. ratio of between-group variability over the within-group
#'   variability.} \item{score}{vector of score measuring the balance between
#'   separation and compactness for each grouping structure.} \item{R2}{vector
#'   of proportion of variance explained by the grouping, for each grouping
#'   structure.}
#'
#' @examples
# Data simulation
p <- 50
n <- 500
true_clusters <- c(rep(1, 10), rep(2, 5), rep(3, 4), rep(4, 2), rep(5, 2), rep(6, 2), 7:31)

set.seed(1)
x <- NULL
for (i in 1:max(true_clusters)) {
  xtmp <- rnorm(n)
  x <- cbind(x, xtmp)
  if (sum(true_clusters == i) > 1) {
    for (j in 2:sum(true_clusters == i)) {
      x <- cbind(x, xtmp + rnorm(n, sd = 0.7))
    }
  }
}
colnames(x) <- paste0("obs", 1:ncol(x))
rownames(x) <- paste0("var", 1:nrow(x))
x=t(x)

# k-means clustering
out=kmeans(x, centers=3)

# Computing the clustering score
score=ClusteringScore(x, membership=out$cluster)
#'
#' @export
ClusteringScore <- function(x, membership, verbose = TRUE) {
  if (is.vector(membership)) {
    membership <- matrix(membership, nrow = 1)
    rownames(membership) <- 1:nrow(membership)
  }
  
  # Estimating the central point by individual (always the same)
  ybar <- apply(t(x), 1, mean)
  
  # Extracting the number of variables
  p <- nrow(x)
  n <- ncol(x)
  
  # Creating empty objects
  ESS <- RSS <- f_stat <- score <- rep(NA, nrow(membership))
  names(ESS) <- names(RSS) <- names(f_stat) <- names(score) <- 1:nrow(membership)
  RSS_contrib <- matrix(0, nrow = nrow(membership), ncol = nrow(x))
  
  # Computing the explained sum of squares (ESS)
  ESS_contrib <- rep(NA, nrow(x))
  for (j in 1:nrow(x)) {
    ESS_contrib[j] <- sum((ybar - x[j, ])^2)
  }
  
  # Computing the total sum of squares (TSS) from ESS with one group per feature
  TSS <- sum(ESS_contrib)
  
  # Iterations over the numbers of clusters
  if (verbose) {
    pb <- utils::txtProgressBar(style = 3)
  }
  for (k in 1:nrow(membership)) {
    for (i in 1:length(unique(membership[k, ]))) {
      # Identifying features belonging to the same cluster
      features_to_group <- which(membership[k, ] == unique(membership[k, ])[i])
      
      # Commputing average in the cluster to be used for variance calculations (ESS and RSS)
      cluster_bar <- apply(t(x[features_to_group, , drop = FALSE]), 1, mean)
      
      # Computing the contribution of each feature within the group to the RSS
      for (j in features_to_group) {
        RSS_contrib[k, j] <- sum((cluster_bar - x[j, ])^2)
      }
    }
    RSS[k] <- sum(RSS_contrib[k, ])
    
    # Updating the ESS
    ESS[k] <- TSS - RSS[k]
    
    if (verbose) {
      utils::setTxtProgressBar(pb, k/nrow(membership))
    }
  }
  
  # Computing the F-statistic
  for (k in 1:length(ESS)) {
    i <- length(unique(membership[k, ])) # number of clusters
    f_stat[k] <- (ESS[k] / (i - 1)) / (RSS[k] / (p - i))
  }
  
  # Computing the score to be maximised from probability under the null (uniform grouping)
  for (k in 1:length(ESS)) {
    i <- length(unique(membership[k, ])) # number of clusters
    score[k] <- -stats::pf(f_stat[k], df1 = n * (i - 1), df2 = n * (p - i), lower.tail = FALSE, log.p = TRUE)
  }
  score[is.infinite(score)] <- NA
  
  # Coefficient of determination
  R2 <- ESS / TSS
  
  out <- list(
    RSS_contrib = RSS_contrib,
    ESS = ESS, RSS = RSS, TSS = TSS,
    f_stat = f_stat,
    score = score,
    R2 = R2
  )
  
  return(out)
}


#' Iterative grouping of features
#'
#' This function can be used to identify groups of variables (given as columns
#' of \code{x}) that can be grouped together and summarised as a single variable
#' without loosing too much information (as measured by the explained variance
#' within the group). This is done using an iterative algorithm that is (i)
#' grouping the closest pair of features together (clustering step), and (ii)
#' combining these two features in a single feature (summary step). At each
#' iteration, the new variable created in the summary step is defined as a
#' linear combination of all variables that are grouped together in the newly
#' formed cluster. The number of summarised groups can be calibrated to optimise
#' the balance between compactness (i.e. small within-cluster variability) and
#' separation (i.e. large between-cluster variability) by maximising a score
#' based on the F-statistic.
#'
#' @param x data with p variables as columns and n observations as rows.
#' @param summary method for summary of the cluster as a single variable. If
#'   \code{summary="centroid"}, the cluster is summarised by extracting the scores from
#'   the first Principal Component of a Principal Component Analysis applied on
#'   the original variables combined in the newly formed cluster. If
#'   \code{summary="medoid"}, the variable most contributing to the first Principal
#'   Component is used. If \code{summary=NULL}, the summary step is skipped (the
#'   clustering tree is obtained from classical hierarchical clustering).
#' @param distance distance metric to use for the identification of the closest
#'   pair of features. By default (\code{distance="correlation"}), the closest pair of
#'   features is identified as the one with highest Pearson's correlation.
#'   Alternatively, with \code{distance="euclidian"}, the closest pair of features is
#'   defined as the one with smallest Euclidian distance. When \code{scale=TRUE} (all
#'   variables have a mean of zero and s.d. of one), these two methods are
#'   equivalent.
#' @param scale logical indicating if the variables should be scaled (i.e.
#'   transformed so that the mean is zero and standard deviation is one). By
#'   default, we use \code{scale=TRUE} (recommended).
#' @param agglomeration_method agglomeration method in hierarchical clustering.
#'   This argument is only used if \code{summary=NULL} and set to
#'   \code{agglomeration_method="centroid"} by default. See \code{method} argument of the
#'   \code{\link[stats]{hclust}} for other options.
#' @param verbose logical indicating if the loading bar should be displayed.
#'
#' @return A list with: \item{membership}{a (pxp) matrix describing cluster
#'   membership of the p variables (columns) across p clustering iterations,
#'   i.e. p possible numbers of clusters.} \item{merging_value}{vector of length
#'   p with the correlation (if \code{distance="correlation"}) or distance
#'   between features grouped at corresponding iteration. Only available if
#'   \code{summary} is not NULL.} \item{ESS}{vector of length p with the
#'   explained sum of squares for each of the p clustering iterations, i.e. sum
#'   of squared distances between the cluster centroids and the average point.}
#'   \item{RSS}{vector of length p with the residual sum of squares for each of
#'   the p clustering iterations, i.e. sum over all clusters of the sum of
#'   squared distances between the cluster centroids and the original
#'   variables.} \item{f_stat}{vector of length p with the F-statistic for each
#'   of the p clustering iterations, i.e. ratio of the between-group variability
#'   over the within-group variability.} \item{pvalue}{vector of length p with
#'   the p-value from the F-test measuring the balance between separation and
#'   compactness of the grouping for each of the p clustering iterations.}
#'   \item{score}{vector of length p with the score measuring the balance
#'   between separation and compactness of the grouping for each of the p
#'   clustering iterations.} \item{R2}{vector of length p with the percentage of
#'   explained variance by the grouping for each of the p clustering
#'   iterations.} \item{params}{list of parameters used for the run.}
#'
#' @examples
#' # Data simulation
p <- 50
n <- 500
true_clusters <- c(rep(1, 10), rep(2, 5), rep(3, 4), rep(4, 2), rep(5, 2), rep(6, 2), 7:31)

set.seed(1)
x <- NULL
for (i in 1:max(true_clusters)) {
  xtmp <- rnorm(n)
  x <- cbind(x, xtmp)
  if (sum(true_clusters == i) > 1) {
    for (j in 2:sum(true_clusters == i)) {
      x <- cbind(x, xtmp + rnorm(n, sd = 0.7))
    }
  }
}
colnames(x) <- paste0("var", 1:ncol(x))
rownames(x) <- paste0("obs", 1:nrow(x))

# Using PC1-scores as centroid
out <- VariableGrouping(x)
plot(out$score)
hat_k <- which.max(out$score)

# Using variable most contributing to PC1 as medoid
out <- VariableGrouping(x, summary = "medoid")
plot(out$score)
hat_k <- which.max(out$score)
#' @export
VariableGrouping <- function(x, summary = "centroid", distance = "correlation",
                             scale = TRUE, agglomeration_method = "centroid", verbose = TRUE) {
  # Checking inputs
  if (is.null(summary)) {
    if (distance == "correlation") {
      warning("Arguments 'summary' and 'distance' are not compatible. The 'distance' was set to 'euclidian'.")
      distance <- "euclidian"
    }
  }
  
  # Scaling the variables to ensure comparability
  if (scale) {
    x <- scale(x)
  }
  
  # Transposing the data to perform clustering on the variables
  x <- t(x)
  
  # Initialisation of cluster membership matrix
  membership <- matrix(NA, nrow = nrow(x), ncol = nrow(x))
  rownames(membership) <- paste0(1:nrow(membership))
  colnames(membership) <- rownames(x)
  
  # Estimating the central point (always the same)
  ybar <- apply(t(x), 1, mean)
  
  # Initialisation of the clustering (starting with 1 cluster)
  membership[1, ] <- 1
  membership[nrow(x), ] <- 1:nrow(x)
  
  # Initialisation of current version of the dataset at iteration
  xtmp <- x
  if (distance == "correlation") {
    mydistance <- -stats::cor(t(xtmp)) # grouping only positive correlations
    mydistance[upper.tri(mydistance, diag = TRUE)] <- NA
  } else {
    mydistance <- matrix(NA, nrow = nrow(xtmp), ncol = nrow(xtmp))
    mydistance[lower.tri(mydistance)] <- as.numeric(stats::dist(xtmp, method = distance))
  }
  
  # Extracting the number of variables
  p <- nrow(x)
  n <- ncol(x)
  
  # Creating empty objects
  ESS <- RSS <- merging_value <- f_stat <- pvalue <- rep(NA, nrow(x))
  names(ESS) <- names(RSS) <- names(f_stat) <- names(pvalue) <- 1:nrow(x)
  RSS_contrib <- matrix(0, nrow = nrow(x), ncol = nrow(x))
  
  # Computing the explained sum of squares (ESS)
  ESS_contrib <- rep(NA, nrow(x))
  for (j in 1:nrow(x)) {
    ESS_contrib[j] <- sum((ybar - x[j, ])^2)
  }
  
  # Computing the total sum of squares (TSS) from ESS with one group per feature
  ESS[nrow(x)] <- sum(ESS_contrib)
  TSS <- ESS[nrow(x)]
  
  # Filling the residual sum of squares (RSS)
  RSS[nrow(x)] <- 0
  
  # Hierarchical clustering (without summary of the clusters at each iteration)
  if (is.null(summary)) {
    myh <- stats::hclust(stats::dist(x, method = distance), method = agglomeration_method)
    for (k in 2:nrow(x)) {
      membership[k, ] <- stats::cutree(myh, k = k)
    }
  }
  
  # Iterations over the numbers of clusters
  if (verbose) {
    pb <- utils::txtProgressBar(style = 3)
  }
  for (k in nrow(x):2) {
    if (!is.null(summary)) {
      # Identification of clusters to merge:
      # smallest distance between centroids (as defined by PC1s)
      # or medoids (as defined by most contributing variable)
      
      # Measuring distance between clusters at current iteration
      if (distance == "correlation") {
        merging_value[k] <- -min(mydistance, na.rm = TRUE)
      } else {
        merging_value[k] <- min(mydistance, na.rm = TRUE)
      }
      clusters_to_merge <- which(mydistance == min(mydistance, na.rm = TRUE), arr.ind = TRUE)[1, ]
      features_to_group <- which(membership[k, ] %in% clusters_to_merge)
      
      # Re-attributing cluster IDs at current iteration
      for (i in 1:ncol(membership)) {
        if (membership[k, i] < min(clusters_to_merge)) {
          membership[k - 1, i] <- membership[k, i]
        }
        if ((membership[k, i] > min(clusters_to_merge)) & (membership[k, i] < max(clusters_to_merge))) {
          membership[k - 1, i] <- membership[k, i] - 1
        }
        if (membership[k, i] > max(clusters_to_merge)) {
          membership[k - 1, i] <- membership[k, i] - 2
        }
      }
      membership[k - 1, features_to_group] <- max(membership[k - 1, ], na.rm = TRUE) + 1
    } else {
      # Simply following structure of classical hierarchical clustering if no summary of the clusters
      merged_cluster <- which(apply(table(membership[k, ], membership[k - 1, ]), 2, FUN = function(m) {
        sum(m != 0) == 2
      }))
      clusters_to_merge <- which(table(membership[k, ], membership[k - 1, ])[, merged_cluster] != 0)
      features_to_group <- which(membership[k, ] %in% clusters_to_merge)
    }
    
    # Calculating the centroid
    if (!is.null(summary)) {
      # Summary of the new cluster formed at current iteration
      xcluster <- x[features_to_group, , drop = FALSE]
      mypca <- FactoMineR::PCA(t(xcluster), graph = TRUE, ncp = 1)
      
      # Removing previous clusters now combined for future iterations
      xtmp <- xtmp[-clusters_to_merge, ]
      mydistance <- mydistance[-clusters_to_merge, -clusters_to_merge]
      
      # Classical centroid (average levels over grouped variables)
      centroid <- apply(t(xcluster), 1, mean)
      
      # Weighted average: scaled PC1-scores
      if (summary == "centroid") {
        if ((sum(mypca$var$coord > 0) / length(mypca$var$coord)) > 0.5) {
          centroid_bar <- scale(mypca$ind$coord)[, 1]
        } else {
          centroid_bar <- -scale(mypca$ind$coord)[, 1]
        }
      }
      
      # Choosing the medoid (actual data point) as most contributing to the variability
      if (summary == "medoid") {
      }
      
      xtmp <- rbind(xtmp, centroid_bar)
      
      if (distance == "correlation") {
        tmpdistance <- as.vector(-stats::cor(t(xtmp), centroid_bar)) # grouping only positive correlations
        tmpdistance[length(tmpdistance)] <- NA
      }
      if (distance == "euclidian") {
        tmpdistance <- rep(NA, ncol(xtmp))
        for (k in 1:(ncol(xtmp) - 1)) {
          tmpdistance[k] <- sqrt(sum((xtmp[k, ] - xtmp[ncol(xtmp), ])^2))
        }
      }
      mydistance <- rbind(mydistance, tmpdistance[-length(tmpdistance)])
      mydistance <- cbind(mydistance, tmpdistance)
    }
    
    # Commputing average in the cluster to be used for variance calculations (ESS and RSS)
    cluster_bar <- apply(t(x[features_to_group, , drop = FALSE]), 1, mean)
    
    # Updating the contributions of grouped features to the RSS
    RSS_contrib[k - 1, ] <- RSS_contrib[k, ]
    for (j in features_to_group) {
      RSS_contrib[k - 1, j] <- sum((cluster_bar - x[j, ])^2)
    }
    RSS[k - 1] <- sum(RSS_contrib[k - 1, ])
    
    # Updating the ESS
    ESS[k - 1] <- TSS - RSS[k - 1]
    
    if (verbose) {
      utils::setTxtProgressBar(pb, (nrow(x) - k + 1) / (nrow(x) - 1))
    }
  }
  membership[1, ] <- 1
  
  # Computing the F-statistic
  for (i in 2:(nrow(x) - 1)) {
    f_stat[i] <- (ESS[i] / (i - 1)) / (RSS[i] / (p - i))
  }
  
  # F-test: probability under the null (uniform grouping)
  for (i in 2:(nrow(x) - 1)) {
    pvalue[i] <- stats::pf(f_stat[i], df1 = i - 1, df2 = p - i, lower.tail = FALSE)
  }
  
  # Computing the score to be maximised
  score <- -log(pvalue)
  score[is.infinite(score)] <- NA
  
  # Coefficient of determination
  R2 <- ESS / TSS
  
  if (is.null(summary)) {
    out <- list(
      membership = membership,
      RSS_contrib = RSS_contrib,
      ESS = ESS, RSS = RSS, TSS = TSS,
      f_stat = f_stat, pvalue = pvalue,
      score = score, R2 = R2,
      params = list(
        summary = summary, distance = distance,
        scale = scale, agglomeration_method = agglomeration_method
      )
    )
  } else {
    out <- list(
      membership = membership,
      merging_value = merging_value,
      RSS_contrib = RSS_contrib,
      ESS = ESS, RSS = RSS, TSS = TSS,
      f_stat = f_stat, pvalue = pvalue,
      score = score, R2 = R2,
      params = list(
        summary = summary, distance = distance,
        scale = scale, agglomeration_method = agglomeration_method
      )
    )
  }
  return(out)
}


#' Summarising groups of features
#'
#' This function can be used to summarise groups of features as a single
#' variable. The grouping structure can be inferred from the data using the
#' function VariableGrouping().
#'
#' @inheritParams VariableGrouping
#' @param x data with p variables as columns and n observations as rows.
#' @param group vector of length p with the group membership for each of the p
#'   variables.
#' @param summary method for summary of the cluster as a single variable. If
#'   \code{summary="centroid"}, the cluster is summarised by extracting the
#'   scores from the first Principal Component of a Principal Component Analysis
#'   applied on the original variables combined in the newly formed cluster. If
#'   \code{summary="medoid"}, the variable most contributing to the first
#'   Principal Component is used.
#'
#' @return A list with: \item{xsummarised}{summarised data with n observations
#'   and one variable per group.} \item{description}{a table with one row per
#'   original variable in the data. The variable name (feature), corresponding
#'   group (group), range in pairwise correlations within the group (min_cor and
#'   max_cor), proportion of explained variance (ev), loadings coefficients
#'   (loadings) and proportion of contribution (contribution) of the original
#'   variables along the first Principal Component of a Principal Component
#'   Analysis on the corresponding group are reported.}
#'
#' @examples
#' # Data simulation
# p <- 50
# n <- 500
# true_clusters <- c(rep(1, 10), rep(2, 5), rep(3, 4), rep(4, 2), rep(5, 2), rep(6, 2), 7:31)
# 
# set.seed(1)
# x <- NULL
# for (i in 1:max(true_clusters)) {
#   xtmp <- rnorm(n)
#   x <- cbind(x, xtmp)
#   if (sum(true_clusters == i) > 1) {
#     for (j in 2:sum(true_clusters == i)) {
#       x <- cbind(x, xtmp + rnorm(n, sd = 0.7))
#     }
#   }
# }
# colnames(x) <- paste0("var", 1:ncol(x))
# rownames(x) <- paste0("obs", 1:nrow(x))
# 
# # Estimation of grouping structure
out <- VariableGrouping(x)
hat_k <- which.max(out$score)
grouping <- out$membership[hat_k, ]
# 
# # Summary of the groups
mysummary <- GroupingSummary(x, group = grouping)
xsummarised <- mysummary$xsummarised
#' @export
GroupingSummary <- function(x, group, summary = "centroid", verbose = TRUE) {
  # Getting data transpose
  x <- t(x)
  
  # Number of group
  k <- max(group)
  
  # Initialisation of the objects
  xsummarised <- matrix(NA, nrow = k, ncol = ncol(x))
  myrownames <- rep(NA, length = k)
  mytable <- data.frame(
    feature = names(group), group = unname(group),
    min_cor = rep(1, length(group)),
    max_cor = rep(1, length(group)),
    ev = rep(NA, length(group)),
    loadings = rep(NA, length(group)),
    contribution = rep(NA, length(group))
  )
  rownames(mytable) <- mytable[, 1]
  
  # Loop over the groups
  if (verbose) {
    pb <- utils::txtProgressBar(style = 3)
  }
  for (i in 1:k) {
    # Extracting data in the group
    xcluster <- x[names(which(group == i)), , drop = FALSE]
    
    # PCA on the group
    mypca <- FactoMineR::PCA(t(xcluster), graph = FALSE, ncp = 1)
    
    # Proportion of explained variance by PC1
    mytable[mytable$group == i, "ev"] <- mypca$eig[1, 2] / 100
    
    # Loadings coefficients on PC1
    mytable[rownames(xcluster), "loadings"] <- mypca$var$coord / sqrt(mypca$eig[1, 1])
    
    # Contribution of original variables to PC1
    mytable[rownames(xcluster), "contribution"] <- mypca$var$contrib / 100
    
    # Range in pairwise correlations
    if (nrow(xcluster) > 1) {
      mycor <- stats::cor(t(xcluster))
      mycor[upper.tri(mycor, diag = TRUE)] <- NA
      mytable[rownames(xcluster), "min_cor"] <- min(mycor, na.rm = TRUE)
      mytable[rownames(xcluster), "max_cor"] <- max(mycor, na.rm = TRUE)
    }
    
    # Using PC1-scores to summarise the group
    if (summary == "centroid") {
      if ((sum(mypca$var$coord > 0) / length(mypca$var$coord)) > 0.5) {
        xsummarised[i, ] <- mypca$ind$coord[, 1]
      } else {
        xsummarised[i, ] <- -mypca$ind$coord[, 1]
      }
    }
    
    # Using the variable most contributing to PC1-scores for summary
    if (summary == "medoid") {
      xsummarised[i, ] <- xcluster[which.max(abs(mypca$var$coord)), ]
      myrownames[i] <- rownames(xcluster)[which.max(abs(mypca$var$coord))]
    }
    
    if (verbose) {
      utils::setTxtProgressBar(pb, i / k)
    }
  }
  
  # Re-ordering the description table (multivariate groups first)
  mytable <- mytable[sort.list(mytable$group, decreasing = TRUE), ]
  
  # Returning the data with summarised features as columns
  colnames(xsummarised) <- colnames(x)
  if (summary == "centroid") {
    rownames(xsummarised) <- paste0("F", 1:k)
  } else {
    rownames(xsummarised) <- myrownames
  }
  xsummarised <- t(xsummarised)
  xsummarised <- scale(xsummarised)
  
  return(list(xsummarised = xsummarised, description = mytable))
}

# explained variance 


