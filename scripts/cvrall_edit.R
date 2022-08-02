function (clustername = NULL, x, categorical = NULL, weights = NULL, 
          ntotal_cluster, ntrt_cluster, cutoff = 0.1, numschemes = NULL, 
          size = 50000, stratify = NULL, seed = NULL, balancemetric = "l2", 
          nosim = FALSE, savedata = NULL, bhist = TRUE, check_validity = FALSE, 
          samearmhi = 0.75, samearmlo = 0.25) 
{
  if (!is.null(seed)) {
    set.seed(seed)
  }
  else {
    set.seed(12345)
  }
  if (is.null(clustername)) {
    clustername <- 1:dim(x)[1]
  }
  nsub <- length(clustername)
  if (ntotal_cluster%%1 != 0) {
    stop("Error: number of clusters specified must be an integer.")
  }
  if (ntotal_cluster <= 0) {
    stop("Error: number of clusters specified must be a positive number.")
  }
  if (ntrt_cluster%%1 != 0) {
    stop("Error: number of treatment clusters specified must be an integer.")
  }
  if (ntrt_cluster <= 0) {
    stop("Error: number of treatment clusters specified must be a positive number.")
  }
  if (nsub != dim(x)[1]) {
    stop("Error: the clustername vector and the covariates data frame must match in the number of clusters")
  }
  if (nsub != ntotal_cluster) {
    stop("Error: number of clusters specified must equal the number of rows in the data set.")
  }
  if (ntotal_cluster <= ntrt_cluster) {
    stop("Error: number of clusters in the treatment arm must be smaller than the total number of clusters!")
  }
  if (cutoff <= 0 | cutoff >= 1) {
    stop("Error: cutoff must be greater than 0 and smaller than 1")
  }
  if ((!is.null(stratify))) {
    if (is.null(categorical)) {
      stop("Error: categorical must be specified when the stratify option is specified")
    }
    else if (!all(stratify %in% categorical)) {
      stop("Error: the categorical variables for stratification must be a subset of the categorical option")
    }
  }
  if (!is.null(stratify)) {
    weights <- rep(1, dim(x)[2])
    if (is.character(stratify)) {
      weights[colnames(x) %in% stratify] <- 1000
    }
    else if (is.numeric(stratify)) {
      weights[x] <- 1000
    }
  }
  data <- x
  if (is.null(categorical)) {
    if (sum(apply(x, 2, function(x) length(unique(x[!is.na(x)]))) <= 
            1) >= 1) {
      warning(cat("Warning: each of columns", c(1:dim(x)[2])[apply(x, 
                                                                   2, function(x) length(unique(x[!is.na(x)]))) <= 
                                                               1], "has only less than or equal to one unique value and no variation."))
    }
    if (!setequal(c(1:dim(x)[2])[apply(x, 2, function(x) length(unique(x[!is.na(x)]))) <= 
                                 4], categorical)) {
      warning(cat("Warning: each of columns", c(1:dim(x)[2])[apply(x, 
                                                                   2, function(x) length(unique(x))) <= 4], "has less than or equal to 4 unique observations. Please check all the categorical variables have been correctly specified."))
    }
  }
  else {
    p <- length(categorical)
    if (is.character(categorical)) {
      if (sum(apply(x, 2, function(x) length(unique(x[!is.na(x)]))) <= 
              1) >= 1) {
        warning(cat("Warning: each of", colnames(x)[apply(x, 
                                                          2, function(x) length(unique(x[!is.na(x)]))) <= 
                                                      1], "has only less than or equal to one unique value and no variation."))
      }
      if (!setequal(colnames(x)[apply(x, 2, function(x) length(unique(x[!is.na(x)]))) <= 
                                4], categorical)) {
        warning(cat("Warning: each of", setdiff(colnames(x)[apply(x, 
                                                                  2, function(x) length(unique(x))) <= 4], categorical), 
                    "has less than or equal to 4 unique observations. Please check all the categorical variables have been correctly specified."))
      }
      if (!is.null(weights)) {
        for (i in 1:p) {
          x <- data.frame(x, model.matrix(~factor(x[, 
                                                    categorical[i]]))[, -1])
          weights <- c(weights, rep(weights[which(categorical[i] == 
                                                    colnames(x))], dim(model.matrix(~factor(x[, 
                                                                                              categorical[i]])))[2] - 1))
        }
        weights <- weights[-which(colnames(x) %in% categorical)]
      }
      else {
        for (i in 1:p) {
          x <- data.frame(x, model.matrix(~factor(x[, 
                                                    categorical[i]]))[, -1])
        }
      }
      x <- x[, -which(colnames(x) %in% categorical)]
    }
    else if (is.numeric(categorical)) {
      if (sum(apply(x, 2, function(x) length(unique(x[!is.na(x)]))) <= 
              1) >= 1) {
        warning(cat("Warning: each of columns", 
                    c(1:dim(x)[2])[apply(x, 2, function(x) length(unique(x[!is.na(x)]))) <= 
                                     1], "has only less than or equal to one unique value and no variation."))
      }
      if (!setequal(c(1:dim(x)[2])[apply(x, 2, function(x) length(unique(x[!is.na(x)]))) <= 
                                   4], categorical)) {
        warning(cat("Warning: each of columns", 
                    setdiff(c(1:dim(x)[2])[apply(x, 2, function(x) length(unique(x))) <= 
                                             4], categorical), "has less than or equal to 4 unique observations. Please check all the categorical variables have been correctly specified."))
      }
      if (!is.null(weights)) {
        for (i in 1:p) {
          x <- data.frame(x, model.matrix(~factor(x[, 
                                                    categorical[i]]))[, -1])
          weights <- c(weights, rep(weights[categorical[i]], 
                                    dim(model.matrix(~factor(x[, categorical[i]])))[2] - 
                                      1))
        }
        weights <- weights[-categorical]
      }
      else {
        for (i in 1:p) {
          x <- data.frame(x, model.matrix(~factor(x[, 
                                                    categorical[i]]))[, -1])
        }
      }
      x <- x[, -categorical]
    }
  }
  x <- as.matrix(x)
  np <- dim(x)[2]
  id = clustername
  n = ntotal_cluster
  ntrt = ntrt_cluster
  if (choose(nsub, ntrt_cluster) <= size | nosim == TRUE) {
    sim <- 0
    emr <- t(combn(nsub, ntrt_cluster))
    R <- dim(emr)[1]
    pmt <- matrix(NA, R, nsub)
    S <- R
    for (r in 1:R) {
      pmt[r, emr[r, ]] <- 1
      pmt[r, -emr[r, ]] <- 0
    }
    if (is.null(weights)) {
      if (balancemetric == "l1") {
        BL <- apply(abs((pmt %*% ((x - matrix(apply(x, 
                                                    2, mean), nsub, np, byrow = TRUE))/matrix(apply(x, 
                                                                                                    2, sd), nsub, np, byrow = TRUE)))), 1, sum)
      }
      else if (balancemetric == "l2") {
        BL <- apply(((pmt %*% ((x - matrix(apply(x, 2, 
                                                 mean), nsub, np, byrow = TRUE))/matrix(apply(x, 
                                                                                              2, sd), nsub, np, byrow = TRUE))))^2, 1, sum)
      }
    }
    else {
      if (balancemetric == "l1") {
        BL <- apply(abs((pmt %*% ((x - matrix(apply(x, 
                                                    2, mean), nsub, np, byrow = TRUE)) * matrix(weights, 
                                                                                                nsub, np, byrow = TRUE)/matrix(apply(x, 2, 
                                                                                                                                     sd), nsub, np, byrow = TRUE)))), 1, sum)
      }
      else if (balancemetric == "l2") {
        BL <- apply(((pmt %*% ((x - matrix(apply(x, 2, 
                                                 mean), nsub, np, byrow = TRUE)) * matrix(weights, 
                                                                                          nsub, np, byrow = TRUE)/matrix(apply(x, 2, 
                                                                                                                               sd), nsub, np, byrow = TRUE))))^2, 1, sum)
      }
    }
    if (is.null(numschemes)) {
      cho <- round(R * cutoff)
      subid <- order(BL)[1:cho]
      BLcut <- BL[order(BL)[cho]]
      if (cho <= 1) {
        warning("Warning: less than or equal to 1 scheme meets the constraints for the constrained space")
        BLcut = NaN
      }
      rw <- sample(subid, 1)
      BLchoice <- BL[rw]
      inter <- pmt[rw, ]
    }
    else {
      if (numschemes >= R) {
        stop("Error: the fixed number of schemes in the constrained space for constrained randomization must be smaller or equal to the total number of randomization schemes.")
      }
      cho <- numschemes
      subid <- order(BL)[1:cho]
      BLcut <- BL[order(BL)[cho]]
      if (cho <= 1) {
        warning("Warning: less than or equal to 1 scheme meets the constraints for the constrained space")
        BLcut = NaN
      }
      rw <- sample(subid, 1)
      BLchoice <- BL[rw]
      inter <- pmt[rw, ]
    }
    qmt <- pmt[subid, , drop = FALSE]
    R_result <- dim(qmt)[1]
    summary_constraints <- as.data.frame(cbind(S, R, R_result, 
                                               paste0(round(R_result/R, 4) * 100, "%")))
    colnames(summary_constraints) <- c("overall allocations", 
                                       "checked allocations", "accepted allocations", 
                                       "overall % acceptable")
    BL_Quantiles <- quantile(BL, prob = c(0, 0.05, 0.1, 0.2, 
                                          0.25, 0.3, 0.5, 0.75, 0.95, 1))
    BL_Quantiles <- c(BLchoice, BLcut, mean(BL), sd(BL), 
                      BL_Quantiles)
    BL_Quantiles <- round(BL_Quantiles, 3)
    names(BL_Quantiles)[c(1, 2, 3, 4, 5, 14)] <- c("score (selected scheme)", 
                                                   "cutoff score", "Mean", "SD", "Min", 
                                                   "Max")
  }
  else {
    sim <- 1
    S <- size
    pmt <- matrix(NA, S, nsub)
    for (s in 1:S) {
      trt <- sample(1:nsub, ntrt_cluster)
      pmt[s, trt] <- 1
      pmt[s, -trt] <- 0
    }
    pmt <- unique(pmt)
    R <- dim(pmt)[1]
    if (is.null(weights)) {
      if (balancemetric == "l1") {
        BL <- apply(abs((pmt %*% ((x - matrix(apply(x, 
                                                    2, mean), nsub, np, byrow = TRUE))/matrix(apply(x, 
                                                                                                    2, sd), nsub, np, byrow = TRUE)))), 1, sum)
      }
      else if (balancemetric == "l2") {
        BL <- apply(((pmt %*% ((x - matrix(apply(x, 2, 
                                                 mean), nsub, np, byrow = TRUE))/matrix(apply(x, 
                                                                                              2, sd), nsub, np, byrow = TRUE))))^2, 1, sum)
      }
    }
    else {
      if (balancemetric == "l1") {
        BL <- apply(abs((pmt %*% ((x - matrix(apply(x, 
                                                    2, mean), nsub, np, byrow = TRUE)) * matrix(weights, 
                                                                                                nsub, np, byrow = TRUE)/matrix(apply(x, 2, 
                                                                                                                                     sd), nsub, np, byrow = TRUE)))), 1, sum)
      }
      else if (balancemetric == "l2") {
        BL <- apply(((pmt %*% ((x - matrix(apply(x, 2, 
                                                 mean), nsub, np, byrow = TRUE)) * matrix(weights, 
                                                                                          nsub, np, byrow = TRUE)/matrix(apply(x, 2, 
                                                                                                                               sd), nsub, np, byrow = TRUE))))^2, 1, sum)
      }
    }
    if (is.null(numschemes)) {
      cho <- round(R * cutoff)
      subid <- order(BL)[1:cho]
      BLcut <- BL[order(BL)[cho]]
      if (cho <= 1) {
        warning("Warning: less than or equal to 1 scheme meets the constraints for the constrained space")
        BLcut = NaN
      }
      rw <- sample(subid, 1)
      BLchoice <- BL[rw]
      inter <- pmt[rw, ]
    }
    else {
      if (numschemes >= R) {
        stop("Error: the fixed number of schemes in the constrained space for constrained randomization must be smaller or equal to the total number of randomization schemes.")
      }
      cho <- numschemes
      subid <- order(BL)[1:cho]
      BLcut <- BL[order(BL)[cho]]
      if (cho <= 1) {
        warning("Warning: less than or equal to 1 scheme meets the constraints for the constrained space")
        BLcut = NaN
      }
      rw <- sample(subid, 1)
      BLchoice <- BL[rw]
      inter <- pmt[rw, ]
    }
    qmt <- pmt[subid, , drop = FALSE]
    R_result <- dim(qmt)[1]
    summary_constraints <- as.data.frame(cbind(S, R, R_result, 
                                               paste0(round(R_result/R, 4) * 100, "%")))
    colnames(summary_constraints) <- c("overall allocations", 
                                       "checked allocations", "accepted allocations", 
                                       "overall % acceptable")
    BL_Quantiles <- quantile(BL, prob = c(0, 0.05, 0.1, 0.2, 
                                          0.25, 0.3, 0.5, 0.75, 0.95, 1))
    BL_Quantiles <- c(BLchoice, BLcut, mean(BL), sd(BL), 
                      BL_Quantiles)
    BL_Quantiles <- round(BL_Quantiles, 3)
    names(BL_Quantiles)[c(1, 2, 3, 4, 5, 14)] <- c("score (selected scheme)", 
                                                   "cutoff score", "Mean", "SD", "Min", 
                                                   "Max")
  }
  if (!is.null(savedata)) {
    SchemeChosen <- rep(0, dim(pmt)[1])
    SchemeChosen[rw] <- 1
    pmt <- cbind(SchemeChosen, pmt)
    write.csv(pmt[subid, ], file = savedata, row.names = FALSE)
  }
  if (bhist) {
    if (sim == 1) {
      par(mar = c(4, 3, 1.5, 1))
      hist(BL, main = paste("Histogram of balance scores across", 
                            R, "unique schemes", "in", S, "simulated schemes"), 
           xlab = NULL, ylab = NULL, breaks = 50)
      title(xlab = "Balance score", line = 2)
      title(ylab = "Frequency", line = 2)
    }
    else {
      par(mar = c(4, 3, 1.5, 1))
      hist(BL, main = paste("Histogram of balance scores across all", 
                            R, "schemes"), xlab = NULL, ylab = NULL, 
           breaks = 50)
      title(xlab = "Balance score", line = 2)
      title(ylab = "Frequency", line = 2)
    }
    abline(v = BLcut, col = "red")
    if (is.null(numschemes)) {
      mtext(paste("The", cutoff, "quantile of the", 
                  balancemetric, "balance score is", round(BLcut, 
                                                           3)), col = "black", side = 1, outer = FALSE, 
            line = 3)
    }
    else {
      mtext(paste("The", numschemes, "schemes", 
                  balancemetric, "balance score is", round(BLcut, 
                                                           3)), col = "black", side = 1, outer = FALSE, 
            line = 3)
    }
    box()
  }
  coin_matrix <- coin_descri <- al_clusters <- alno_clusters <- hi_clusters <- lo_clusters <- NULL
  if (check_validity) {
    n_pair <- t(combn(nsub, 2))
    same_arm_count <- same_arm_frac <- diff_arm_count <- diff_arm_frac <- rep(NA, 
                                                                              dim(n_pair)[1])
    for (j in 1:(nsub - 1)) {
      for (k in (j + 1):nsub) {
        same_arm <- sum((qmt[, j] - qmt[, k]) == 0)
        diff_arm <- R_result - same_arm
        same_prop <- round(same_arm/R_result, 4)
        diff_prop <- round(1 - same_prop, 4)
        index_jk <- which(unlist(lapply(1:dim(n_pair)[1], 
                                        function(i) setequal(c(j, k), n_pair[i, ]))))
        same_arm_count[index_jk] <- same_arm
        same_arm_frac[index_jk] <- same_prop
        diff_arm_count[index_jk] <- diff_arm
        diff_arm_frac[index_jk] <- diff_prop
      }
    }
    first_cluster <- id[n_pair[, 1]]
    second_cluster <- id[n_pair[, 2]]
    coin_matrix <- as.data.frame(cbind(first_cluster, second_cluster, 
                                       same_arm_count, paste0(same_arm_frac * 100, "%"), 
                                       diff_arm_count, paste0(diff_arm_frac * 100, "%")))
    if (sum(same_arm_frac == 1) > 0) {
      alto_index <- which(same_arm_frac == 1)
      alto_clt_pair <- c()
      for (t in 1:length(alto_index)) {
        clt_index <- alto_index[t]
        alto_clts <- paste0(first_cluster[clt_index], 
                            " and ", second_cluster[clt_index])
        alto_clt_pair <- c(alto_clt_pair, alto_clts)
      }
      alto_all_pt <- rep("100.0%", length(alto_index))
      al_clusters <- as.data.frame(cbind(alto_clt_pair, 
                                         alto_all_pt))
      colnames(al_clusters) <- c("cluter pair", "% allocs in the same arm")
    }
    if (sum(same_arm_frac == 0) > 0) {
      alnoto_index <- which(same_arm_frac == 0)
      alnoto_clt_pair <- c()
      for (t in 1:length(alnoto_index)) {
        clt_index <- alnoto_index[t]
        alnoto_clts <- paste0(first_cluster[clt_index], 
                              " and ", second_cluster[clt_index])
        alnoto_clt_pair <- c(alnoto_clt_pair, alnoto_clts)
      }
      alnoto_all_pt <- rep("0.0%", length(alnoto_index))
      alno_clusters <- as.data.frame(cbind(alnoto_clt_pair, 
                                           alnoto_all_pt))
      colnames(alno_clusters) <- c("cluter pair", 
                                   "% allocs in the same arm")
    }
    if (sum(same_arm_frac >= samearmhi) > 0) {
      hi_index <- which(same_arm_frac >= samearmhi)
      hi_pair <- c()
      for (t in 1:length(hi_index)) {
        clt_index <- hi_index[t]
        hi_cluts <- paste0(first_cluster[clt_index], 
                           " and ", second_cluster[clt_index])
        hi_pair <- c(hi_pair, hi_cluts)
      }
      hi_pt <- paste0(same_arm_frac[hi_index] * 100, "%")
      hi_num <- same_arm_count[hi_index]
      hi_non_num <- diff_arm_count[hi_index]
      hi_clusters <- as.data.frame(cbind(hi_pair, hi_pt, 
                                         hi_num, hi_non_num))
      colnames(hi_clusters) <- c("cluster pair", 
                                 "% allocs in same arm", "# allocs in same arm", 
                                 "# allocs in different arms")
    }
    if (sum(same_arm_frac <= samearmlo) > 0) {
      lo_index <- which(same_arm_frac <= samearmlo)
      lo_pair <- c()
      for (t in 1:length(lo_index)) {
        clt_index <- lo_index[t]
        lo_cluts <- paste0(first_cluster[clt_index], 
                           " and ", second_cluster[clt_index])
        lo_pair <- c(lo_pair, lo_cluts)
      }
      lo_pt <- paste0(same_arm_frac[lo_index] * 100, "%")
      lo_num <- same_arm_count[lo_index]
      lo_non_num <- diff_arm_count[lo_index]
      lo_clusters <- as.data.frame(cbind(lo_pair, lo_pt, 
                                         lo_num, lo_non_num))
      colnames(lo_clusters) <- c("cluster pair", 
                                 "% allocs in same arm", "# allocs in same arm", 
                                 "# allocs in different arms")
    }
    coin_descri <- rbind(c(mean(same_arm_count), sd(same_arm_count), 
                           quantile(same_arm_count, prob = c(0, 0.25, 0.5, 0.75, 
                                                             1))), c(mean(same_arm_frac), sd(same_arm_frac), 
                                                                     quantile(same_arm_frac, prob = c(0, 0.25, 0.5, 0.75, 
                                                                                                      1))), c(mean(diff_arm_count), sd(diff_arm_count), 
                                                                                                              quantile(diff_arm_count, prob = c(0, 0.25, 0.5, 0.75, 
                                                                                                                                                1))), c(mean(diff_arm_frac), sd(diff_arm_frac), 
                                                                                                                                                        quantile(diff_arm_frac, prob = c(0, 0.25, 0.5, 0.75, 
                                                                                                                                                                                         1))))
    coin_descri <- round(coin_descri, 3)
    colnames(coin_descri) <- c("Mean", "Std Dev", 
                               "Minimum", "25th Pctl", "Median", 
                               "75th Pctl", "Maximum")
    rownames(coin_descri) <- c("samecount", "samefrac", 
                               "diffcount", "difffrac")
    colnames(coin_matrix) <- c("cluster 1", "cluster 2", 
                               "# same arm", "% same arm", "# different arms", 
                               "% different arms")
  }
  allocation <- cbind(clustername, inter)
  colnames(allocation) <- c("clustername", "allocation")
  allocation <- as.data.frame(allocation)
  BL_Quantiles <- data.frame(names(BL_Quantiles), BL_Quantiles)
  rownames(BL_Quantiles) <- NULL
  colnames(BL_Quantiles) <- NULL
  assignment_message <- paste("You have indicated that you want to assign", 
                              ntrt_cluster, "clusters to treatment", "and", 
                              ntotal_cluster - ntrt_cluster, "to control")
  if (sim == 1) {
    scheme_message <- paste("Simulating", S, "schemes with", 
                            R, "unique schemes for", ntrt_cluster, "clusters in the treatment arm out of", 
                            ntotal_cluster, "clusters in total")
  }
  else {
    scheme_message <- paste("Enumerating all the", 
                            R, "schemes for", ntrt_cluster, "clusters in the treatment arm out of", 
                            ntotal_cluster, "clusters in total")
  }
  if (is.null(numschemes)) {
    cutoff_message <- paste("The quantile cutoff value is", 
                            cutoff, "based on the", balancemetric, "balance metric, the cutoff balance score is", 
                            round(BLcut, 3))
  }
  else {
    cutoff_message <- paste("The fixed number of schemes is", 
                            numschemes, "based on the", balancemetric, 
                            "balance metric, the cutoff balance score is", 
                            round(BLcut, 3))
  }
  choice_message <- paste("Balance score of selected scheme by", 
                          balancemetric, "is", round(BLchoice, 3), collapse = " ")
  data_merge <- data.frame(inter, clustername, data)
  colnames(data_merge)[1] <- "arm"
  if (!is.null(categorical)) {
    if (is.character(categorical)) {
      varsToFactor <- categorical
    }
    else {
      varsToFactor <- colnames(data_merge)[categorical + 
                                             2]
    }
    data_merge[varsToFactor] <- lapply(data_merge[varsToFactor], 
                                       factor)
  }
  Descriptive_statistics <- CreateTableOne(vars = colnames(data_merge)[c(-1, 
                                                                         -2)], strata = c("arm"), data = data_merge, test = FALSE)
  invisible(capture.output(DS <- as.data.frame(print(Descriptive_statistics))))
  colnames(DS)[1:2] <- c("arm = 0", "arm = 1")
  return(list(balancemetric = balancemetric, allocation = allocation, 
              bscores = BL_Quantiles, assignment_message = assignment_message, 
              scheme_message = scheme_message, cutoff_message = cutoff_message, 
              choice_message = choice_message, data_CR = data_merge, 
              baseline_table = DS, cluster_coincidence = coin_matrix, 
              cluster_coin_des = coin_descri, clusters_always_pair = al_clusters, 
              clusters_always_not_pair = alno_clusters, clusters_high_pair = hi_clusters, 
              clusters_low_pair = lo_clusters, overall_allocations = summary_constraints))
}