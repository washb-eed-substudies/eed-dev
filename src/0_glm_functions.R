

fit_washb_glm <- function (d, Y, X, W = NULL, forcedW = NULL, V = NULL, id = "clusterid",
                           family = "gaussian", pval = 0.2, print = TRUE){
  
  cat("\nNon-prescreened covariates: ", paste(forcedW, sep = "", 
                                              collapse = ", "), "\n")
  set.seed(12345)
  require(dplyr)
  #require(faraway)
  if(!is.null(V)) {
    require(lmtest)
  }
  if(!is.null(W)) {
    W <- subset(d, select = W)
  }
  Y <- subset(d, select = Y)
  colnames(Y) <- "Y"
  X <- subset(d, select = X)
  colnames(X) <- "X"
  id <- subset(d, select = id)
  colnames(id) <- "id"
  if(!is.null(V)) {
    Vvar <- subset(d, select = V)
    colnames(Vvar) <- "V"
  }else{
    Vvar <- data.frame(V = rep(1, nrow(d)))
  }
  collinear_vars <- NULL
  if(!is.null(W)) {
    glmdat <- data.frame(Y, X, id, Vvar, W)
  }else{
    glmdat <- data.frame(Y, X, id, Vvar)
  }
  if(!is.null(W)) {
    if(sum(is.na(forcedW)) != 0) {
      colnamesW <- names(W)
    }else{
      if(is.null(forcedW)) {
        Wnames <- names(W)
        forcedW <- c(Wnames[Wnames == "tr" | grepl("age_", 
                                                   Wnames) | grepl("agedays_", Wnames) | grepl("ageday_", 
                                                                                               Wnames)])
      }
      cat("\nNon-prescreened covariates: ", paste(forcedW, 
                                                  sep = "", collapse = ", "), "\n")
      colnamesW <- names(W)[!(names(W) %in% forcedW)]
    }
    screenW <- subset(glmdat, select = colnamesW)
  }else{
    screenW <- NULL
  }
  if(!is.null(screenW)) {
    if(print == TRUE) {
      cat("\n-----------------------------------------\nPre-screening the adjustment covariates:\n-----------------------------------------\n")
    }
    suppressWarnings(Wscreen <- washb_prescreen(Y = glmdat$Y, Ws = screenW, family = family, pval = pval, print = print))
    if(!is.null(forcedW)) {
      Wscreen <- c(as.character(Wscreen), as.character(forcedW))
    }
    W <- subset(glmdat, select = Wscreen)
    Wdf <- W
    Wdf$constant <- rep(1, nrow(glmdat))
    for (i in 1:ncol(W)) {
      tmp <- glm(constant ~ ., data = Wdf, family = family)
      todrop <- NULL
      todrop <- suppressWarnings(names(tmp$coefficients)[-1][as.vector(vif(tmp)) > 
                                                               10][1])
      if(!is.null(todrop) & !is.na(todrop)) {
        collinear_vars <- c(collinear_vars, todrop)
        Wdf <- Wdf[, colnames(Wdf) != todrop]
      }
    }
    to_keep <- colnames(W)[!(colnames(W) %in% collinear_vars)]
    if(length(to_keep) != length(colnames(W))) {
      cat("\nDropped for collinearity with other covariates:\n", 
          colnames(W)[!(colnames(W) %in% to_keep)])
    }
    W_processed <- W[which(colnames(W) %in% to_keep)]
    Wscreen <- colnames(W_processed)
    cat("\n\nCovariated included in model:\n", Wscreen)
  }else{
    Wscreen = NULL
  }
  if(!is.null(Wscreen)) {
    d <- subset(glmdat, select = c("Y", "X", "id", "V", Wscreen))
  }else{
    d <- subset(glmdat, select = c("Y", "X", "id", "V"))
  }
  fullrows <- nrow(d)
  d <- d %>% filter(!is.na(Y))
  Yrows <- nrow(d)
  cat("\nRows dropped due to missing outcome: ", fullrows - 
        Yrows, "\n")
  d <- d %>% filter(!is.na(X))
  Xrows <- nrow(d)
  cat("Rows dropped due to missing exposure: ", Yrows - Xrows, 
      "\n")
  if(!is.null(W) & length(Wscreen) > 0) {
    cat("Percent missingness by covariate:\n")
    print(sapply(d[, -c(1:3)], function(x) round(sum(is.na(x))/nrow(X) * 
                                                   100, 1)))
    d <- d[complete.cases(d), ]
    cat("\nRows dropped due to missing covariates: ", Xrows - 
          nrow(d), "\n")
  }
  cat("Final sample size: ", nrow(d), "\n")
  
  if(!is.null(W) & length(Wscreen) > 0){
    glmdat <- subset(d, select = c('Y','X',Wscreen))
    
    suppressWarnings(fit <- glm(Y ~ ., family = family, data = glmdat))
    vcovCL <- sandwichSE(glmdat, fm = fit, cluster = d$id)
    rfit <- coeftest(fit, vcovCL)
    res <- data.frame(t(rfit[2,]))
    colnames(res) <- c("coef","se","zval","pval")
    res$lb <- res$coef - 1.96 * res$se
    res$ub <- res$coef + 1.96 * res$se
    
  }else{
    
    suppressWarnings(fit <- glm(Y ~ X, family = family, data = d))
    vcovCL <- sandwichSE(d, fm = fit, cluster = d$id)
    rfit <- coeftest(fit, vcovCL)
    res <- data.frame(t(rfit[2,]))
    colnames(res) <- c("coef","se","zval","pval")
    res$lb <- res$coef - 1.96 * res$se
    res$ub <- res$coef + 1.96 * res$se
  }
  
  return(res)
}



# data=d
# X=Xvars[1]
# Y=Yvars[1]
# W=H1a_W
# forcedW = NULL
# V = NULL
# clusterid = "clusterid"
# with.replacement = T
# family = "gaussian"
# B = 200
# confint.level = 0.95
# n.cores = 8


fit_cowboy_glm <- function (data, Y, X, W = NULL, forcedW = NULL, V = NULL, clusterid = "clusterid",
                            with.replacement = T, family = "gaussian", B = 200, confint.level = 0.95, 
                            n.cores = 8){  
  
  require(rsample)
  set.seed(12345)
  bfull <- paste(c(X, W, forcedW), collapse = "+")
  data$Y = data[[Y]]
  if (!is.null(W)){
    prescreened_W <- washb_glmnet_prescreen(Y = data$Y, data %>% select(!!(W)), family = family)
  }else{
    prescreened_W = NULL
  }
  b <- paste(c(X, prescreened_W, forcedW), collapse = "+")
  full_model <- as.formula(paste("Y ~ ", bfull, sep = ""))
  model <- as.formula(paste("Y ~ ", b, sep = ""))
  res.or <- glm(full_model, family = family, data = data)
  res.or.screen <- glm(model, family = family, data = data)
  confint.pboundaries = c((1 - confint.level)/2, 1 - (1 - confint.level)/2)
  confint.Zboundaries = qnorm(confint.pboundaries)
  n <- nrow(data)
  p <- length(res.or$coef)
  coefs <- matrix(NA, nrow = B, ncol = p)
  if (with.replacement) {
    f = NA
    D <- data %>% as_tibble() %>% nest(-clusterid)
    bs <- bootstraps(D, times = B)
    for (i in 1:B) {
      set.seed(i)
      dboot <- as.tibble(bs$splits[[i]]) %>% arrange(clusterid) %>% 
        unnest(cols = c(data))
      if (!is.null(W)) {
        prescreened_W <- washb_glmnet_prescreen(Y = dboot$Y, 
                                                 dboot %>% select(!!(W)), family = family)
      }
      else {
        prescreened_W = NULL
      }
      b <- paste(c(X, prescreened_W, forcedW), 
                 collapse = "+")
      model <- as.formula(paste("Y ~ ", b, sep = ""))
      bootcoef <- tryCatch(coef(glm(model, family = family, 
                                    data = dboot)), error = function(x) rep(as.numeric(NA), 
                                                                            p))
      coefs[i, which(names(res.or$coef) %in% names(bootcoef))] <- bootcoef
    }
  }else {
    cluster <- as.character(data[[clusterid]])
    clusters <- unique(data[[clusterid]])
    Obsno <- split(1:n, cluster)
    f = matrix(clusters, length(clusters), B)
    ff = matrix(f, prod(dim(f)), 1)
    fff = sample(ff)
    f = matrix(fff, length(clusters), B)
    for (i in 1:B) {
      set.seed(i)
      j <- f[, i]
      obs <- unlist(Obsno[j])
      dboot = data[obs, ]
      table(dboot$Y)
      if (!is.null(W)) {
        prescreened_W <- washb_glmnet_prescreen(Y = dboot$Y, 
                                                 dboot %>% select(!!(W)), family = family)
      }
      else {
        prescreened_W = NULL
      }
      b <- paste(c(X, prescreened_W, forcedW), 
                 collapse = "+")
      model <- as.formula(paste("Y ~ ", b, sep = ""))
      bootcoef <- tryCatch(coef(glm(model, family = family, 
                                    data = dboot)), error = function(x) rep(as.numeric(NA), 
                                                                            p))
      coefs[i, which(names(res.or$coef) %in% names(bootcoef))] <- bootcoef
    }
  }
  invalid.samples <- colSums(is.na(coefs))
  names(invalid.samples) <- colnames(coefs) <- names(res.or$coef)
  samples.with.NA.coef <- which(is.na(rowSums(coefs)))
  sdcoefs <- apply(coefs, 2, sd, na.rm = TRUE)
  ci_percentile <- t(apply(coefs, 2, quantile, probs = confint.pboundaries, 
                           na.rm = TRUE))
  ci_parametric <- cbind(res.or$coef + confint.Zboundaries[1] * 
                           sdcoefs, res.or$coef + confint.Zboundaries[2] * sdcoefs)
  ci_BCa <- matrix(NA, 1, 1)
  rownames(ci_percentile) <- dimnames(ci_parametric)[[1]]
  colnames(ci_parametric) <- dimnames(ci_percentile)[[2]]
  res <- list(call = match.call(), model = model, family = family, 
                 B = B, coefficients = coefs, data = data, bootstrap.matrix = f, 
                 subject.vector = clusterid, lm.coefs = res.or$coef, boot.coefs = colMeans(coefs, 
                                                                                           na.rm = TRUE), boot.sds = sdcoefs, ci.level = confint.level, 
                 percentile.interval = ci_percentile, parametric.interval = ci_parametric, 
                 BCa.interval = ci_BCa, samples.with.NA.coef = samples.with.NA.coef, 
                 failed.bootstrap.samples = invalid.samples)
  #class(result) <- "clusbootglm"
  modelfit <- data.frame(est = res$boot.coefs[2], ci.lb = res$percentile.interval[2, 1], ci.ub = res$percentile.interval[2, 2], se = res$boot.sds[2])
  
  return(modelfit)
}


