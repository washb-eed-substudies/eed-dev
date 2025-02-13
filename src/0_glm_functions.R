

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




