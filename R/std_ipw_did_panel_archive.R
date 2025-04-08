#' @title An archived version of the DRDID's std_ipw_did_panel without the fastglm function
#'
#' @param y1 The post-treatment vector
#' @param y0 The pre-treatment vector
#' @param D The binary treatment vector
#' @param covariates The matrix or vector of covariates to train weights
#' @param i.weights Sample weights. Default is NULL.
#' @param boot A logical as to whether to bootstrap. Default is FALSE.
#' @param boot.type Type of bootstrap. Default is a weighted bootstrap.
#' @param nboot Number of bootstraps
#' @param inffunc A logical as to whether to report the influence function or not.
#'
#' @return Returns an object with calculated ATT, standard error, confidence intervals, and influence functions.
#'
#' @export
#'
#' @examples
#' std_ipw_did_panel_archive(rep(1,20),rep(0,20),c(1,rep(0,19)),c(70,rep(50,9),rep(80,10)))
#'
std_ipw_did_panel_archive <- function(y1, y0, D, covariates, i.weights = NULL,
                             boot = FALSE, boot.type = "weighted", nboot = NULL,
                             inffunc = FALSE){
  #-----------------------------------------------------------------------------
  # D as vector
  D <- as.vector(D)
  # Sample size
  n <- length(D)
  # generate deltaY
  deltaY <- as.vector(y1 - y0)
  # Covariate vector
  if(is.null(covariates)){
    int.cov <- as.matrix(rep(1,n))
  } else{
    int.cov <- as.matrix(covariates)
  }


  # Weights
  if(is.null(i.weights)) {
    i.weights <- as.vector(rep(1, n))
  } else if(min(i.weights) < 0) stop("i.weights must be non-negative")
  # Normalize weights
  i.weights <- i.weights/mean(i.weights)
  #-----------------------------------------------------------------------------
  # Pscore estimation (logit) and also its fitted values
  PS <- stats::glm(D ~ int.cov, family=stats::binomial(link="logit"))
  ps.fit <- stats::predict(PS, type="response")

  if(PS$converged == FALSE){
    warning("Propensity score estimation did not converge.")
  }

  # PS2 <- suppressWarnings(fastglm::fastglm(
  #   x = int.cov,
  #   y = D,
  #   family = stats::binomial(),
  #   weights = i.weights,
  #   intercept = TRUE,
  #   method = 3
  # ))
  # ps.fit2 <- fitted(PS2)


  # Do not divide by zero
  ps.fit <- pmin(ps.fit, 1 - 1e-6)
  W <- ps.fit * (1 - ps.fit) * i.weights
  #-----------------------------------------------------------------------------
  #Compute IPW estimator
  # First, the weights
  w.treat <- i.weights * D
  w.cont <- i.weights * ps.fit * (1 - D)/(1 - ps.fit)

  std.w.cont <- w.cont/sum(w.cont)

  att.treat <- w.treat * deltaY
  att.cont <- w.cont * deltaY

  eta.treat <- mean(att.treat) / mean(w.treat)
  eta.cont <- mean(att.cont) / mean(w.cont)

  ipw.att <- eta.treat - eta.cont
  #-----------------------------------------------------------------------------
  #get the influence function to compute standard error
  #-----------------------------------------------------------------------------
  # Asymptotic linear representation of logit's beta's
  score.ps <- i.weights * (D - ps.fit) * int.cov
  #Hessian.ps <- stats::vcov(PS) * n
  Hessian.ps <- chol2inv(chol(t(int.cov) %*% (W * int.cov))) * n
  asy.lin.rep.ps <-  score.ps %*% Hessian.ps
  #-----------------------------------------------------------------------------
  # Now, the influence function of the "treat" component
  # Leading term of the influence function: no estimation effect
  inf.treat <- (att.treat - w.treat * eta.treat)/mean(w.treat)
  # Now, get the influence function of control component
  # Leading term of the influence function: no estimation effect
  inf.cont.1 <- (att.cont - w.cont * eta.cont)
  # Estimation effect from gamma hat (pscore)
  # Derivative matrix (k x 1 vector)
  M2 <- base::colMeans(w.cont *(deltaY - eta.cont) * int.cov)
  # Now the influence function related to estimation effect of pscores
  # Batch multiple matrix multiplications for inf functions
  #batch_results <- batch_matrix_operations(NULL, NULL, score.ps, Hessian.ps, NULL, M2, NULL)
  inf.cont.2 <- asy.lin.rep.ps %*% M2
  #inf.cont.2 <- batch_results$inf_cont_2

  # Influence function for the control component
  inf.control <- (inf.cont.1 + inf.cont.2) / mean(w.cont)

  #get the influence function of the DR estimator (put all pieces together)
  att.inf.func <- inf.treat - inf.control
  #-----------------------------------------------------------------------------
  if (boot == FALSE) {
    # Estimate of standard error
    se.att <- stats::sd(att.inf.func)/sqrt(n)
    # Estimate of upper boundary of 95% CI
    uci <- ipw.att + 1.96 * se.att
    # Estimate of lower boundary of 95% CI
    lci <- ipw.att - 1.96 * se.att

    #Create this null vector so we can export the bootstrap draws too.
    ipw.boot <- NULL
  }
  if (boot == TRUE) {
    if (is.null(nboot) == TRUE) nboot = 999
    if(boot.type == "multiplier"){
      # do multiplier bootstrap
      ipw.boot <- mboot.did.archive(att.inf.func, nboot)
      # get bootstrap std errors based on IQR
      se.att <- stats::IQR(ipw.boot) / (stats::qnorm(0.75) - stats::qnorm(0.25))
      # get symmetric critical values
      cv <- stats::quantile(abs(ipw.boot/se.att), probs = 0.95)
      # Estimate of upper boundary of 95% CI
      uci <- ipw.att + cv * se.att
      # Estimate of lower boundary of 95% CI
      lci <- ipw.att - cv * se.att
    } else {
      # do weighted bootstrap
      ipw.boot <- unlist(lapply(1:nboot, wboot.std.ipw.panel.archive,
                                n = n, deltaY = deltaY, D = D, int.cov = int.cov, i.weights = i.weights))
      # get bootstrap std errors based on IQR
      se.att <- stats::IQR(ipw.boot - ipw.att) / (stats::qnorm(0.75) - stats::qnorm(0.25))
      # get symmetric critical values
      cv <- stats::quantile(abs((ipw.boot - ipw.att)/se.att), probs = 0.95)
      # Estimate of upper boundary of 95% CI
      uci <- ipw.att + cv * se.att
      # Estimate of lower boundary of 95% CI
      lci <- ipw.att - cv * se.att

    }

  }
  if(inffunc == FALSE) att.inf.func <- NULL
  #---------------------------------------------------------------------
  # record the call
  call.param <- match.call()
  # Record all arguments used in the function
  argu <- mget(names(formals()), sys.frame(sys.nframe()))
  boot.type <- ifelse(argu$boot.type=="multiplier", "multiplier", "weighted")
  boot <- ifelse(argu$boot == TRUE, TRUE, FALSE)
  argu <- list(
    panel = TRUE,
    normalized = TRUE,
    boot = boot,
    boot.type = boot.type,
    nboot = nboot,
    type = "ipw"
  )
  ret <- (list(ATT = ipw.att,
               se = se.att,
               uci = uci,
               lci = lci,
               boots = ipw.boot,
               att.inf.func = att.inf.func,
               call.param = call.param,
               argu = argu))

  # return the list
  return(ret)
}
