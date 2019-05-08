#' export
predict.harModel <- function(object, newdata = NULL) {

  # If no new data is provided - just forecast on the last day of your estimation sample
  # If new data with colnames as in object$model$x is provided, i.e. right measures for that model, just use that data
  if (is.null(newdata)) {

    if (is.null(object$transform) == TRUE) {
      return(as.numeric(c(1, xts::last(object$model$x))  %*%  x$coefficients))
    }

    if (object$transform == "log") {
      warning("Due to log-transform, forecast of RV is derived under assumption of log-normality.")
      return(as.numeric(exp(c(1, xts::last(object$model$x))  %*%  object$coefficients + 1/2 * var(object$residuals))))
    }

    if (object$transform == "sqrt") {
      warning("Forecast for sqrt(RV) due to transform == \"sqrt\".")
      return(as.numeric(c(1, xts::last(object$model$x))  %*%  object$coefficients))
    }
  }

  # Check whether newdata is in right format
  if (sum(colnames(newdata) == colnames(object$model$x)) == length(colnames(object$model$x))) {

    if (is.null(object$transform) == TRUE) {
      return(as.numeric(cbind(1, newdata)  %*%  object$coefficients))
    }

    if (object$transform == "log") {
      warning("Due to log-transform, forecast of RV is derived under assumption of log-normality.")
      return(as.numeric(exp(cbind(1, newdata)  %*%  object$coefficients + 1/2 * var(object$residuals))))
    }

    if (object$transform == "sqrt") {
      warning("Forecast for sqrt(RV) due to transform == \"sqrt\".")
      return(as.numeric(cbind(1, newdata)  %*%  object$coefficients))
    }

  } else {
    warning(paste0(c("newdata column names should be", colnames(object$model$x), "as harModel-type is", object$type, "."), collapse = " "))
  }
}
