#' predictNodes
#' 
#' predict the Node ID of tree object
#' 
#' @export
predictNodes<-function (object, newdata, na.action = na.pass) {
  where <-
    if (missing(newdata))
      object$where
  else {
    if (is.null(attr(newdata, "terms"))) {
      Terms <- delete.response(object$terms)
      newdata <- model.frame(Terms, newdata, na.action = na.action,
                             xlev = attr(object, "xlevels"))
      if (!is.null(cl <- attr(Terms, "dataClasses")))
        .checkMFClasses(cl, newdata, TRUE)
    }
    rpart:::pred.rpart(object, rpart:::rpart.matrix(newdata))
  }
  as.integer(row.names(object$frame))[where]
}
