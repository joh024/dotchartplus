parseFormula = local({
    TILDE    = as.name("~")
    BAR      = as.name("|")
    PLUS     = as.name("+")
    IDENTITY = as.name("I")        
    stripI =
        function(e) {
            if (is.call(e) && identical(e[[1]], IDENTITY)) {
                if (length(e) == 2)
                    e[[2]]
                else
                    stop("incorrect use of I()")
            }
            else e
        }   
    element.list =
        function(expr) {
            if (is.call(expr) && identical(expr[[1]], PLUS))
                if (length(expr) == 3)
                    c(element.list(expr[[2]]),
                      stripI(expr[[3]]))
                else 
                    stop("invalid element list")
            else
                list(stripI(expr))
        }
    right.side =
        function(e) {
            if (is.call(e) && identical(e[[1]], BAR)) {
                if(length(e) == 3)
                    list(rhs = element.list(e[[2]]),
                         condition = element.list(e[[3]]))
                else
                    stop("invalid formula rhs")
            }
            else
                list(rhs = element.list(e),
                     condition = NULL)
        }
    formula =
        function(expr) {
            if (is.call(expr)
                    && identical(expr[[1]], TILDE)) {
                if(length(expr) == 2)
                    c(lhs = list(),
                      right.side(expr[[2]]))
                else if (length(expr) == 3)
                    c(lhs = list(element.list(expr[[2]])),
                      right.side(expr[[3]]))
                else
                    stop("invalid formula")
            }
            else
                stop("missing ~ in formula\n")
        }
})
