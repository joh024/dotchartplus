## The code in this .R file is machine generated.
## To understand the program, read the literate description
##  (pdf file) rather than studying just the R code.
## No separate Manual exists.
setClass("layout",
         representation(matrix = "matrix",
                        widths = "character",
                        heights = "character"))
newlayout =
  function(mat, widths = NA, heights = NA){
    if(!is.matrix(mat))
      stop("mat must be a matrix")
    widths = rep(widths, length = dim(mat)[2])
    heights = rep(heights, length = dim(mat)[1])
    new("layout",
        matrix = mat,
        widths = as.character(widths),
        heights = as.character(heights))
  }
getmat = function(obj) obj@matrix
getwid = function(obj) obj@widths
gethei = function(obj) obj@heights
getfino = function(obj) max(getmat(obj)) + 1
LHdefault =
  function(udim = c(1, 1), byrow = FALSE, fino = 1,
           pad = c(0, 0), padmar = lcm(c(0.5, 0.5)),
           widths = NA, heights = NA, reverse = FALSE){
    useq = seq(fino, length = prod(udim))
    if(reverse){
      useq = rev(useq)
      heights = rev(heights)
      widths = rev(widths)
    }
    laymat = matrix(useq, udim[1], udim[2], byrow)
    heights = rep(heights, length = udim[1])
    widths = rep(widths, length = udim[2])
    pad = rep(pad, length = 2)
    padmar = rep(padmar, length = 2)
    if(any(pad > 0)){
      rowind = (1 + pad[1]) * 1:udim[1] - pad[1]
      colind = (1 + pad[2]) * 1:udim[2] - pad[2]
      Lmat = matrix(0, max(rowind), max(colind))
      Lheights = rep(padmar[1], length = dim(Lmat)[1])
      Lwidths = rep(padmar[2], length = dim(Lmat)[2])
      Lmat[rowind, colind] = laymat
      Lheights[rowind] = heights
      Lwidths[colind] = widths
      laymat = Lmat
      heights = Lheights
      widths = Lwidths
    }
    newlayout(laymat, widths, heights)
  }
LHlabels =
  function(x, lengths = c(0, 1, 0), colvec = TRUE, widhei = NA){
    if(colvec){
      ncol = 1
      widths = widhei
      heights = NA
    } else{
      ncol = sum(lengths)
      widths = NA
      heights = widhei
    }
    
    maxlen = max(length(x), length(lengths))
    x = rep(x, maxlen)
    lengths = rep(lengths, maxlen)
    
    labvec = NA
    for(i in 1:maxlen)
      labvec = c(labvec, rep(x[i], lengths[i]))
    labvec = labvec[-1]
    
    newlayout(matrix(labvec, ncol = ncol), widths, heights)
  }
cbind.layout =
  function(..., reverse = FALSE){
    parlist = list(...)
    parlist = parlist[!sapply(parlist, is.null)]
    if(reverse) rev(parlist)
    parmat = lapply(parlist, function(x) getmat(x))
    parwid = lapply(parlist, function(x) getwid(x))
    parhei = lapply(parlist, function(x) gethei(x))
    rowmax = max(sapply(parmat, nrow))
    matscaled = lapply(parmat, function(x)
          matrix(rep(x, each = ceiling(rowmax/dim(x)[1])),
                 ncol = dim(x)[2])[1:rowmax, ,drop = FALSE]     
          )
    new.mat = do.call(cbind, matscaled)
    new.wid = unlist(parwid)
    notna.hei = parhei[sapply(parhei, function(x) all(!is.na(x)))]
    lens.hei = sapply(notna.hei, length)
    firstlongest.hei = which(lens.hei == max(lens.hei))[1]
    new.hei = notna.hei[[firstlongest.hei]]
    newlayout(new.mat, new.wid, new.hei)
  }
setMethod("t", signature(x = "layout"),
  function(x) newlayout(t(getmat(x)), gethei(x), getwid(x)))
rbind.layout =
  function(..., reverse = TRUE){
    parlist = list(...)
    parlist = parlist[!sapply(parlist, is.null)]
    tparlist = lapply(parlist, t)
    combined = do.call(cbind, tparlist)
    t(combined)
  }
LHshift = function(x, prevmax = NULL){
  mat = getmat(x)
  if(is.null(prevmax)) prevmax = max(mat)
  mat[mat > 0] = mat[mat > 0] + prevmax
  newlayout(mat, getwid(x), gethei(x))
}
setMethod("rep", signature(x = "layout"),
          local({
            repfunc = function(x, times, what = "cbind",
              pad = 0, padmar = c(NA, NA)){
              padobj = if(pad > 0)
                rep(list(LHdefault(fino = 0, widths = padmar[2],
                                   heights = padmar[1])), length = pad)
                else NULL
              newx = x
              if(times > 1)
                for(i in 2:times)
                  newx = do.call(what, c(list(newx), padobj,
                    list(LHshift(x, max(getmat(newx))))))
              newx
            }
            repright = function(x, times, pad = 0, padmar = lcm(0.5))
              repfunc(x, times, "cbind", pad = pad, padmar = c(NA, padmar))
            repdown = function(x, times, pad = 0, padmar = lcm(0.5))
              repfunc(x, times, "rbind", pad = pad, padmar = c(padmar, NA))
            function(x, rowtimes = 1, coltimes = 1, byrow = FALSE,
                     pad = c(0, 0), padmar = lcm(c(0.5, 0.5))){
              if(byrow){
                newx = repright(x, coltimes, pad[2], padmar[2])
                repdown(newx, rowtimes, pad[1], padmar[1])
              } else{
                newx = repdown(x, rowtimes, pad[1], padmar[1])
                repright(newx, coltimes, pad[2], padmar[2])
              }
            }
            }))
LHborder =
  function(obj, border = 0.5, numbered = FALSE){
    border[border == 0] = 10^-8
    border = rep(border, length = 4)
    if(numbered) obj = LHshift(obj, 1)
    mat = getmat(obj)
    wid = getwid(obj)
    hei = gethei(obj)
    
    dims = dim(mat)
    bordnum = if(numbered) min(mat[mat > 0]) - 1 else 0
    
    bordtop = rep(bordnum, dims[2])
    bordsides = rep(bordnum, dims[1] + 2)
    
    new.mat = rbind(bordtop, mat, bordtop, deparse.level = 0)
    new.mat = cbind(bordsides, new.mat, bordsides, deparse.level = 0)

    new.wid = c(lcm(border[2]), wid, lcm(border[4]))
    new.hei = c(lcm(border[3]), hei, lcm(border[1]))
  
    newlayout(new.mat, new.wid, new.hei)
  }
LHcall =
  function(obj, cex = NULL){
   layout(getmat(obj), getwid(obj), gethei(obj))
   if(!is.null(cex)) par(cex = cex)
  }
