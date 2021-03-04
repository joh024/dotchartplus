##-----------------------------------------------------------
## The code in this .R file is machine generated.
## To understand the program, read the literate description
##  pdf rather than studying just the R code.
##-----------------------------------------------------------
setGeneric("dotchartplus", useAsDefault =
  function(object, ...){
    cat('"', class(object),
        '" is not a recognised object type.',
        '\nThe following are the currently defined methods:',
        '\n(Note that object="ANY" corresponds to this ',
        'error message function)\n', sep = "")
    showMethods("dotchartplus", inherited = FALSE)
  })
dcp = function(...) dotchartplus(...)
setMethod("dotchartplus", signature(object = "list"),
          local({
            dcpLayout =
              function(parslist)
              with(parslist, {
                axes = layoutaxes
                if(is.null(grouplabel)){
                  laymat = LHdefault(udim, pad = pad, padmar = padmar,
                    widths = widths, heights = heights)
                  } else{
                    vlay = LHdefault(c(2, 1), widths = "plot",
                      heights = c("plot", "grouplab"), reverse = TRUE)
                    ulay = rep(vlay, udim[1], udim[2], pad = pad,
                      padmar = padmar)
                    uwid = getwid(ulay)
                    uwid[uwid == "plot"] = widths
                    uhei = gethei(ulay)
                    uhei[uhei == "plot"] = heights
                    uhei[uhei == "grouplab"] =
                      llines(nlines(grouplabel) + 0.1, cex = grouplabcexmult)
                    laymat = newlayout(getmat(ulay), uwid, uhei)
                  }
                if(!is.null(setslabel))
                  laymat = rbind(laymat, LHdefault(fino = getfino(laymat),
                    heights = llines(nlines(setslabel) + 0.1,
                      cex = setslabcexmult)))
                metamat = matrix(c(0, 0, dim(getmat(laymat)), 0, 0), nrow = 2)
                if(any(axes == 1)){
                  laymat = rbind(laymat,
                    LHdefault(fino = 0, heights = axiswidhei[1]))
                  metamat[1, 3] = metamat[1, 3] + 1
                }
                if(any(axes == 2)){
                  laymat = cbind(LHdefault(fino = 0,
                    widths = axiswidhei[2]), laymat)
                  metamat[2, 1] = metamat[2, 1] + 1
                }
                if(any(axes == 3)){
                  laymat = rbind(LHdefault(fino = 0,
                    heights = axiswidhei[3]), laymat)
                  metamat[1, 1] = metamat[1, 1] + 1
                }
                if(any(axes == 4)){
                  laymat = cbind(laymat,
                    LHdefault(fino = 0, widths = axiswidhei[4]))
                  metamat[2, 3] = metamat[2, 3] + 1
                }
                attachlabelsf = function(labn, colvec, reverse){
                  arglist = list(laymat,
                    LHlabels(c(0, getfino(laymat), 0),
                             metamat[2 - as.numeric(colvec),],
                             colvec = colvec,
                             widhei = llines(nlines(labn) + 0.1,
                               cex = labcexmult)))
                  if(reverse) arglist = rev(arglist)
                  do.call(if(colvec) "cbind" else "rbind", arglist)
                }
                if(!is.null(lab1)){
                  laymat = attachlabelsf(lab1, FALSE, FALSE)
                  metamat[1, 3] = metamat[1, 3] + 1
                }
                if(!is.null(lab2)){
                  laymat = attachlabelsf(lab2, TRUE, TRUE)
                  metamat[2, 1] = metamat[2, 1] + 1
                }
                if(!is.null(lab3)){
                  laymat = attachlabelsf(lab3, FALSE, TRUE)
                  metamat[1, 1] = metamat[1, 1] + 1
                }
                if(!is.null(lab4)){
                  laymat = attachlabelsf(lab4, TRUE, FALSE)
                  metamat[2, 3] = metamat[2, 3] + 1
                }
                if(!is.null(main))
                  laymat = rbind(LHdefault(fino = getfino(laymat),
                    heights = llines(nlines(main) + 0.1, cex = maincexmult)),
                    laymat)
                if(any(border > 0))
                  laymat = LHborder(laymat, border)
                LHcall(laymat, cex)
                laymat
              })
            dcpPlot =
              function(datlist, textlist, parslist)
              with(parslist, {
                plotloop =
                  function(rowi, colj){
                    plot.new()
                    plot.window(xlim = xlim[[colj]], ylim = ylim[[rowi]],
                                xaxs = xaxs, yaxs = "i")
                    box()
                    x = datlist[[rowi]]
                    datlen = nrow(x)
                    datcol = ncol(x)
                    y = 1:datlen
                    graphpars = expandpars(parslist[c("fcol", "font",
                      "pbg", "pch", "pcol", "lcol", "lty", "lwd")], datlen,
                      datcol, highlight[[rowi]])
                    xv = as.vector(x)
                    yv = rep(y, datcol)
                    if(full.lines){
                      curusr = par("usr")
                      xstart = curusr[1]
                      xend = curusr[2]
                    } else{
                      xstart = 0
                      xend = xv
                    }
                    
                    lfunc(xstart, yv, xend, yv,
                             col = graphpars$lcol,
                             lty = graphpars$lty,
                             lwd = graphpars$lwd)
                    pfunc(xv, yv,
                           bg = graphpars$pbg,
                           pch = graphpars$pch,
                           col = graphpars$pcol)
                    grouplabjump =
                      if(is.null(grouplabel)) NA
                      else (nlines(grouplabel) + 0.1) * grouplabcexmult
                    setslabjump =
                      if(is.null(setslabel)) NA
                      else (nlines(setslabel) + 0.1) * setslabcexmult
                    if(any(axes == 1) && rowi == udim[1])
                      axis.cus(1, at1[[colj]], atsmall1[[colj]], atlabels1[[colj]],
                               line = setslabjump)
                    if(any(axes == 3) && rowi == 1)
                      axis.cus(3, at3[[colj]], atsmall3[[colj]], atlabels3[[colj]],
                               line = grouplabjump)
                    if(percentile == TRUE && colj == udim[2]){
                      pvaln = c(1, 2, 4, 5)[datlen < c(10, 20, 30, Inf)][1]
                      pvallabels = (0:pvaln)/pvaln
                      pvalat = quantile(y, pvallabels)
                      axis.cus(4, pvalat, NULL, pvallabels * 100)
                    }
                    txtjumpf = function(txtcol){
                      lcmTOlines(fpad) +
                        sum(lcmTOlines(labwidths[0:(txtcol - 1)])) +
                          lcmTOlines(sumlcm(labwidths[txtcol], -fpad)) *
                            adj[txtcol]
                    }
                    txt = textlist[[rowi]]
                    if(any(axes == 2) && colj == 1)
                      for(txtcol in 1:ncol(txt)){
                        txtjump = sum(lcmTOlines(sumlcm(labwidths, fpad))) -
                          txtjumpf(txtcol)
                        mtext(txt[,txtcol], 2, txtjump, at = y,
                              adj = adj[txtcol], cex = cex, las = 2,
                              col = graphpars$fcol[1:datlen],
                              font = graphpars$font[1:datlen])
                      }
                    if(any(axes == 4) && colj == udim[2])
                      for(txtcol in 1:ncol(txt)){
                        mtext(txt[,txtcol], 4, txtjumpf(txtcol), at = y,
                              adj = adj[txtcol], cex = cex, las = 2,
                              col = graphpars$fcol[1:datlen],
                              font = graphpars$font[1:datlen])
                      }
                    if(!is.null(grouplabel)){
                        text.cus(grouplabel[rowi],
                                 pos = c(grouplabadj, 0.5),
                                 col = grouplabcol,
                                 font = grouplabfont,
                                 cex = grouplabcexmult,
                                 bg = grouplabbg)
                        box()
                      }
                  }
                for(colj in 1:udim[2])
                  for(rowi in 1:udim[1])
                    plotloop(rowi, colj)
                if(!is.null(setslabel)){
                  nsets = length(setslabel)
                  graphpars = expandpars(parslist[c("pbg", "pch",
                    "pcol")], 1, nsets)
                  plot.new()
                  plot.window(xlim = c(0.5, nsets + 1), ylim = c(0, 2),
                              xaxs = "i", yaxs = "i")
                  rect(0.5, 0, nsets + 1, 2, col = grouplabbg,
                       border = NA)
                  box()
                  pchjump = strwidth("MM")
                  for(i in 1:nsets){
                    points(i, 1, bg = graphpars$pbg[i],
                           col = graphpars$pcol[i], pch = graphpars$pch[i])
                    text(i + pchjump, 1, setslabel[i], adj = 0,
                         cex = setslabcexmult)
                  }
                }
                if(!is.null(lab1))
                  text.cus(lab1, labcexmult)
                if(!is.null(lab2))
                  text.cus(lab2, labcexmult, srt = 90)
                if(!is.null(lab3))
                  text.cus(lab3, labcexmult)
                if(!is.null(lab4))
                  text.cus(lab4, labcexmult, srt = 90)
                if(!is.null(main))
                  text.cus(main, maincexmult)
              })
            expandpars =
              function(parslist, nrows = 1, ncols = 1, highsub = NULL){
                parsdf = lrow(parslist, 1:ncols)
                dfcommand = "`=`(parsdf, data.frame("
                for(i in 1:length(parsdf))
                  dfcommand = paste(dfcommand, names(parsdf)[i],
                    " = I(matrix(rep(parsdf[[", i,
                    "]], each = nrows), nrow = nrows)), ", sep = "")
                dfcommand = paste(substr(dfcommand, 1,
                  nchar(dfcommand) - 2), "))", sep = "")
                eval(parse(text = dfcommand))
                if(!is.null(highsub))
                  for(j in 1:ncol(highsub)){
                    parsdf[highsub[,j],] =
                      data.frame(lrow(parslist, j * ncols + 1:ncols))
                  }
                parsdf
              }
            llines = function(x, cex = 1)
              lcm(2.54 * cex * x * par("csi"))
            nlines = function(x)
              nchar(x) - nchar(gsub("\n", "", x)) + 1
            lcmTOlines = function(x, cex = 1)
              as.numeric(sub(" cm", "", x))/(2.54 * cex * par("csi"))
            strwidth.cm =
              Vectorize(function(x, font = par("font"), cex = 1)
                        2.54 * strwidth(x, "inches", font = font, cex = cex))
            sumlcm = function(...)
              lcm(do.call(sum, lapply(list(...), function(x)
                                      as.numeric(sub(" cm", "", x)))))
            axis.cus =
              function(side, at = NULL, atsmall = NULL, labels = TRUE,
                       tcl = par("tcl"), ...){
                if(!is.null(atsmall))
                  axis(side, at = atsmall, labels = FALSE, tcl = tcl/2, ...)
                axis(side, at = at, labels = labels, tcl = tcl, ...)
              }
            text.cus =
              function(txt, cex = 1, pos = c(0.5, 0.5), bg = NULL, ...){
                plot.new()
                plot.window(xlim = 0:1, ylim = 0:1, xaxs = "i", yaxs = "i")
                if(!is.null(bg))
                  rect(0, 0, 1, 1, col = bg, border = NA)
                text(pos[1], pos[2], txt, adj = pos, cex = cex, ...)
              }
            lrow = function(lobj, row)
              lapply(lobj, function(x) x[(row - 1) %% length(x) + 1])
            function(object, textlist = NULL, xlab = NULL, col = NULL,
                     at = NULL, atsmall = NULL, atlabels = NULL,
                     parslist = DefaultParslist, ...){
              datlist = object
              rm(object)
              if(!is.null(col))
                parslist$pbg = col
              if(!is.null(at))
                for(subs in c("at1", "at3"))
                  parslist[[subs]] = at
              if(!is.null(atsmall))
                for(subs in c("atsmall1", "atsmall3"))
                  parslist[[subs]] = atsmall
              if(!is.null(atlabels))
                for(subs in c("atlabels1", "atlabels3"))
                  parslist[[subs]] = atlabels
              optpars = list(...)
              for(i in 1:length(optpars))
                if(any(names(parslist) == names(optpars)[i]))
                  parslist[names(optpars)[i]] = optpars[i]
              if(!is.null(xlab)){
                if(any(parslist$axes == 1))
                  parslist$lab1 = xlab
                if(any(parslist$axes == 3))
                  parslist$lab3 = xlab
                }
              if(!is.list(datlist))
                stop("datlist must be a list.")
              datlist = lapply(datlist, as.matrix)
              parslist$cex = with(parslist, {
                cex = rep(cex, length = 2)
                cex.best = function(){
                  dat.length = sum(sapply(datlist, function(x) nrow(x) + 2))
                  total.lines = 1.05 * dat.length +
                    2.6 * (any(axes == 1) + any(axes == 3)) +
                      labcexmult * (!is.null(lab1) + !is.null(lab3)) +
                        maincexmult * (!is.null(main))
                  dcm = 2.54 * par("din")[2] - 2 * border
                  cm.per.line = dcm/total.lines
                  cm.per.line/(2.54 * par("csi"))
                }
                min(max(cex[1], cex.best()), cex[2])
              })
              opar = par(cex = parslist$cex, mar = rep(0, 4))
              oopt = options(stringsAsFactors = FALSE)
              on.exit({
                par(opar)
                options(oopt)})
              if(parslist$newlayout)
                on.exit(layout(1), add = TRUE)
              if(is.null(textlist))
                textlist = lapply(datlist, rownames)
              txtlen = function(txt)
                if(is.null(txt)) 0 else nrow(as.matrix(txt))
              for(i in 1:length(datlist)){
                if(txtlen(textlist[[i]]) != nrow(datlist[[i]]))
                  textlist[[i]] =
                    paste(LETTERS[i], 1:nrow(datlist[[i]]) , sep = "")
              }
              textlist = lapply(textlist, as.matrix)
              parslist = with(parslist, {
                if(is.null(xlim))
                  xlim = range(unlist(datlist))
                if(!is.list(xlim))
                  xlim = list(xlim)
                xlim = lapply(xlim, function(x) rep(x, length = 2))
                if(is.null(full.lines))
                  if((all(xlim[[1]] != 0) &&
                      sum(sign(xlim[[1]] + diff(xlim[[1]]) *
                               0.04 * c(-1, 1))) != 0) || length(xlim) > 1)
                    full.lines = TRUE
                  else
                    full.lines = FALSE
                for(subs in c("at1", "at3", "atsmall1", "atsmall3",
                              "atlabels1", "atlabels3"))
                  eval({
                    substitute({
                      if(!is.list(get(curat))) `=`(curat, list(get(curat)));
                      `=`(curat, rep(get(curat), length = length(xlim)))},
                               list(curat = subs))
                  })
                ylim = lapply(datlist, function(x) c(0.25, nrow(x) + 0.75))
                adj = rep(adj, length = ncol(textlist[[1]]))
                if(!is.null(highlight)){
                  if(!is.list(highlight))
                    highlight = list(highlight)
                  highlight = lapply(highlight, as.matrix)
                  highlight = lapply(highlight, function(x){
                    for(j in 1:ncol(x))
                      if(!all(sign(x[,j]) >= 0) && !all(sign(x[,j]) <= 0)){
                        warning(paste("Only 0's may be mixed with negative",
                                      "subscripts.",
                                      "\nInvalid highlight columns removed."),
                                call. = FALSE)
                        x[,j] = 0
                      }
                    x
                  })
                  highlight = rep(highlight, length = length(datlist))
                }
                udim = c(length(ylim), length(xlim))
                pad = rep(pad, length = 2)
                if(percentile && any(axes == 4)){
                  warning(paste("The percentile axis is always drawn on axis",
                                "4 (to the right of the plot).",
                                "The specification of axis 4 for a label",
                                "axis has been overriden to accomodate the",
                                "percentile axis.", sep = ""),
                          call. = FALSE)
                  axes = axes[axes != 4]
                  }
                if(is.null(grouplabel))
                  grouplabel = names(datlist)
                if(any(grouplabel == TRUE))
                  grouplabel = paste("Group", LETTERS[1:length(datlist)])
                if(any(grouplabel == FALSE))
                  grouplabel = NULL
                if(!is.null(grouplabel) &&
                   length(datlist) != length(grouplabel)){
                  grouplabel = NULL
                  warning(paste("length(datlist) != length(grouplabel).",
                                "grouplabel forced to NULL"),
                          call. = FALSE)
                }
                if(is.null(setslabel) && (ncol(datlist[[1]]) > 1))
                  setslabel = dimnames(datlist[[1]])[[2]]
                if(any(setslabel == FALSE))
                  setslabel = NULL
                if(!is.null(setslabel) &&
                   ncol(datlist[[1]]) != length(setslabel)){
                  setslabel = NULL
                  warning(paste("ncol(datlist[[1]]) != length(sets",
                                "label)). setslabel forced to NULL",
                                sep = ""), call. = FALSE)
                }
                if(is.null(widths))
                  widths = abs(sapply(xlim, diff))
                if(is.null(heights))
                  heights = abs(sapply(ylim, diff))
                if(is.null(padmar))
                  padmar = lcm(rep(strwidth.cm("m"), 2))
                if(is.null(fpad))
                  fpad = strwidth.cm("m")
                if(is.null(labwidths)){
                  labwidthsf = function(){
                    textlen = length(textlist)
                    textcols = ncol(textlist[[1]])
                    labwidmat = matrix(NA, nrow = textlen, ncol = textcols)
                    for(i in 1:textlen){
                      curtext = textlist[[i]]
                      curfont = expandpars(parslist["font"], nrow(curtext),
                        ncol(datlist[[i]]), highlight[[i]])
                      for(j in 1:textcols)
                        labwidmat[i, j] = max(strwidth.cm(curtext[,j],
                                   font = curfont$font[1:nrow(curtext)]))
                    }
                    lcm(apply(labwidmat, 2, max) + fpad)
                  }
                  labwidths = labwidthsf()
                  rm(labwidthsf)
                }
                axiswidhei = numeric(4)
                axiswidhei[c(1, 3)] = llines(2.6)
                axiswidhei[c(2, 4)] = sumlcm(labwidths, fpad)
                if(percentile){
                 layoutaxes = c(axes, 4)
                 lab4 = "Percentile"
                 axiswidhei[4] = llines(2.6)
                } else layoutaxes = axes
                as.list(environment())
              })
              laymat = if(parslist$newlayout) dcpLayout(parslist) else NULL
              dcpPlot(datlist, textlist, parslist)
              invisible(list(layout = laymat, parslist = parslist))
              }
            })
          )
dcpParslist =
  function(cex = c(0.6, 1.1), highlight = NULL,
           
           widths = NULL, heights = NULL, labwidths = NULL,
           fpad = NULL, pad = c(0, 1), padmar = NULL,
           border = 0.5, newlayout = TRUE,
           
           axes = c(1, 2),
           lab1 = NULL, lab2 = NULL, lab3 = NULL, lab4 = NULL,
           labcexmult = 1, percentile = FALSE,
           xlim = NULL, xaxs = "r",
           at1 = NULL, atsmall1 = NULL, atlabels1 = NULL,
           at3 = NULL, atsmall3 = NULL, atlabels3 = NULL,
           
           main = NULL, maincexmult = 1.5,
           grouplabel = NULL, grouplabcexmult = 1,
           grouplabadj = 0.5, grouplabbg = "#F0F0F0",
           grouplabcol = 1, grouplabfont = 1,
           setslabel = NULL, setslabcexmult = 1,
           
           pfunc = points, pbg = c("white", "black"),
           pch = c(21, 21, 24, 24), pcol = 1,
           adj = 0.5, fcol = 1, font = 1:4,
           full.lines = NULL, lfunc = segments,
           lcol = 1, lty = 3, lwd = 1)
  as.list(environment())
DefaultParslist = dcpParslist()
