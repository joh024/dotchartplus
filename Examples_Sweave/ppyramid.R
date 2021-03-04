## ppyramid
## Argument:
## male: the heights of the bars in the left panel of the display.
## female: the heights of the bars in the right panel of the display.
## labels: character string labels for the age-group intervals.
## genderlabels: character string labels for the left and right panels.
## xlim: the range of the x axis
## col: the colour of the bars for the left and right sides
##  (these are recycled if necessary).
## mar: the size of the margins to be drawn around the plot.
## main: a main title to appear at the top of the graph.
## xlab: a label to appear below the x axes.
##
## Output:
## Draws population pyramids
##
## Narration:
## Yak yak
ppyramid =
  function(male, female, labels,
           genderlabels = c("Male", "Female"),
           xlim = range(male, female),
           col = "lightgray", mar = 1,
           main = NULL, xlab = NULL,
		   cex = 0.67)
  {
    ## Check lengths
    if(length(male) != length(female) |
       length(male) != length(labels))
      stop("Incompatible lengths")
    
    ## Adjust xlim for silly specified arg
    xlim = c(0, 1.04 * max(xlim))
    
    ## Ensure col is length 2 (recycling used)
    col = rep(col, length = 2)
    
    ## Add 1.6 lines for lower margin for x axis,
    ## If xlab specified, further add 1 line to lower margin.
    ## Add 1.1 lines for upper margin for gender labels,
    ## If main specified, further add 1.2 lines to upper margin.
    marplus = c(as.numeric(!is.null(xlab)) + 1.6, 0,
      as.numeric(!is.null(main)) * 1.2 + 1.1, 0)
    opar = par(mar = rep(0, 4), oma = mar + marplus, cex = cex)
    
    ## Calculate labwidth
    ## Add 0.3cm padding so the text doesn't go right to the edge
    labwidth = lcm(max(2.54 * strwidth(labels, "inches")) + 0.3)
    layout(matrix(c(1, 3, 2), nrow = 1),
           widths = c(1, labwidth, 1),
           heights = 1)
    par(cex = cex)
    
    ppyramid.hist(male, genderlabels, 0, rev(xlim), col, cex)
    ppyramid.hist(female, genderlabels, 1, xlim, col, cex)
    
    ppyramid.labels(labels, cex, main, xlab)
    
    par(opar)
  }

ppyramid.hist =
  function(dat, genderlabels, adj, xlim, col, cex)
  {
    plot.new()
    plot.window(xlim = xlim, ylim = c(0, length(dat)),
                xaxs = "i", yaxs = "i")
    box()
    
    ## Axis
    at = seq(0, max(xlim), by = 50)
    abline(v = at, lty = 3)
    axis(1, at = at)
    
    ## Hist
    y = 1:length(dat)
    rect(0, y - 1, dat, y, col = col[adj + 1])
    
    ## Gender Lab
    mtext(genderlabels[adj + 1], 3, line = 0.5, adj = adj, cex = cex)
  }

ppyramid.labels =
  function(labels, cex, main, xlab)
  {
    ## mid-labels
    plot.new()
    plot.window(xlim = 0:1, ylim = c(0, length(labels)),
                xaxs = "i", yaxs = "i")
    text(0.5, 1:length(labels) - 0.5, labels)
    
    ## main/xlab
    if(!is.null(main))
      mtext(main, 3, line = 1.6, adj = 0.5, cex = cex * 1.2, font = 2)
    if(!is.null(xlab))
      mtext(xlab, 1, line = 2.1, adj = 0.5, cex = cex)
  }

ppyramid.test =
  function(yearind = 1, ...)
    ppyramid(nzpop[,yearind,1], nzpop[,yearind,2],
             labels = dimnames(nzpop)$group,
             main = paste("New Zealand Population,",
               dimnames(nzpop)$year[yearind]),
             xlab = "Population Size in Thousands", ...)