##- forest plot basic - Stephane
##---- libs ----
library(ggplot2)
library(reshape2)
library(cowplot)

##---- utils ----
if (TRUE){
  ##---- paste_x_ci ----
  #' @param m matrix of 3 columns: estimate, lower and upper bounds
  #' @param k significant digits
  #' @param rfn rounding function
  #' @param sep separator
  #' @examples
  #' m0 <-  structure(list(
  #'  or_clr = c(2.759, 1.702, 25.765, 0.868, 1.658, 0.902),
  #'  lo_clr = c(1.261, 0.805, 15.787, 0.207, 1.163, 0.649),
  #'  up_clr = c(6.038, 3.599, 42.048, 3.644, 2.365, 1.254)),
  #'  row.names = c(NA, 6L), class = "data.frame")
  #' paste_x_ci( m0 )
  paste_x_ci <- function(m, k = 2,
                         rfn = function(x) table1::signif_pad(x, digits=k),
                         sep = "-",
                         bigmark = NULL,
                         decmark =getOption("OutDec")){
    # require(table1)
    apply(m, 1, function(r){
      if (any(is.na(r))) return(NA)
      r <- sapply(r, rfn)
      if (!is.null(bigmark)){
        r <- sapply(r, function(x) prettyNum(as.numeric(x),
                                             big.mark = bigmark,
                                             decimal.mark = decmark))
      }
      paste0(r[1], " (", r[2], sep, r[3], ")")
    })
  }
  ##---- prep_tab ----
  #' order rows and reverse when showing
  prep_tab <- function(dt){
    if (!is.data.frame(dt)) stop("needs a dataframe")
    a <- 1:nrow(dt)
    dt$id <- factor(a, rev(a))
    dt
  }
  # str(prep_tab(d1))
  ##---- add_group_rows ----
  #' add a group level as heading of expo variable
  add_group_rows <- function(dt,
                             var_group = "group",
                             var_expo =  "Dose"){
    u <- levels(factor(dt[, var_group]))
    k <- length(u)
    # dt <- prep_tab(dt)
    .a <- by(dt, dt[, var_group], identity)
    d2 <- do.call(rbind, lapply(.a, function(x) rbind(NA, x)) )
    ## add subtitle
    d2[ is.na(d2[, var_expo]), var_expo] <- u
    ## add fake y column
    d2 <- prep_tab(d2)
    rownames(d2) <- NULL
    d2
  }
}

##---- fake data ----
#' m number estimates
#' n samplesize multiplier
#' k decimal rounding
#' sep separator for CI
#' groups group labels
## debug m = 6; n = 1e3; k = 2; sep = ", "; seed = 123
make_data <- function(m = 6, n = 1e2, k = 2,
                      sep = ", ",
                      seed = 123){
  set.seed(seed)
  var <-  LETTERS[1:m] ## estimates labels
  rr  <-  round( exp( rnorm(m) ), k) ## sort of RR
  de <-  runif(m)*rr*2
  lo <-  rr - de
  up <-  rr + de
  events <- round( n / de^2 ) ## let's pretend
  ci <- paste0("(", round(lo, k), sep, round(up, k), ")")
  rr_ci <- paste(round(rr, k), ci )
  data.frame(var, events, rr, lo, up , ci, rr_ci )
}
# ( d <- make_data(sep = " to ", seed = 123) )
# prep_tab(d)

##---- plot ----
make_plot <- function(dt, x, lo, up,
                      y = "id",
                      id2 = NULL,
                      labx = NULL,
                      xref = 1,
                      co = NULL,
                      dodgew = 0,
                      log = FALSE,
                      shp = 22,
                      size = 11){
  dt <- prep_tab(dt)
  pos <- position_dodge2(width = dodgew)
  p0 <- ggplot(dt, aes_string(x, y,
                              colour = co))
  p1 <- p0 +
    geom_linerange(aes_string(xmin = lo, xmax = up),
                   position = pos,
                   show.legend = FALSE) + ## error bars
    geom_vline(xintercept = xref, linetype = "dotted") + ## reference
    geom_point(position = pos, shape = shp,
               col = "black",
               fill = "white" ) +
    xlab(labx) +
    theme_classic() + # option
    theme(
      axis.text.x = element_text(size = size, colour = "black"),
      axis.line.x = element_line(),
      axis.text.y = element_blank(),
      axis.title.y = element_blank(),
      axis.line.y = element_blank(),
      axis.ticks.y = element_blank(),
      legend.position = "top",
      legend.title= element_blank())
  if(log) p1 <- p1 + scale_x_log10()
  if (is.null(id2)) return(p1) else {
    p1 +
      facet_grid(id2,
                 scales = "free",
                 space = "free") +
      theme(strip.text = element_blank())
  }
}
# p0 <- make_plot(d, x = "rr", lo = "lo", up = "up")

##---- table ----
make_table <- function(dt, x, lbl,
                       size = 11,
                       id = "id",
                       id2 = NULL,
                       h = 1, # hjust in geom_text and scale, in [0,1]
                       ...){
  dt <- prep_tab(dt)
  # dt[, id] <- factor(dt[, id], rev(unique(dt[, id]))) ## reverse order
  long_d <- reshape2::melt(dt[, c(id, id2, x)], id.vars = c(id, id2) )
  d0 <- ggplot(long_d, aes_string(x = "variable", y = id, label = "value")) +
    geom_text(hjust = h, ...) +
    scale_x_discrete(position = "top", labels = lbl) +
    theme(panel.background = element_blank(), # option
          axis.text.y = element_blank(),
          axis.text.x = element_text(size = size, colour = "black", hjust = h),
          axis.title = element_blank(),
          # axis.line.y = element_blank(),
          axis.ticks = element_blank())
  if (is.null(id2)) return(d0) else {
    d0 +
      facet_grid(id2,
                 scales = "free",
                 switch = "y",
                 space = "free") +
      theme(strip.placement = "outside",
            strip.text.y.left = element_text(size = size,
                                         angle = 0,
                                         hjust = 0,
                                         vjust = 1),
        # panel.spacing= unit(0, "lines"),
        panel.background = element_rect(),
        panel.grid = element_blank(),
        strip.background = element_rect()
        # strip.text = element_text(size = size, colour = "black")
      )
  }
}
# t1 <- make_table(d, x =  c('var', 'events'), lbl = c('Group', 'Events'))
# t2 <- make_table(d, x = c('rr_ci'), lbl = c('RR (95%CI)'), h = 0.5)
# cowplot::plot_grid(t1, NULL, p0, t2,
#                    nrow = 1,
#                    align = "h", rel_widths = c(.4, -.1, .2, .4))
# cowplot::plot_grid(t1, NULL, t2, p0, nrow = 1,
#                    align = "h", rel_widths = c(1, -.2, 1, .5))

##---- stop ----
if (FALSE){
  ##---- join ----
  plot1 <- cowplot::plot_grid(make_table(d), make_plot(d), align = "h", rel_widths = c(1, 1))

  ##---- add group ----
  groups = c("Group 1", "Group 2")
  d1 <- d
  d1$group <- sample(groups, nrow(d1), replace = TRUE)
  ## might be simpler wih tidyverse
  {
    a <- by(d1, d1$group, identity)
    aa <- unlist( lapply(a, function(x) list(NA, x)) , recursive = FALSE)
    d2 <- do.call(rbind, aa )
    d2[ which(is.na(d2$var)), "var"] <- names(a) ## fiddly
    rownames(d2) <- NULL
  }
  plot2 <- cowplot::plot_grid(make_table(d2),
                              make_plot(d2),
                              align = "h", rel_widths = c(1, 1))

  ##---- by model ----
  d3 <- make_data(seed = 456)
  plot3 <- cowplot::plot_grid(make_table(d3), make_plot(d3),
                              align = "h", rel_widths = c(1, 1))

  #' Add title to a cowplot object
  #'
  #' @param p the cowplot object
  #' @param title title
  #' @param rel_heights relative heights between the title and the cowplot object
  #'
  #' @return the plot with title added in
  #' @export
  cowplot_title <- function(p, title, rel_heights = c(0.1, 1)) {
    title <- cowplot::ggdraw() +
      cowplot::draw_label(title, fontface = "bold")
    cowplot::plot_grid(title, p, ncol =1 , rel_heights = rel_heights)
  }

  plot4 <- cowplot::plot_grid(cowplot_title(plot1, "Model 1"),
                              cowplot_title(plot3, "Model 2"),
                              ncol = 1)


  ##---- by model 2 ----
  d5 <- rbind(cbind(d, model = "model1"), cbind(d3, model = "model2") )

  t5 <- make_table(d5, id2 = "model")

  p5 <- make_plot(d5, dodgew = 0.3, co = "model")

  plot5 <- cowplot::plot_grid(t5,
                              p5,
                              align = "h", axis = "bt",
                              rel_widths = c(1, .5))

}
