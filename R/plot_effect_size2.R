## test generic forest plot
# https://www.bmj.com/content/362/bmj.k3851
dpot <- data.frame(
  type = c("All cancers", "Colorectal", "Breast", "Bladder"),
  e = c(1.09, 1.46, 0.85, 0.66),
  lo = c(0.85, 0.79, 0.42, 0.15),
  up = c(1.41, 2.73, 1.73, 2.89),
  n = c(302, 51, 36, 8) )

## utils
##- force given order
prep_tab <- function(dt = d1){
  a <- LETTERS[1:nrow(dt)]
  dt[, "id_"] <- factor(a, rev(a))
  dt
}
# dpot2 <- prep_tab(dpot)
paste_e_ci <- function(e, lo, up,
                       lb = "(", rb = ")",
                       sep = " to "){
  sapply(1:length(e), function(i){
    paste( e[i], paste0(lb, lo[i], sep, up[i], rb ) )
  })
}
dpot$e_ci <- with(dpot, paste_e_ci(e, lo, up))

##- simple plot
{ dt = dpot; e = "e"; y = "type"; lo = "lo"; up = "up"; co = NULL; dodgew = 0; labx = NULL }
make_plot <- function(dt = dpot,
                      e, ## estimate effect size
                      y, ## categorical
                      lo , up ,
                      log = TRUE,
                      labx = NULL,
                      co = NULL,
                      dodgew = 0,
                      size = 11){
  require(ggplot2)
  dt <- prep_tab(dt) ## index of reverse order
  pos <- position_dodge(width = dodgew)
  p0 <- ggplot(dt, aes_string(x = e, y = "id_",
                              colour = co))
  p1 <- p0 +
    geom_point(position = pos, shape = 15 ) +
    geom_linerange(aes_string(xmin = lo, xmax = up),
                   position = pos,
                   show.legend = FALSE) + ## error bars
    geom_vline(xintercept = 1, linetype = "dotted") + ## reference
    xlab(labx) + theme_classic()

  if (log){
    p1 <- p1 + scale_x_log10()
  }
    p2 <- p1 +
      scale_y_discrete(labels = dt[ order(dt[, "id_"]), y] ) + ## labels
      theme(axis.text.y = element_text(size = size, colour = "black"), # element_blank(),
            axis.text.x = element_text(size = size, colour = "black"),
            axis.line.y = element_blank(),
            axis.title.y = element_blank(),
            axis.ticks.y = element_blank())
    p2
}

## table
make_table <- function(dt,
                       x = c("e_ci", "n"), ## vector covar
                       lbl = c("HR (95%CI)", "No events"),
                       y = "type",
                       id2 = NULL,
                       size = 11
){
  dt <- prep_tab(dt) ## index of reverse order
  long_d <- reshape2::melt(dt[, c("id_", id2, x)], id.vars = c("id_", id2) )
  p1 <- ggplot(long_d, aes_string(x = "variable", y = "id_", label = "value")) +
    geom_text()
  p1 +
    scale_y_discrete(labels = dt[ order(dt[, "id_"]), y] ) + ## labels
    scale_x_discrete(position = "top", labels = lbl) +
    theme(panel.background = element_blank(),
          axis.text.y = element_text(size = size, colour = "black"), # element_blank(),
          axis.text.x = element_text(size = size, colour = "black"),
          axis.title = element_blank(),
          # axis.line.y = element_blank(),
          axis.ticks = element_blank())

}


# source("R/plot_effect_size.R")
p1 <- make_plot(dt = dpot,
               e = "e", y = "type",
               lo = "lo", up = "up")
# p + theme(axis.text.y = element_blank())

tab1 <- make_table(dt = dpot,
                   x = c("e_ci", "n"),
                   lbl = c("HR (95%CI)", "No of\nevents"),
                   y = "type",
                   id2 = NULL,
                   size = 11 )

tab1.1 <- make_table(dt = dpot,
                   x = c("e_ci"),
                   lbl = c("HR (95%CI)"),
                   y = "type",
                   id2 = NULL,
                   size = 11 )
tab1.2 <- make_table(dt = dpot,
                     x = c("n"),
                     lbl = c("No of\nevents"),
                     y = "type",
                     id2 = NULL,
                     size = 11 )

plot1 <- cowplot::plot_grid( tab1, # + theme(plot.margin = unit(c(0, -1, 0, 0), "cm")), # tlbr
                            p1 + theme(axis.text.y = element_blank()),
                            align = "h", rel_widths = c(2, 1))

plot2 <- cowplot::plot_grid(p1,
                            tab1 + theme(axis.text.y = element_blank()),
                            align = "h", rel_widths = c(1, 2))

## decrease margins
plot2.2 <- cowplot::plot_grid(p1,
                            tab1 + theme(axis.text.y = element_blank(),
                                         plot.margin = unit(c(0,0,0,0), "cm")), ##
                            align = "h", rel_widths = c(1, 2))

## decrease margins 2

{
  plot3 <- cowplot::plot_grid(
    p1,
    tab1.1  + theme(axis.text.y = element_blank(),
                    plot.margin = unit(c(0,0,0,0), "cm")), ##
    tab1.2  + theme(axis.text.y = element_blank(),
                    plot.margin = unit(c(0,0,0,0), "cm")), ##
    nrow = 1,
    align = "h", rel_widths = c(1.5, 1.2, .5))
  plot3
}

## decrease margins 3
{
  plot4 <- cowplot::plot_grid(
    p1,
    NULL, ##
    tab1.1  + theme(axis.text.y = element_blank(),
                    plot.margin = unit(c(0,0,0,0), "cm")),
    NULL, ##
    tab1.2  + theme(axis.text.y = element_blank(),
                    plot.margin = unit(c(0,0,0,0), "cm")),
    nrow = 1,
    align = "h", rel_widths = c(1.5, 0, 1.2, -0.2, .5)) ##
  plot4
}

# ggsave("./figs/plot4.pdf", plot3, width = 5, height = 2)


