##- forest plot basic - Stephane
##---- libs ----
library(ggplot2)
library(reshape2)
library(cowplot)

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
  rr  <-  exp( rnorm(m) ) ## sort of RR
  de <-  runif(m)*rr
  lo <-  rr - de
  up <-  rr + de
  events <- round( n / de^2 ) ## let's pretend
  rr_ci <- paste(round(rr, k), 
                 paste0("(", round(lo, k), sep, round(up, k), ")") )
  data.frame(var, events, rr, lo, up , rr_ci )
}
( d <- make_data(seed = 123) )


##---- plot ----
make_plot <- function(dt = d, 
                      x = "rr", y = "var", 
                      lo = "lo", up = "up", 
                      labx = "RR",
                      co = NULL,
                      dodgew = 0){
  dt$var <- factor(dt$var, rev(unique(dt$var))) ## reverse order
  pos <- position_dodge(width = dodgew)
  p0 <- ggplot(dt, aes_string(x, y, 
                              colour = co))
  p1 <- p0 +
    geom_point(position = pos, shape = 15 ) + 
    geom_linerange(aes_string(xmin = lo, xmax = up),
                   position = pos,
                   show.legend = FALSE) + ## error bars
    geom_vline(xintercept = 1, linetype = "dotted") + ## reference
    xlab(labx) +
    theme_classic() +
    theme(
      # axis.text.x = element_text(size = 12,colour = "black"),
      axis.text.y = element_blank(), 
      axis.title.y = element_blank(),
      # axis.line.y = element_blank(),
      axis.ticks.y = element_blank(),
      legend.position = "top",
      legend.title= element_blank())
  p1
}
p0 <- make_plot()

##---- table ----
make_table <- function(dt = d, size = 11, 
                       id = "var",
                       id2 = NULL,
                       x = c('events', 'rr_ci'),
                       lbl = c("n", "RR (95%CI)")){
  dt$var <- factor(dt[, id], rev(unique(dt[, id]))) ## reverse order
  long_d <- reshape2::melt(dt[, c(id, id2, x)], id.vars = c(id, id2) )
  d0 <- ggplot(long_d, aes_string(x = "variable", y = id, label = "value")) +
    geom_text() +
    scale_x_discrete(position = "top", labels = lbl) +
    theme(panel.background = element_blank(),
          axis.text.y = element_text(size = size, colour = "black"), # element_blank(), 
          axis.text.x = element_text(size = size, colour = "black"),
          axis.title = element_blank(),
          # axis.line.y = element_blank(),
          axis.ticks = element_blank())
  if (is.null(id2)) return(d0) else {
    d0 +
    facet_wrap(id2) +
      theme(panel.background = element_blank(),
            strip.placement = "outside",
            strip.background = element_blank(),
            strip.text = element_text(size = size, colour = "black"))
  }
}
d0 <- make_table()

##---- join ----
plot1 <- cowplot::plot_grid(d0, p0, align = "h", rel_widths = c(1, 1))

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
