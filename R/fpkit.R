## forest plot kit
# with shiny
# read a table_left, values for plot, and table_right
# set up options
# export plot

library(ggplot2)
## table left
k <- 4
tl <- expand.grid(paste("Model", 1:k), 
                  paste("Drug", LETTERS[1:2]),
                  KEEP.OUT.ATTRS = FALSE)
tl$Patients <- round( runif(nrow(tl), 1000, 1e4) )
tl$Events <- round( runif(nrow(tl), 50, 500) )
tl

long_tl <- reshape(tl, direction = "long",
                   varying = list(c("Patients", "Events")),
                   v.names = "value",
                   idvar = c("Var1", "Var2"),
                   times = c("Patients", "Events")
                   )
# long_d <- reshape(d, direction = "long",
#                   varying = list(c("events", "rr_ci")),
#                   v.names = "value",
#                   idvar ="var",
#                   times = c("events", "rr_ci") #, drop = c("rr", "lo", "up")
#                   )

# wide_tl <- reshape(tl, 
#                     idvar = "Var1",
#                     timevar = "Var2",
#                     direction = "wide" )
# wide_tl

d0 <- ggplot(long_tl, aes_string(x = "time", y = "Var1", label = "value")) +
  geom_text() +
  facet_wrap("Var2")
  # scale_x_discrete(position = "top", labels = lbl) +
  # theme(panel.background = element_blank(),
  #       axis.text.y = element_text(size = size, colour = "black"), # element_blank(), 
  #       axis.text.x = element_text(size = size, colour = "black"),
  #       axis.title = element_blank(),
  #       # axis.line.y = element_blank(),
  #       axis.ticks = element_blank())