# Plot WoE patterns for variables
#
# Plots calculated WoE for every variable
# Modify iv.plot.woe() from tomasgreif/woe, by ordering the intervals, layouting the subplots, and adding the colour of bars
#
# @param iv WoE data for variables - output from iv.mult with summary=FALSE (false is default for iv.mult)
# @param cols number of subplots per row
# @export
# @examples
# iv.plot.woe(iv.mult(german_data,"gb"))
# iv.plot.woe(iv.mult(german_data,"gb",vars=c("ca_status","housing","age","duration"),summary=FALSE))

iv.plot.woe.new <- function(iv, cols) {
  library(ggplot2)
  
  woe_plot <- list()
  for(i in seq_along(iv)) {
    data <- iv[[i]]
    woe_plot[[i]] <- ggplot(data=data) + geom_bar(aes(y=woe,x=factor(class, levels=class)), fill="steelblue", stat="identity", position="identity") + 
      xlab(data[1,1]) +
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line(linetype="dashed",colour="grey"),
      panel.grid.minor = element_blank(),
      panel.background = element_blank(),
      axis.ticks.x = element_blank(),
      axis.ticks.y = element_blank(),
      axis.text.x = element_text(angle = 45, hjust = 1),
      plot.title = element_text(hjust = 0.5),
      legend.position = "none"
    ) 
  }
  
  # layout of subplots, here is four subplots per row
  library(grid)
  
  col_num <- cols
  num <- ifelse(length(iv) %% col_num == 0, 1, 2)
  row_num <- length(iv) %/% col_num + num
   
  grid.newpage()
  pushViewport(viewport(layout = grid.layout(row_num, col_num, heights=unit(c(1, rep(4, row_num - 1)), "null"))))
  vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)
  
  rowth <- 1
  colth <- 1
  for(i in seq_along(woe_plot)) {
    if(i %% col_num == 1) {
      rowth <- rowth + 1
      colth <- 1
    }
    print(woe_plot[[i]], vp=vplayout(rowth, colth))
    colth <- colth + 1
  }
  
  grid.text("Weight of Evidence (WoE) Patterns", vp = vplayout(1, 1:cols), gp = gpar(fontsize = 20))
}
