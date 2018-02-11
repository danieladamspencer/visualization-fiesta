left_right_footnote <- function(gg_object,
                                left_text = "Left",
                                right_text = "Right",
                                size = 0.7, 
                                color = "black"){
  require(grid)
  print(gg_object)
  pushViewport(viewport())
  grid.text(label = right_text ,
            x = unit(1,"npc") - unit(2, "mm"),
            y = unit(2, "mm"),
            just = c("right", "bottom"),
            gp = gpar(cex = size, col = color))
  popViewport()
  pushViewport(viewport())
  grid.text(label = left_text ,
            x = unit(0,"npc") + unit(2, "mm"),
            y = unit(2, "mm"),
            just = c("left", "bottom"),
            gp = gpar(cex = size, col = color))
  popViewport()
}