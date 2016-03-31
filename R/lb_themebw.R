lb_theme_bw = function(x_or = NULL, y_or = NULL, xsize_t = NULL, ysize_t = NULL, xsize = NULL, ysize= NULL,tsize = NULL, title = NULL, frame = NULL) {

  themeMod <- theme_bw() + theme(axis.title.x = element_text(margin = margin(8, 0, 0, 0)) ,axis.title.y = element_text(margin = margin(0, 8, 0, 0)) )

  if(length(x_or == 1)) {themeMod = themeMod + theme(axis.text.x = element_text(angle = x_or))}
  if(length(y_or == 1)) {themeMod = themeMod + theme(axis.text.y = element_text(angle = x_or))}
  if(length(xsize_t == 1)) {themeMod = themeMod + theme(axis.title.x = element_text(size = xsize_t))}
  if(length(ysize_t == 1)) {themeMod = themeMod + theme(axis.title.y = element_text(size = ysize_t))}
  if(length(xsize == 1)) {themeMod = themeMod + theme(axis.text.x = element_text(size = xsize_t))}
  if(length(ysize == 1)) {themeMod = themeMod + theme(axis.text.y = element_text(size = ysize_t))}
  if(length(tsize == 1)) {themeMod = themeMod + theme(title = element_text(size = tsize))}
  if(length(frame == 1)) {themeMod = themeMod + theme(plot.background = element_rect(colour = "black"))}
  Sys.setlocale("LC_ALL","English")
  themeMod

}
