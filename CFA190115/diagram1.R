library(diagram)
names <- c("KT1-3", "KT4", "KT5", "KT6", "KT7", "PT8", "TOP3/3", "BONUS = SCR PT8")
a = "SCORE"
b = "VOLD"

M <- matrix(nrow = 8, ncol = 8, byrow = TRUE, data = c(
  # 1  4  5   6   7   8   T   B
    0, 0, 0,  0,  0,  0,  0,  0,  #1
    0, 0, 0,  0,  0,  0,  0,  0,  #4
    0, 0, 0,  0,  0,  0,  0,  0,  #5
    0, 0, 0,  0,  0,  0,  0,  0,  #6
    0, 0, 0,  0,  0,  0,  0,  0,  #7
    0, 0, 0,  0,  0,  0,  0,  0,  #8
    0, a, a,  a,  a,  0,  0,  0,  #T
    0, 0, 0,  0,  0,  b,  b,  0   #B
    ))
#
dg1 <- plotmat(M, pos = c(5, 2, 1),
                  curve = 0,
                  name = names,
                  lwd = 1,
                  box.lwd = 2,
                  cex.txt = 0.75,
                  cex.main = 2,
                  dtext = 0.1,
                  box.size = c(0.05,0.05,0.05,0.05,0.05,0.05,0.05,0.1),
                  box.type = "square",
                  box.prop = c(0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.2),
                  arr.type = "triangle",
                  arr.pos = 0.6,
                  arr.length = 0.1,
                  shadow.size = 0.01,
                  prefix = "",
                  relsize = 1,
                  main = "Bonussysteem")
#
