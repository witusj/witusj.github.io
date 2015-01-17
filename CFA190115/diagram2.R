library(diagram)
names <- c("KT4", "KT5", "KT6", "KT7", "PT8", "", "DATA TOOL", "", "KT1", "RAPPORTAGE")
a = "METADATA"
b = "DATA"
c = "INFO"

M1 <- matrix(nrow = 10, ncol = 10, byrow = TRUE, data = c(
  # 4  5  6   7   8   E   D   E   1   R
    0, 0, 0,  0,  0,  0,  0,  0,  0, 0, #4
    0, 0, 0,  0,  0,  0,  0,  0,  0, 0, #5
    0, 0, 0,  0,  0,  0,  0,  0,  0, 0, #6
    0, 0, 0,  0,  0,  0,  0,  0,  0, 0, #7
    0, 0, 0,  0,  0,  0,  0,  0,  0, 0, #8
    0, 0, 0,  0,  0,  0,  0,  0,  0, 0, #E
    b, b, b,  b,  b,  0,  0,  0,  a, 0, #D
    0, 0, 0,  0,  0,  0,  0,  0,  0, 0, #E
    0, 0, 0,  0,  0,  0,  0,  0,  0, 0, #1
    0, 0, 0,  0,  0,  0,  c,  0,  0, 0  #R
    ))
#
dg1 <- plotmat(M1, pos = c(5,1,1,2,1),
                  curve = 0,
                  name = names,
                  lwd = 1,
                  box.lwd = 2,
                  cex.txt = 0.75,
                  cex.main = 2,
                  dtext = 0.1,
                  box.size = c(0.05,0.05,0.05,0.05,0.05,0,0.05,0,0.05,0.1),
                  box.type = "square",
                  box.prop = c(0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.2),
                  arr.type = "triangle",
                  arr.pos = 0.6,
                  arr.length = 0.1,
                  shadow.size = 0.01,
                  prefix = "",
                  relsize = 1,
                  main = "")
#
