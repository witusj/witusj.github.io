# libraries
library(ggplot2)
library(corrgram)
library(MethComp)

# read file
HCV_ICL3 <- read.csv("HCV_ICL3.csv")
View(HCV_ICL3)

summary(HCV_ICL3)


x <- HCV_ICL3$ICL
y <- HCV_ICL3$BioS

# derive difference
mean(x)
mean(y)

# mean ICL - mean BioS
mean(x)-mean(y)

HCV_ICL3$diff <- x-y
HCV_ICL3$diff

# Because n=8 is small, the distribution of the differences should be 
# approximately normal. Check using a boxplot and QQ plot. 
# There is some skew.

boxHCV <- boxplot(HCV_ICL3$diff)
normHCV <- qqnorm(HCV_ICL3$diff)
lineHCV <- qqline(HCV_ICL3$diff)

# Shaphiro test of normality. 

shapHCV <- shapiro.test(HCV_ICL3$diff)

# The normality test gives p < 0.001, which is small, so we 
# reject the null hypothesis that the values are distributed normally. 


# Because results do not have a normal distribution, 
# the Mann-Whitney-Wilcoxon Test is used instead. 
# We can decide whether the population distributions are identical without 
# assuming them to follow the normal distribution.

wilcHCV <- wilcox.test(x, y, paired = TRUE)
# p > 0.05 and therefore the H0 is NOT rejected. 
# The two populations are identical.

# Just to see what happens in the Student T-test.
# A paired t-test: one sample, two tests
# H0 = no difference; H1 = mean of 2 tests are different
# mu= a number indicating the true value of the mean 
# (or difference in means if you are performing a two sample test).

studtHCV <- t.test(x, y, mu=0, paired=T, alternative="greater")
# p = 0.8425. Because p is larger than alpha, we do NOT reject H0.
# In other words, it is unlikely the observed agreements happened by chance.
# However, because the populations do not have a normal distribution, we can not use 
# the outcome if this test.


# Correlation

corHCV <- cor.test(x, y, 
           alternative = c("two.sided", "less", "greater"),
           method = c("pearson", "kendall", "spearman"),
           exact = NULL, conf.level = 0.95)


# drop first column
HCV_ICL3$Nr <- NULL


#ggplot
g <- ggplot(HCV_ICL3, aes(log(ICL), log(BioS)))


# add layers
g <- g + 
  geom_smooth(method="lm", se=TRUE, col="steelblue", size = 1) +
  geom_point(size = 3, aes(colour = x)) +
  scale_colour_gradient("IU/ml", high = "red", low = "blue", space = "Lab") +
  labs(y = "Reference lab (log IU/ml)") +
  labs(x = "Home lab (log IU/ml)") +
  theme_bw(base_family = "Helvetica", base_size = 14) +
  scale_x_continuous(breaks=c(0,8,10,12,14,16))

# geom_point(alpha = 1/3, size = 4) +

regmod <- lm(y~x, data=HCV_ICL3)
summary(regmod)

# convert table to datafram
df <- as.data.frame.matrix(HCV_ICL3) 


# Bland-Altman Analysis
BlandAltman(x, y,
            x.name = "Reference lab IU/ml",
            y.name = "Home lab IU/ml",
            maintit = "Bland-Altman plot for HCV count",
            cex = 1,
            pch = 16,
            col.points = "black",
            col.lines = "blue",
            limx = NULL,
            limy = NULL,
            ymax = NULL,
            eqax = FALSE,
            xlab = NULL,
            ylab = NULL,
            print = TRUE,
            reg.line = FALSE,
            digits = 2,
            mult = FALSE)


