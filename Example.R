library(stp25samplesize)

sample_size("cor", drop.out.rate = .10)
sample_size("aov")
sample_size("lm")
sample_size("aov", treatment = 2 , f = .5)
sample_size("anova", treatment = 2, f = .5)
sample_size("anova", treatment = 2, control = 2 * 2, f = .5)
sample_size("anova", treatment = 1, measurements = 4, f = 0.608)


 convert(d=.30, digits= 4)
convert(r=.14834)
convert(r2=.0220)
convert(f=.1500 )
convert(f2=.0225)
convert(eta2=.0220)
convert(ods=1.7231)
convert(log.ods=0.5441)


