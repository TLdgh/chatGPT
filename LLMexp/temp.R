#RCD
trt_levels = rep(c("F1","F2","F3","Control"),times= 6)
data.frame(plant=1:24, Treatment=trt_levels)%>%mutate(Assign=sample(trt_levels))














levels=factor(1:4)
x=data.frame(levels=rep(levels,6),Obs=c(90.3,92.5,85.5,82.5,89.2,89.5,90.8,89.5,
                                        98.2,90.6,89.6,85.6,93.9,94.7,86.2,87.4,
                                        87.4,87,88,78.9,97.9,95.8,93.4,90.7))%>%
  mutate(Block=rep(1:6,each=4))%>%mutate(Block=as.factor(Block))
fit=lm(Obs~levels+Block, x)
anova(fit)
qqnorm(fit$residuals)
qqline(fit$residuals, lwd=3)
plot(density(fit$residuals))





# RCBD Example data
trt_levels=rep(c("M1", "M2","M3","M4"), times = 6) #there are 6 blocks
data.frame(
  Block = as.factor(rep(1:6, each=4)), #there are 4 treatment levels
  Treatment = trt_levels
)%>%group_by(Block)%>%mutate(Assign=sample(Treatment))%>%print(n=24)


# Perform ANOVA
fit2 <- lm(Response ~ Treatment + Block, data = data)
anova(fit2)
