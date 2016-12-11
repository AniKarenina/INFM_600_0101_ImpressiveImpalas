#Relationhip between 'Agricultural Exports' and 'Number of Tornadoes' + 'F-Scale' for Mid-West
d=read.csv(file.choose())
s1=subset(d,Region="Mid-West")
head(s1)
m=lm(s1$Sum.of.Agricultural.Exports ~ s1$Sum.of.Count + s1$Average.of.Average.of.F.Scale2)
summary(m)
summary(lm(s1$Sum.of.Agricultural.Exports ~ s1$Sum.of.Count))

#F-scale below 1 for TX
d2=read.csv(file.choose())
summary(lm(d3$Agricultural.Exports ~ d3$Number.of.Tornadoes + d3$Average.F.Scale.Below.1))
summary(lm(d3$Agricultural.Exports ~ d3$Number.of.Tornadoes))
summary(lm(d3$Agricultural.Exports ~ d3$Average.F.Scale.Below.1))

#F-scale >=2 for TX
d3=read.csv(file.choose())
summary(lm(d3$Agricultural.Exports ~ d3$Number.of.Tornadoes + d3$Average.F.Scale.Greater.or.Equal.to.2))
summary(lm(d3$Agricultural.Exports ~ d3$Number.of.Tornadoes))
summary(lm(d3$Agricultural.Exports ~ d3$Average.F.Scale.Greater.or.Equal.to.2))
