d=read.csv("C:\\Users\\kavas\\Desktop\\Documents\\Courses\\600_AW\\Project\\Rplot\\Tor_Exp_StateWise_RPlot.csv")

#High Occurrence states
d.Texas=subset(d,d$State=="TX")
summary(lm(d.Texas$Agricultural.Exports~d.Texas$Count+d.Texas$Average.of.F.Scale))
summary(lm(d.Texas$Animal.Products~d.Texas$Count+d.Texas$Average.of.F.Scale))
summary(lm(d.Texas$Plant.Products~d.Texas$Count+d.Texas$Average.of.F.Scale))

d.NeMex=subset(d,d$State=="NM")
head(d.NeMex)
summary(lm(d.NeMex$Agricultural.Exports~d.NeMex$Count+d.NeMex$Average.of.F.Scale))

d.Ok=subset(d,d$State=="OK")
head(d.Ok)
summary(lm(d.Ok$Agricultural.Exports~d.Ok$Count+d.Ok$Average.of.F.Scale))

d.Ar=subset(d,d$State=="AR")
head(d.Ar)
summary(lm(d.Ar$Agricultural.Exports~d.Ar$Count+d.Ar$Average.of.F.Scale))

d.La=subset(d,d$State=="LA")
head(d.La)
summary(lm(d.La$Agricultural.Exports~d.La$Count+d.La$Average.of.F.Scale))

d.Az=subset(d,d$State=="AZ")
head(d.Az)
summary(lm(d.Az$Agricultural.Exports~d.Az$Count+d.Az$Average.of.F.Scale))

d.Ks=subset(d,d$State=="KS")
head(d.Ks)
summary(lm(d.Ks$Agricultural.Exports~d.Ks$Count+d.Ks$Average.of.F.Scale))

#Low Occurrence states
d.NJ=subset(d,d$State=="NJ")
summary(lm(d.NJ$Agricultural.Exports~d.NJ$Count+d.NJ$Average.of.F.Scale))

d.High=subset(d,d$Count>55)
nrow(d.High)
summary(lm(d.High$Agricultural.Exports~d.High$Count+d.High$Average.of.F.Scale))

d.High=subset(d,d$Average.of.F.Scale>=1)
summary(lm(d.High$Agricultural.Exports~d.High$Count+d.High$Average.of.F.Scale)
summary(lm(d$Agricultural.Exports~d$Count+d$Average.of.F.Scale))
cor.test(d$Agricultural.Exports,d$Count)

x=read.csv("C:\\Users\\kavas\\Desktop\\Documents\\Courses\\600_AW\\Project\\Rplot\\Tor_Exp_RegionWise_RPlot.csv")
#Regions:
x.MW=subset(x,x$Region=="Mid-West")
summary(lm(x.MW$Sum.of.Agricultural.Exports~x.MW$Sum.of.Count))
summary(lm(x.MW$Sum.of.Animal.Products~x.MW$Sum.of.Count))
summary(lm(x.MW$Sum.of.Plant.Products~x.MW$Sum.of.Count))

x.NE=subset(x,x$Region=="North-East")
summary(lm(x.NE$Sum.of.Agricultural.Exports~x.NE$Sum.of.Count))
summary(lm(x.NE$Sum.of.Animal.Products~x.NE$Sum.of.Count))
summary(lm(x.NE$Sum.of.Plant.Products~x.NE$Sum.of.Count))

x.SE=subset(x,x$Region=="South-East")
summary(lm(x.NE$Sum.of.Agricultural.Exports~x.SE$Sum.of.Count))
summary(lm(x.SE$Sum.of.Animal.Products~x.SE$Sum.of.Count))
summary(lm(x.SE$Sum.of.Plant.Products~x.SE$Sum.of.Count))

x.W=subset(x,x$Region=="Western")
summary(lm(x.NE$Sum.of.Agricultural.Exports~x.W$Sum.of.Count))
summary(lm(x.W$Sum.of.Animal.Products~x.W$Sum.of.Count))
summary(lm(x.W$Sum.of.Plant.Products~x.W$Sum.of.Count))
