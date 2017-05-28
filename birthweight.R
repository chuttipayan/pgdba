birth= read.csv("d1.csv")

birth$LOW=as.factor(birth$LOW)
birth$RACE=as.factor(birth$RACE)
birth$hyperten=as.factor(birth$hyperten)
birth$SMOKE=as.factor(birth$SMOKE)
#birth$preLab=as.factor(birth$preLab)

library(caTools)
set.seed(101) 
sample = sample.split(birth$LOW, SplitRatio = .75)
#id=sample(1:nrow(birth),0.75*nrow(birth))
#train = birth[id,]
#test  = birth[-id,]
# no need to sample 

birthmodel=glm(LOW~. -BWT,data=birth,family=binomial(link="logit"))

birthmodel_probit=glm(LOW~. -BWT,data=birth,family=binomial(link="probit"))

probs=predict(birthmodel,birth,type="resp")

pred=prediction(probs,birth$LOW)

prf1 <- performance(pred, measure = "acc")

# rpp rate of positive response 
prf2 <- performance(pred,"lift","rpp")
prf3 <- performance(pred, measure = "tpr", x.measure = "fpr")

par(mfrow=c(1,3))
plot(prf1)
plot(prf2)

plot(prf3)

table(birth$LOW,probs >0.55)

library(ROCR)