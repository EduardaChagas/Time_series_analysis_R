library(DataDriven)

datasets <- c("Wichmann-Hill", "Marsaglia-Multicarry", "Super-Duper", "Mersenne-Twister", "Knuth-TAOCP-2002", "Knuth-TAOCP", "L'Ecuyer-CMRG")

#for(i in c(1:length(datasets))){
for(i in c(1:3)){
  results <- data.frame(Percentage = 0, Sum_BP = 0, Sum_DD = 0, Entropy_BP = 0, Entropy_DD = 0)
  
  set.seed(1234567890, kind = datasets[i])
  x1_7 <- runif(110000)
  x1_7 <- x1_7[-(1:10000)]
  
  x1_6 <- precision(x1_7,6)
  d1_6 <- distribution(x1_6,3,1)
  dd1_6 <- dataDriven(x1_6,3,1)
  e1_6 <- shannonNormalized(d1_6)
  es1_6 <- shannonNormalized(dd1_6)
  ev1_6 <- equalitiesValues(x1_6)
  results <- rbind(results,list(ev1_6,sum(d1_6), sum(dd1_6), e1_6, es1_6))
  
  x1_5 <- precision(x1_7,5)
  d1_5 <- distribution(x1_5,3,1)
  dd1_5 <- dataDriven(x1_5,3,1)
  e1_5 <- shannonNormalized(d1_5)
  es1_5 <- shannonNormalized(dd1_5)
  ev1_5 <- equalitiesValues(x1_5)
  results <- rbind(results,list(ev1_5,sum(d1_5), sum(dd1_5), e1_5, es1_5))
  
  x1_4 <- precision(x1_7,4)
  d1_4 <- distribution(x1_4,3,1)
  dd1_4 <- dataDriven(x1_4,3,1)
  e1_4 <- shannonNormalized(d1_4)
  es1_4 <- shannonNormalized(dd1_4)
  ev1_4 <- equalitiesValues(x1_4)
  results <- rbind(results,list(ev1_4,sum(d1_4), sum(dd1_4), e1_4, es1_4))
  
  x1_3 <- precision(x1_7,3)
  d1_3 <- distribution(x1_3,3,1)
  dd1_3 <- dataDriven(x1_3,3,1)
  e1_3 <- shannonNormalized(d1_3)
  es1_3 <- shannonNormalized(dd1_3)
  ev1_3 <- equalitiesValues(x1_3)
  results <- rbind(results,list(ev1_3,sum(d1_3), sum(dd1_3), e1_3, es1_3))
  
  x1_2 <- precision(x1_7,2)
  d1_2 <- distribution(x1_2,3,1)
  dd1_2 <- dataDriven(x1_2,3,1)
  e1_2 <- shannonNormalized(d1_2)
  es1_2 <- shannonNormalized(dd1_2)
  ev1_2 <- equalitiesValues(x1_2)
  results <- rbind(results,list(ev1_2,sum(d1_2), sum(dd1_2), e1_2, es1_2))
  
  print(results)
  
}