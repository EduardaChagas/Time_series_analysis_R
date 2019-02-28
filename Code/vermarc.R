#Lendo os dados
X <- read.csv(url("https://pkgstore.datahub.io/core/global-temp/annual_csv/data/a26b154688b061cdd04f1df36e4408be/annual_csv.csv"))

#Filtrando a série temporal
X <- X[,3]

#old.par <- par(mfrow=c(1, 3))

#Plotando a série temporal
#timeSeries(X)

#Plotando os padrões
patternsOnGraph(X,3,1,1,1)

#Plotando o histograma de probabilidade
histogram(X,3,1)

#Plano entropia complexidade
partitionMPR(X,3,1,1)

#par(old.par)