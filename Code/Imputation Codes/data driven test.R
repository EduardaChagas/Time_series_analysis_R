precision <-function(x, casas){
  x = round(x*(10^casas))
  x
}

#Gera 100.000 dados 
set.seed(1234567890, kind = "Mersenne-Twister")
x <- runif(110000)
x <- x[-(1:10000)]


#64 bits ou precisão dupla (double), equivalente a até 15 dígitos decimais.
a1 = precision(x,15)
ea1 = equalitiesValues(a1)
dd_a1 = dataDriven(a1,3,1)
ha1 = shannonNormalized(dd_a1)

#32 bits ou precisão simples (float), equivalente a até 7 dígitos decimais.
a2 = precision(x,7)
ea2 = equalitiesValues(a2)
dd_a2 = dataDriven(a2,3,1)
ha2 = shannonNormalized(dd_a2)

#6 dígitos decimais.
a3 = precision(x,6)
ea3 = equalitiesValues(a3)
dd_a3 = dataDriven(a3,3,1)
ha3 = shannonNormalized(dd_a3)

#5 dígitos decimais.
a4 = precision(x,5)
ea4 = equalitiesValues(a4)
dd_a4 = dataDriven(a4,3,1)
ha4 = shannonNormalized(dd_a4)

#4 dígitos decimais.
a5 = precision(x,4)
ea5 = equalitiesValues(a5)
dd_a5 = dataDriven(a5,3,1)
ha5 = shannonNormalized(dd_a5)

#3 dígitos decimais.
a6 = precision(x,3)
ea6 = equalitiesValues(a6)
dd_a6 = dataDriven(a6,3,1)
ha6 = shannonNormalized(dd_a6)
  
#2 dígitos decimais.
a7 = precision(x,2)
ea7 = equalitiesValues(a7)
dd_a7 = dataDriven(a7,3,1)
ha7 = shannonNormalized(dd_a7)

#1 dígitos decimais.
a8 = precision(x,1)
ea8 = equalitiesValues(a8)
dd_a8 = dataDriven(a8,3,1)
ha8 = shannonNormalized(dd_a8)

d1 = distribution(a1,3,1)
d2 = distribution(a2,3,1)
d3 = distribution(a3,3,1)
d4 = distribution(a4,3,1)
d5 = distribution(a5,3,1)
d6 = distribution(a6,3,1)
d7 = distribution(a7,3,1)
d8 = distribution(a8,3,1)
h1 = shannonNormalized(d1)
h2 = shannonNormalized(d2)
h3 = shannonNormalized(d3)
h4 = shannonNormalized(d4)
h5 = shannonNormalized(d5)
h6 = shannonNormalized(d6)
h7 = shannonNormalized(d7)
h8 = shannonNormalized(d8)
