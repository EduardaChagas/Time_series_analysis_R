library(BETS)
#Possible values for the parameter src
  #IBGE
  #Brazilian Institute of Geography and Statistics
  #BCB
  #Central Bank of Brazil
  #FGV
  #Getulio Vargas Foundation
  #FGv-IBRE
  #Getulio Vargas Foundation - Brazilian Institute of Economics
  #BCB e FGV
  #Central Bank of Brazil and Getulio Vargas Foundation
  #BCB-Deban
  #Cetral Bank of Brazil - Department of Banking and Payments
  #BCB-Depin
  #Central Bank of Brazil - Department of International Reserves
  #BCB-Derin
  #Central Bank of Brazil - Department of International Affairs
  #BCB-Desig
  #Central Bank of Brazil - Department of Financial Monitoring
  #BCB-Secre
  #Central Bank of Brazil - Executive Secretariat
  #BCB-Demab
  #Central Bank of Brazil - Department of Open Market Operations
  #BCB-Denor
  #Central Bank of Brazil - Department of Economics
  #Sisbacen
  #Central Bank of Brazil Information System
#Possible values for the parameter periodicity:
 #A - anual data
 #M - monthly data
 #Q - quaterly data
 #W - weekly data
SearchTimeSeries<-function(orignPlace,per){
  aux = BETS.search(src = orignPlace,periodicity = per)
  print(aux)
}

GetTimeSeries<-function(myCode){
  ts = BETS.get(code = myCode)
  ts
}