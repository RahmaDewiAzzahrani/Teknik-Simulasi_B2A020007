#RAHMA DEWI AZZAHRANI/B2A020007

#Z0=11123
#a=35
#m=138
#c=437
#n=100
#p=0,65

Additive_RNG<-function(a,z0,c,m,n) {
  xi<-matrix(NA,n,3)
  colnames(xi)<-c("aZ(i-1)+c","Xi","Ui")
  for (i in 1:n)
  {
    xi[i,1]<-(a*z0+c)
    xi[i,2]<-xi[i,1]%%m
    xi[i,3]<-xi[i,2]/m
    z0<-xi[i,2]
  }
  hist(xi[,3])
  View(xi)
}
Additive_RNG(35,11123,437,138,100)

Bernouli_2<-function(n,p) {
  i<-n
  p<-p
  X<-runif(i)
  Y<-(X<=p)+0
  (tabel<-table(Y)/length(Y))
}
Bernouli_2(100,0.65)
Bernouli_2(30,0.65)
Bernouli_2(107,0.65)
Bernouli_2(42,0.65)
Bernouli_2(113,0.65)
Bernouli_2(114,0.65)
Bernouli_2(11,0.65)
Bernouli_2(132,0.65)
Bernouli_2(89,0.65)
Bernouli_2(102,0.65)
Bernouli_2(5,0.65)
Bernouli_2(60,0.65)
Bernouli_2(53,0.65)
Bernouli_2(84,0.65)
Bernouli_2(65,0.65)
Bernouli_2(90,0.65)
Bernouli_2(137,0.65)
Bernouli_2(126,0.65)
Bernouli_2(17,0.65)
Bernouli_2(66,0.65)
Bernouli_2(125,0.65)
Bernouli_2(120,0.65)
Bernouli_2(83,0.65)
Bernouli_2(30,0.65)
Bernouli_2(107,0.65)
Bernouli_2(42,0.65)
Bernouli_2(113,0.65)
Bernouli_2(114,0.65)
Bernouli_2(11,0.65)
Bernouli_2(132,0.65)
Bernouli_2(89,0.65)
Bernouli_2(102,0.65)
Bernouli_2(5,0.65)
Bernouli_2(60,0.65)
Bernouli_2(53,0.65)
Bernouli_2(84,0.65)
Bernouli_2(65,0.65)
Bernouli_2(90,0.65)
Bernouli_2(137,0.65)
Bernouli_2(126,0.65)
Bernouli_2(17,0.65)
Bernouli_2(66,0.65)
Bernouli_2(125,0.65)
Bernouli_2(120,0.65)
Bernouli_2(83,0.65)
Bernouli_2(30,0.65)
Bernouli_2(107,0.65)
Bernouli_2(42,0.65)
Bernouli_2(113,0.65)
Bernouli_2(114,0.65)
Bernouli_2(11,0.65)
Bernouli_2(132,0.65)
Bernouli_2(89,0.65)
Bernouli_2(102,0.65)
Bernouli_2(5,0.65)
Bernouli_2(60,0.65)
Bernouli_2(53,0.65)
Bernouli_2(84,0.65)
Bernouli_2(65,0.65)
Bernouli_2(90,0.65)
Bernouli_2(137,0.65)
Bernouli_2(126,0.65)
Bernouli_2(17,0.65)
Bernouli_2(66,0.65)
Bernouli_2(125,0.65)
Bernouli_2(83,0.65)
Bernouli_2(30,0.65)
Bernouli_2(107,0.65)
Bernouli_2(42,0.65)
Bernouli_2(113,0.65)
Bernouli_2(114,0.65)
Bernouli_2(11,0.65)
Bernouli_2(132,0.65)
Bernouli_2(89,0.65)
Bernouli_2(102,0.65)
Bernouli_2(5,0.65)
Bernouli_2(60,0.65)
Bernouli_2(53,0.65)
Bernouli_2(84,0.65)
Bernouli_2(65,0.65)
Bernouli_2(90,0.65)
Bernouli_2(137,0.65)
Bernouli_2(126,0.65)
Bernouli_2(17,0.65)
Bernouli_2(66,0.65)
Bernouli_2(125,0.65)
Bernouli_2(120,0.65)
Bernouli_2(83,0.65)
Bernouli_2(30,0.65)
Bernouli_2(107,0.65)
Bernouli_2(42,0.65)
Bernouli_2(113,0.65)
Bernouli_2(114,0.65)
Bernouli_2(11,0.65)
Bernouli_2(132,0.65)
Bernouli_2(89,0.65)
Bernouli_2(102,0.65)
Bernouli_2(5,0.65)
Bernouli_2(60,0.65)
Bernouli_2(53,0.65)








