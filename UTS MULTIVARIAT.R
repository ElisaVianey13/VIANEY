#Nomor 1
#corr=cov(xi,xj)/(sqrt(var(xi)*sqrt(var(xj))))

X1 = matrix(c(12,18,14,20,16,17,20,16,18,19,29,38,30,38,35), nrow = 5, ncol = 3)
X1

rerata = data.frame(x1_rerata1 = mean(X1[,1]),x1_rerata2 =mean (X1[,2]), x1_rerata3 = mean(X1[,3]))
rerata

X1_1 = (X1[,1] - rerata$x1_rerata1)^2
X1_2 = (X1[,2] - rerata$x1_rerata2)^2
X1_3 = (X1[,3] - rerata$x1_rerata3)^2
X1_1_2 = (X1[,1] - rerata$x1_rerata1)*(X1[,2] - rerata$x1_rerata2)
X1_1_3 = (X1[,1] - rerata$x1_rerata1)*(X1[,3] - rerata$x1_rerata3)
X1_2_3 = (X1[,2] - rerata$x1_rerata2)*(X1[,3] - rerata$x1_rerata3)

squareX = data.frame(X1_1, X1_2,X1_3,X1_1_2,X1_1_3,X1_2_3)

SS_matriks = data.frame(SSX1_1 = sum(X1_1), SSX1_2 = sum(X1_2), SSX1_3 = sum(X1_3), SSX1_1_2=sum(X1_1_2), SSX1_1_3 = sum(X1_1_3), SSX1_2_3 = sum(X1_2_3))


var_X1_1 = (1/(5-1))*SS_matriks$SSX1_1
var_X1_2 = (1/(5-1))*SS_matriks$SSX1_2
var_X1_3 = (1/(5-1))*SS_matriks$SSX1_3

cov_X1_2 =(1/(5-1))*SS_matriks$SSX1_1_2
cov_X1_3 =(1/(5-1))*SS_matriks$SSX1_1_3
cov_X2_3 =(1/(5-1))*SS_matriks$SSX1_2_3

korr12 = cov_X1_2/sqrt(var_X1_1*var_X1_2)
korr13 = cov_X1_3/sqrt(var_X1_1*var_X1_3)
korr23 = cov_X2_3/sqrt(var_X1_1*var_X1_3)
#LANGSUNG
cov(X1) 
cor(X1) 

#Nomor 2
X2 = matrix(c(2,8,6,8,12,9,9,10), nrow = 4, ncol = 2)
X2

miu = matrix(c(7,11), nrow = 2, ncol =1)
miu
xbar = matrix(c(xbar1 = mean(X2[,1]), xbar2 = mean(X2[,2])))
xbar
varX1 = 1/(4-1)*sum((X2[,1]- xbar[1,1])^2)
varX1
varX2 = 1/(4-1)*sum((X2[,2]- xbar[2,1])^2)
varX2
covX12=1/(4-1)*sum((X2[,1]- xbar[1,1])*(X2[,2]- xbar[2,1]))
covX12

cov(X2)
Thit = nrow(X2)*t(xbar-miu)%*%solve(cov(X2))%*%(xbar - miu)
Thit
Ftabel = qf(.95, df1 =2, df2=(4-2))
Ftabel
Daerahkritis=(4-1)/(4-2)*2*Ftabel
Daerahkritis

#gagal tolak H0 karena Thitung kurang dari daerah kritis

#Nomor 3

library(dplyr)
manv = read.delim("clipboard", header = TRUE)
manv
resmanv = manova(cbind(X1,X2)~Perlakuan, data=manv)
summary(resmanv, test = "Wilks")

library(dplyr)
library(tidyr)
library(magrittr)

attach(manv)
detach(manv)

perl1_bar = matrix(c(mean(filter(manv,Perlakuan == "perl1")[,1]), mean(filter(manv, Perlakuan =="perl1")[,2])))
perl2_bar = matrix(c(mean(filter(manv,Perlakuan == "perl2")[,1]), mean(filter(manv, Perlakuan =="perl2")[,2])))
perl3_bar = matrix(c(mean(filter(manv,Perlakuan == "perl3")[,1]), mean(filter(manv, Perlakuan =="perl3")[,2])))
perl1_bar
perl2_bar
perl3_bar

perl_bar = matrix(c(mean(manv[,1]), mean(manv[,2])))
perl_bar 
#observasi1
SSobs1 = sum(manv[,1]*manv[,1])
SSobs1
mean1 = matrix(perl_bar[1,1], nrow (manv))
mean1
SSmean1 = sum(mean1^2)
SSmean1

treatmen11 = matrix(perl1_bar[1,1] - perl_bar[1,1], nrow (filter(manv, Perlakuan =="perl1")))
treatmen12 = matrix(perl2_bar[1,1] - perl_bar[1,1], nrow (filter(manv, Perlakuan =="perl2")))
treatmen13 = matrix(perl3_bar[1,1] - perl_bar[1,1], nrow (filter(manv, Perlakuan =="perl3")))
treatmen11
treatmen12
treatmen13
treatmen1 = c(treatmen11, treatmen12, treatmen13)
treatmen1
treatmen1^2
SStr1 = sum(treatmen1^2)
SStr1

res1_perl1 = filter(manv, Perlakuan =="perl1")[,1]- perl1_bar[1,1]
res1_perl2 = filter(manv, Perlakuan =="perl2")[,1]- perl2_bar[1,1]
res1_perl3 = filter(manv, Perlakuan =="perl3")[,1]- perl3_bar[1,1]
res1_perl1
res1_perl2
res1_perl3
res1= c(res1_perl1, res1_perl2, res1_perl3)
res1
SSres1 = sum(res1^2)
SSres1

SSmean1+SStr1+SSres1
Tot_cor1 = SSobs1 - SSmean1
Tot_cor1

SSobs1 = sum(manv[,1]*manv[,1])
SSobs1

#observasi 2
SSobs2 = sum(manv[,2]*manv[,2])
SSobs2
mean2 = matrix(perl_bar[2,1], nrow (manv))
mean2
SSmean2 = sum(mean1^2)
SSmean2

treatmen21 = matrix(perl1_bar[2,1] - perl_bar[2,1], nrow (filter(manv, Perlakuan =="perl1")))
treatmen22 = matrix(perl2_bar[2,1] - perl_bar[2,1], nrow (filter(manv, Perlakuan =="perl2")))
treatmen23 = matrix(perl3_bar[2,1] - perl_bar[2,1], nrow (filter(manv, Perlakuan =="perl3")))
treatmen21
treatmen22
treatmen23
treatmen2 = c(treatmen21, treatmen22, treatmen23)
treatmen2
treatmen2^2
SStr2 = sum(treatmen2^2)
SStr2

res2_perl1 = filter(manv, Perlakuan =="perl1")[,2]- perl1_bar[2,1]
res2_perl2 = filter(manv, Perlakuan =="perl2")[,2]- perl2_bar[2,1]
res2_perl3 = filter(manv, Perlakuan =="perl3")[,2]- perl3_bar[2,1]
res2_perl1
res2_perl2
res2_perl3
res2= c(res2_perl1, res2_perl2, res2_perl3)
res2
SSres2 = sum(res2^2)
SSres1

SSmean2+SStr2+SSres2
Tot_cor2 = SSobs2 - SSmean2
Tot_cor1

#crossproduct
mean12 = sum(mean1*mean2)
mean12
treat12 = sum(treatmen1*treatmen2)
treat12
res12 = sum(res1*res2)
res12
obs12 = sum(manv[,1]*manv[,2])
mean12+treat12+res12

Total_corrected = obs12 - mean12
Total_corrected

matriks_W_res= matrix(c(SSres1, res12, res12, SSres2), nrow = 2, ncol= 2)
matriks_W_res
matriks_B_treat= matrix(c(SStr1, treat12, treat12, SStr2), nrow = 2, ncol= 2)
matriks_B_treat
matriks_total = matrix(c(Tot_cor1, Total_corrected, Total_corrected, Tot_cor2), nrow =2, ncol =2)
matriks_total
matriks_W_res+matriks_B_treat

#Wilklambda
wilks_lambda = det(matriks_W_res)/det(matriks_W_res + matriks_B_treat)
wilks_lambda
stat_uji = ((nrow(manv) -3-1)/(3-1)*(1- sqrt(wilks_lambda))/sqrt(wilks_lambda))
stat_uji
Ftabel3 = qf(0.95, df1 = (2*(3-1)), df2 = (2*(nrow(manv)-3-1)))
Ftabel3
##tolak H0, stat uji > Ftabel

#Barlett
stat_ujibarlet = -(nrow(manv)-1-((2+3)/2))*log(wilks_lambda)
stat_ujibarlet
chisqtabel = qchisq(.95, df = 2*(3-1))
chisqtabel
#tolak H0, stat uji > chisqtabel
