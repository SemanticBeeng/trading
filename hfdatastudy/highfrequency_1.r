library(highfrequency)

data(realized_library); #Get sample daily Realized Volatility data
DJI_RV = realized_library$Dow.Jones.Industrials.Realized.Variance; #Select DJI
DJI_RV = DJI_RV[!is.na(DJI_RV)]; #Remove NA's
DJI_RV = DJI_RV['2008']

x = harModel(data=DJI_RV , periods = c(1,5,22), RVest = c("rCov"),
               type="HARRV",h=1,transform=NULL);
class(x)
x
summary(x)
plot(x)

data("sample_5minprices_jumps");
data = sample_5minprices_jumps[,1];
data = makeReturns(data); #Get the high-frequency return data
x = harModel(data, 
             periods = c(1,5,10), 
             periodsJ=c(1,5,10),
             RVest = c("rCov","rBPCov"), 
             type="HARRVCJ",
             transform="sqrt");


