#With this code you can specify the expected returns, the standard deviations and the 
#correlations of 2 assets, and it will plot all possible portfolios which can be created
#from them. You have the option to give it multiple correlation values, and with those
#given, the program will plot those portfolios also, with different colored "lines".

#INPUTS of the 2 assets in the portfolio:
#Expeced returns:
e1=0.1
e2=0.15
#Standard deviations:
sd1=0.2
sd2=0.4
#Correlation of the 2 assets:
correl=0.5

#If you want to see more colorful plots for different correlations,
	#fill up this vector (max 10 elements):
morecorrels=c(-0.8,-0.6,-0.3,0,0.2,1);


#MAKING the vector e, and the matrix v based on the inputs:
e=matrix(c(e1,e2),2,1);
v11=sd1^2;
v12=correl*sd1*sd2;
v21=v12;
v22=sd2^2;
v=matrix(c(v11,v12,v21,v22),2,2);


#FUNCTIONS to calculate the risk and the expected return of the portfolio:
sigma <- function(beta){
s <- beta*beta*v[1,1]+(1-beta)*(1-beta)*v[2,2]+2*beta*(1-beta)*v[1,2];
sqrt(s)}

expected <- function(beta){
beta1=beta;
beta2=(1-beta);
ex <- beta1*e[1,1]+beta2*e[2,1]}


#PLOTTING all possible portfolios based on the diffferent weights,
	#from beta1=0 to beta1=1 and 99 values in between
betas=seq(0,1,0.01);
xaxis=sigma(betas);
yaxis=expected(betas);
plot(xaxis,yaxis,
xlab="Sigma (risk)", ylab="Expected return",
xlim=c(sd1-0.15,sd2+0.005)) #with these numbers you can adjust how much of the x axis is shown


#PLOTTING more portfolios, if the 'morecorrels' vector has at least 1 element in it:
colors=c("red","green","blue","hotpink","lightblue","orange1","yellowgreen","salmon1","plum2","khaki1");

idx=1; #index for the colors

for (co in morecorrels)
{
v12=co*sd1*sd2;
v21=v12;
v=matrix(c(v11,v12,v21,v22),2,2); #recalculating the v matrix based on the new correlations
xaxis2=sigma(betas);
points(xaxis2,yaxis,col=colors[idx]) #plotting the new points, all with different colors
idx=idx+1;
}


#PRINTING to the console which colored dots represent which correlations
idx=1;

for (i in morecorrels)
{
  if(idx == 1)
  {
  cat("The # black # colored dots represent the correlation",correl,"\n")
  }
cat("The #",colors[idx],"# colored dots represent the correlation ",i,"\n")
idx=idx+1;
}
