#With this code you can specify the expected returns, the standard deviations and the 
#correlations of 2 assets, and it will generate a given number (n) of beta values.
#With these informations, the program will plot you the n portfolios in a coordinate system.

#INPUTS of the 2 assets in the portfolio and the number of betas to be generated
#Expeced returns
e1=0.1
e2=0.15
#Standard deviations
sd1=0.2
sd2=0.4
#Correlation of the 2 assets
correl=-0.5
#How many random betas (portfolios) need to be generated?
n = 10;

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


#GENERATING the betas
betas = runif(n);


#PLOTTING the portfolios with the generated betas
xaxis=sigma(betas);
yaxis=expected(betas);
plot(xaxis,yaxis,
xlab="Sigma (risk)", ylab="Expected return",
xlim=c(sd1-0.1,sd2+0.005), ylim=c(e1-0.005,e2+0.005))
		#with these numbers you can adjust how much of the x axis is shown