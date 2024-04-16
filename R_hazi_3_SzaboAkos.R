#With this code you can specify the expected returns, the standard deviations and the correlation 
#of 2 assets. In addition, the expected return of the risk free asset, and 3 beta_1 values for 
#3 portfolios. The code will plot all the different portfolios on a line that can be created form
#the 2 assets, plus the 3 given portfolios with red dots. It also shows the opportunity lines if you
#can lend/borrow at the given risk free r0 rate with 3 different colored (red, green, blue) lines.

#INPUTS of the 2 assets in the portfolio:
#Expeced returns:
e1 = 0.12
e2 = 0.08
#Standard deviations:
sd1 = 0.08
sd2 = 0.05
#Correlation of the 2 assets:
correl = 0.2

#Risk free expected return:
r0 = 0.05
#The 3 portfolios' beta_1 values from the exercise
mybetas = c(0.5, 0.25, 0.75)


#MAKING the vector e, and the matrix v based on the inputs:
e = matrix(c(e1,e2), 2, 1);
v11 = sd1^2;
v12 = correl * sd1 * sd2;
v21 = v12;
v22 = sd2^2;
v = matrix(c(v11,v12,v21,v22), 2, 2);


#FUNCTIONS to calculate the risk and the expected return of the portfolio based on a beta_1 value:
sigma <- function(beta){
s <- beta * beta * v[1,1] + (1-beta) * (1-beta) * v[2,2] + 2 * beta * (1-beta) * v[1,2];
sqrt(s)
}

expected <- function(beta){
beta1 = beta;
beta2 = (1-beta);
ex <- beta1 * e[1,1] + beta2 * e[2,1]
}


#PLOTTING all possible portfolios based on the different weights, from beta_1=0 to beta_1=1
betas = seq(0, 1, 0.01);
xaxis = sigma(betas);
yaxis = expected(betas);

#USE THIS plot if you would like to see the WHOLE PICTURE with the x and y axis
plot(xaxis, yaxis, type="l", xlab="Sigma (risk)", ylab="Expected return", xlim=c(0, max(sd1,sd2)), ylim=c(0, max(e1,e2)))

#USE THIS plot if you would like to see the portfolios from UP CLOSE
#plot(xaxis, yaxis, type="l", xlab="Sigma (risk)", ylab="Expected return")

#USE THIS plot if you would like to see the TANGENCY PORTFOLIO from UP CLOSE
#plot(xaxis, yaxis, type = "l", xlab="Sigma (risk)", ylab="Expected return", xlim=c(sigmapitangency-0.03, sigmapitangency+0.03), ylim=c(epitangency-0.03,epitangency+0.03))



#PLOTTING the x and y AXIS, the RISK FREE asset, and the 3 given PORTFOLIOS with black dots
abline(h = 0)
abline(v = 0)
points(0, r0, pch=19)
points(sigma(mybetas), expected(mybetas), pch=19)


#PLOTTING the 3 different lines for the case, when we can LEND/BORROW at an r0 interest rate
xx1=c(0, sigma(mybetas[1]));
yy1=c(r0, expected(mybetas[1]));
abline(lm(yy1~xx1), col="red", lwd=2)

xx2=c(0, sigma(mybetas[2]));
yy2=c(r0, expected(mybetas[2]));
abline(lm(yy2~xx2), col="green", lwd=2)

xx3=c(0, sigma(mybetas[3]));
yy3=c(r0, expected(mybetas[3]));
abline(lm(yy3~xx3), col="blue", lwd=2)


#Calculating the K value, and making the CML function
onematrix=matrix(c(1,1),2,1);
K = t(e - r0 * onematrix) %*% solve(v) %*% (e - r0 * onematrix)

cml <- function(sigmapi){
epi <- sigmapi * c(sqrt(K)) + r0
}

#PLOTTING the CML with black dashed line
sigmapis = seq(0, max(sd1,sd2)+5, 0.01);
lines(sigmapis, cml(sigmapis), lwd=2, lty='dashed')


#Calculating and plotting the TANGENCY PORTFOLIO with a black square
C = t(onematrix) %*% solve(v) %*% onematrix
A = t(onematrix) %*% solve(v) %*% e

epitangency = r0 + K / (C * (A/C - r0));
sigmapitangency = (epitangency - r0) / sqrt(K);
points(sigmapitangency, epitangency , pch=15)

