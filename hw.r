#Input data:
n = 3;
e = matrix(c(0.15,0.13,0.08), n, 1);
sdvector = c(0.40,0.35,0.25);
correl = matrix(c(1,0.4,0.3,
			0.4,1,0.5,
			0.3,0.5,1), n, n);
r0 = 0.05;

diagsd=diag(sdvector);
v = t(diagsd) %*% correl %*% diagsd;

onevector = matrix(c(1,1,1), n, 1);

#Restricted case
A = t(onevector) %*% solve(v) %*% e;
B = t(e) %*% solve(v) %*% e;
C = t(onevector) %*% solve(v) %*% onevector;
D = B * C - A * A

A = A[1,1];
B = B[1,1];
C = C[1,1];
D = D[1,1];

g = (B * solve(v) %*% onevector - A * solve(v) %*% e) / D;
h = (C * solve(v) %*% e - A * solve(v) %*% onevector) / D;

#Extended case
K = t(e - r0 * onevector) %*% solve(v) %*% (e - r0 * onevector);
K = K[1,1];

h_tilda = (solve(v) %*% (e - r0 * onevector)) / K;
g_tilda = -r0 * h_tilda;


#THE TASK:
y = 0.14

#Restricted case's weights (beta1,...,n):
betas = g + y * h;

#Extended case's weights (beta1,...,n):
betas_tilda = g_tilda + y * h_tilda;
#Plus the rest in risk free (beta0)
beta0 = 1 - sum(betas_tilda);


#Checking the results:
#These should be equal to y (0.14):
check_res = sum(betas * e); #Restricted
check_ext = sum(betas_tilda * e) + beta0 * r0; #Extended

#---------------------------------------------------------

sigma <- function(betas){
betasmatrix = matrix(betas,3,1);
variance = t(betasmatrix) %*% v %*% betasmatrix;
sqrt(variance)
}

expected <- function(betas){
betasmatrix = matrix(betas,3,1);
ex = t(betasmatrix) %*% e;
ex
}

xaxis=c()
yaxis=c()
for (i in 1:5000){
random_betas = runif(3)
normalized_betas = random_betas / sum(random_betas)
xaxis = append(xaxis,sigma(normalized_betas));
yaxis = append(yaxis,expected(normalized_betas));
}

plot(xaxis,yaxis,xlab="Sigma (risk)", ylab="Expected return", pch=20)


