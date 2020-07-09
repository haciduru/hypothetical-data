library(corpcor)

# The error term that we are going to add will depend on the direction and the 
# size of the current correlation coefficient. This function decides whether
# the error term will be positive or negative value depending on values of x, 
# X, R, and r. A positive error term will pull the value of y above the yhat
# (predicted value of y). A negative error term will do the opposite.
# Inputs:
#	x (float) a normal variable
#	X (vector) a vector of normally distributed variable
#	R (float) correlation coefficient (targetted)
#	r (float) correlation coefficient (current)
# Returns:
# 	1 or -1
vote4e = function(x, X, R, r) {

	# If the current correlation coefficient is positive ...
	if (r > 0) {
	
		# ... and the size of the current correlation coefficient is smaller than the target coefficient ...
		if (abs(R) > abs(r)) {
			if (x > mean(X)) {
				return(1)
			} else {
				return(-1)
			}
		# Else ...
		} else {
			if (x > mean(X)) {
				return(-1)
			} else {
				return(1)
			}
		}
	# Else ...
	} else {
		if (abs(R) > abs(r)) {
			if (x > mean(X)) {
				return(-1)
			} else {
				return(1)
			}
		} else {
			if (x > mean(X)) {
				return(1)
			} else {
				return(-1)
			}
		}
	}

}

# This function creates two normally distributed random variables that are correlated with each other. 
# Inputs:
#	n (integer) number of cases
#	r (float) bivariate correlation between the variables.
# Returns:
# 	A (n X 2) matrix
create_xy = function(n, R) {

	# Initialize the first variable.
	X = round(matrix(rnorm(n)), 2)
	
	# Initialize the second variable. At this time, there is perfect
	# correlation between the two variables.
	Y = X * R
	
	# The following loop adds n new cases to the data. Every time a new case is
	# added at the bottom, one case is dropped from the top. 
	for (i in 1:n) {

		# Find the linear regression equation and the current correlation coefficient.
		X = matrix(cbind(1, X), ncol=2)
		b = solve(t(X) %*% X) %*% t(X) %*% Y
		r = cor(X[,2], Y)

		# Initialize x (the first variable) for the new case. 
		x = rnorm(1)
		
		# Calculate the predicted value of y (the second variable) based on the regression equation.
		y = b[1] + b[2] * x

		# Get a random normal value to add as the error term.
		e = vote4e(x, X, R, r) * abs(rnorm(1))
		
		# Add the error term to the predicted value
		y = y + e

		# Add the new x and y values to the bottom of the dataset.
		# Also, drop the first row of the dataset.
		X = matrix(X[,2])
		X = rbind(matrix(X[-1]), matrix(x))
		Y = rbind(matrix(Y[-1]), matrix(y))
		
	}
	
	return(cbind(X, Y))

}

# This function creates normally distributed random variables given the correlation matrix Rs. 
# Inputs:
#	n (integer) number of cases
#	Rs (matrix) zero order correlation matrix
# Returns:
# 	A (n X ncol(Rs)) matrix
create_xyz = function(n, Rs) {

	# Create the first two variables.
	data = create_xy(1000, Rs[1,2])

	# If the matrix has more than two columns, then we want to create more than two variables.
	# The following loop creates variables 3 to ncol(Rs)--ncol(Rs) is the number of variables 
	# that we want to create.
	k = 2
	while (k < ncol(Rs)) {
	
		k = k + 1
		
		# Find the partial correlations between the new variable and the existing variables.
		pRs = cor2pcor(Rs[,1:k][1:k,])

		# Calculate the predicted values of the new variable given the partial correlations.
		Yhat = 0
		for (i in 1:(k-1)) {
			Yhat = Yhat + data[,i] * pRs[i,k]
		}
		data = cbind(data, matrix(Yhat))

		# Finally, adjust the value of the new variable by adding an error term. Each existing 
		# variable will vote for the direction of the error term. If 1's are more than -1's, then
		# a positive normal value will be added. If -1's are more than 1's, then a negative normal 
		# value will be added. If the number of 1's and -1's are equal, no error term will be added
		# to the predicted value. The result of vote4e function decides the direction of the error term.
		i = 0
		while (i < nrow(data)) {
			i = i + 1
			e = abs(rnorm(1))
			ehat = 0
			j = 0
			while (j < (k-1)) {
				j = j + 1
				ehat = ehat + vote4e(data[i,j], data[,j], Rs[j,k], cor(data[,j], data[,k]))
			}
			data[i,k] = data[i,k] + ehat * e
		}

	}
	
	return(data)
}
