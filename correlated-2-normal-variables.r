# This function creates two normally distributed random variables that are correlated with each other. 
# Inputs:
#	N (integer) number of cases
#	R (float) bivariate correlation between the variables.
# Returns:
# 	A (N X 2) matrix

create_data = function(N, R) {

	# Initialize the first variable.
	x = round(matrix(rnorm(N)), 2)
	
	# Initialize the second variable. At this time, there is perfect
	# correlation between the two variables.
	y = x * R
	
	# The following loop adds N new cases to the data. Every time a new case is
	# added at the bottom, one case is dropped from the top. 
	for (i in 1:N) {

		# Find the linear regression equation and the current correlation coefficient.
		X = matrix(cbind(1, x), ncol=2)
		b = solve(t(X) %*% X) %*% t(X) %*% y
		r = cor(x, y)
		
		# Initialize x (the first variable) for the new case. 
		x_new = rnorm(1)
		
		# Then, calculate the predicted value of y (the second variable) based on 
		# the regression equation. Also, get a random normal value to add as the error term.
		yhat = b[1] + b[2] * x_new
		e = abs(rnorm(1))

		# The error term that we are goint to add will depend on the direction and the size of 
		# the current correlation coefficient. If it is positive, run the following section, else 
		# run below.
		if (r > 0) {
			# The correlation coefficient is positive. Thus, run this section.
			# If the absolute value of the current correlation coefficient is weaker than 
			# the target coefficient, we need to decrease the variance. That means, the error 
			# term must pull the new y closer to the regression line. Otherwise, we need to 
			# increase the variance, which means the error term must push the new y variable 
			# further from the regression line. 
			if (abs(R) > abs(r)) {
				# The current correlation coefficient is weaker than expected. Therefore, 
				# decrease the variance. 
				if (x_new > mean(x)) {
					y_new = yhat + e
				} else {
					y_new = yhat - e
				}
			} else {
				# The current correlation coefficient is weaker than expected. Therefore, 
				# increase the variance. 
				if (x_new > mean(x)) {
					y_new = yhat - e
				} else {
					y_new = yhat + e
				}
			}
		} else {
			# The correlation coefficient is negative. Thus, run this section.
			# If the absolute value of the current correlation coefficient is weaker than 
			# the target coefficient, we need to decrease the variance. That means, the error 
			# term must pull the new y closer to the regression line. Otherwise, we need to 
			# increase the variance, which means the error term must push the new y variable 
			# further from the regression line. 
			if (abs(R) > abs(r)) {
				# The current correlation coefficient is weaker than expected. Therefore, 
				# decrease the variance. 
				if (x_new > mean(x)) {
					y_new = yhat - e
				} else {
					y_new = yhat + e
				}
			} else {
				# The current correlation coefficient is weaker than expected. Therefore, 
				# increase the variance. 
				if (x_new > mean(x)) {
					y_new = yhat + e
				} else {
					y_new = yhat - e
				}
			}
		}
		
		# Add the new x and y values to the bottom of the dataset. Also, drop the first 
		# row of the dataset.
		x = rbind(matrix(x[-1]), round(matrix(x_new), 2))
		y = rbind(matrix(y[-1]), round(matrix(y_new), 2))
		
	}
	
	return(cbind(x, y))

}

# Run the following lines to test the function.
# data = create_data(100, .3)
# cor(data)
