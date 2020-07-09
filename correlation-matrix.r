

# This function creates a hypothetical correlation matrix.
create_correlation_matrix = function(n) {

	m = matrix(rnorm(n*n), ncol=n)
	for (i in 1:(n-1)) {
		for (j in i:n) {
			m[i,j] = mean(c(m[i,j], m[j,i]))
			m[j,i] = m[i,j]
		}
	}
	sdr = sd(m)
	for (i in 1:length(m)) { m[i] = m[i] / abs(sdr) / 4 }
	for (i in 1:n) { m[i,i] = 1 }
	return(m)

}

