# =================================
#        PRINCIPAL COMPONENTS ANALYSIS      
#		   ( unsupervised linear projection method )		  
# =================================
pca = function(ndim)
{
	data_wine <- read.table( "wine.data", sep="," )
	
	# ----------------------------------------------------------------------------------------------		
	#	ignore the cultivar type class and take only feature #1, feature #2, ..., feature #13
	# ----------------------------------------------------------------------------------------------			
	matrix_features <- data_wine[, 2:14]

	# ---------------------------------------------------------	
	#  calculate the means of matrix_features' columns
	# ---------------------------------------------------------
	column_means <- colMeans( matrix_features )
	
	# -----------------------------------------------
	# create a matrix for mean-centered data
	# -----------------------------------------------	
	matrix_means_centered <- matrix_features
		
	# ----------------------------------------------------------------------------------		
	#   subtract the mean from matrix features to center the data on the origin
	# ----------------------------------------------------------------------------------
	for ( index in 1 : nrow( matrix_features ) ){
		matrix_means_centered[index, ] <- matrix_features[index, ] - column_means	
	}
	
	matrix_covariance <- matrix(0, ncol = ncol( matrix_features ), nrow = ncol( matrix_features ))
	
	# ----------------------------------------
	#       compute the covariance matrix 
	# ----------------------------------------
	for ( index_x in 1:ncol(matrix_features)  ){
		for (index_y in index_x:ncol(matrix_features)){
			if ( index_x == index_y ){
				matrix_covariance[ index_x, index_y ] = var( matrix_means_centered[, index_x] )			
			} else {
				matrix_covariance[ index_x, index_y ] = cov( matrix_means_centered[, index_x], matrix_means_centered[, index_y] )
				matrix_covariance[ index_y, index_x ] = matrix_covariance[ index_x, index_y ]			
			}
		}	
	}
	
	# -----------------------------------------------------
	#     compute the eigen values and eigen vectors
	# -----------------------------------------------------
	solution_eigen <- eigen( matrix_covariance )

	# ------------------------------------------------
	#     get the eigen values and eigen vectors
	# ------------------------------------------------	
	value_eigen <- solution_eigen$values
	vector_eigen <- solution_eigen$vectors
	
	# -------------------------------------------------------
	#     get the 'ndim' eigen values and eigen vectors
	# -------------------------------------------------------
	final_value_eigen   <- value_eigen[1:ndim]
	final_vector_eigen <- as.matrix(vector_eigen[, 1:ndim])
	
	# ----------------------------------------------------------------------------------------------
	#     transform the original data into new data in the reduced ndim-dimension space
	# ----------------------------------------------------------------------------------------------
	final_dataset <- t( final_vector_eigen ) %*% t( matrix_means_centered )
				
	return (t(final_dataset))
}

fld = function(ndim)
{
#	dataset <- pca( 13 )
#	print( dim(dataset) )

		
	# -----------------------------------------
	#            read the file 'wine_1.data'
	# -----------------------------------------
	data_wine_1 <- read.table( "wine_1.data", sep=",")
#	matrix_features_1 <- as.matrix( dataset[1:59,] )

	# ----------------------------------------------------------------------------------------------		
	#	ignore the cultivar type class and take only feature #1, feature #2, ..., feature #13
	# ----------------------------------------------------------------------------------------------			
	matrix_features_1 <- data_wine_1[, 2:14]		
	
	# ---------------------------------------------------------	
	#  calculate the means of matrix_features' columns
	# ---------------------------------------------------------
	column_means_1 <- as.matrix(colMeans( matrix_features_1 ))
	
	
	# -----------------------------------------
	#            read the file 'wine_2.data'
	# -----------------------------------------
	data_wine_2 <- read.table( "wine_2.data", sep=",")
#	matrix_features_2 <- as.matrix( dataset[60:130,] )	
	# ----------------------------------------------------------------------------------------------		
	#	ignore the cultivar type class and take only feature #1, feature #2, ..., feature #13
	# ----------------------------------------------------------------------------------------------			
	matrix_features_2 <- data_wine_2[, 2:14]
	# ---------------------------------------------------------	
	#  calculate the means of matrix_features' columns
	# ---------------------------------------------------------
	column_means_2 <- as.matrix(colMeans( matrix_features_2 ))

	
	# -----------------------------------------
	#            read the file 'wine_3.data'
	# -----------------------------------------
	data_wine_3 <- read.table( "wine_3.data", sep=",")
#	matrix_features_3 <- as.matrix( dataset[131:178,] )	
	# ----------------------------------------------------------------------------------------------		
	#	ignore the cultivar type class and take only feature #1, feature #2, ..., feature #13
	# ----------------------------------------------------------------------------------------------			
	matrix_features_3 <- data_wine_3[, 2:14]
	# ---------------------------------------------------------	
	#  calculate the means of matrix_features' columns
	# ---------------------------------------------------------
	column_means_3 <- as.matrix(colMeans( matrix_features_3 ))
		
	m_i <- cbind( column_means_1, column_means_2, column_means_3 )	
	
	
	# -----------------------------------------
	#            read the file 'wine.data'
	# -----------------------------------------
	data_wine <- read.table( "wine.data", sep=",")	
	matrix_features <- data_wine[, 2:14]
	dataset <- matrix_features
#    matrix_features <- dataset
	
	# -----------------------------------------
	#            calculate 'm'
	# -----------------------------------------	
	sum_total <- matrix(0, nrow=ncol(dataset), ncol=1)   
	for ( index in 1 : ncol( m_i ) ){
		sum_total <- sum_total + m_i[,index]
	}
	m <- sum_total / ncol(m_i)
	N_i <- cbind( nrow( matrix_features_1 ), nrow( matrix_features_2 ), nrow( matrix_features_3 ) )
	
	
	# -----------------------------------------
	#            calculate 'S_1'
	# -----------------------------------------
	S_1 <- matrix( 0, nrow=ncol(matrix_features_1), ncol = ncol(matrix_features_1) )
			
	for ( index in 1:nrow( matrix_features_1 ) ) {
		S_1 <- S_1 + (( t(as.matrix(matrix_features_1[index,])) -  as.matrix(m_i[, 1] ) )  %*%  t(  t(as.matrix( matrix_features_1[index,])) - as.matrix(m_i[, 1] )   ) )
#		S_1 <- S_1 + (( as.matrix(matrix_features_1[index,]) -  as.matrix(m_i[, 1] ) )  %*%  t(  as.matrix( matrix_features_1[index,]) - as.matrix(m_i[, 1] )   ) )		
	}
	

	# -----------------------------------------
	#            calculate 'S_2'
	# -----------------------------------------
	S_2 <- matrix( 0, nrow=ncol(matrix_features_2), ncol = ncol(matrix_features_2) )	
			
	for ( index in 1:nrow( matrix_features_2 ) ) {
		S_2 <- S_2 + (( t(as.matrix(matrix_features_2[index,])) -  as.matrix(m_i[, 2] ) )  %*%  t(  t(as.matrix( matrix_features_2[index,])) - as.matrix(m_i[, 2] )   ) )
#		S_2 <- S_2 + (( as.matrix(matrix_features_2[index,]) -  as.matrix(m_i[, 2] ) )  %*%  t( as.matrix( matrix_features_2[index,]) - as.matrix(m_i[, 2] )   ) )		
	}			
	
	# -----------------------------------------
	#            calculate 'S_3'
	# -----------------------------------------
	S_3 <- matrix( 0, nrow=ncol(matrix_features_3), ncol = ncol(matrix_features_3) )	
			
	for ( index in 1:nrow( matrix_features_3 ) ) {
		S_3 <- S_3 + (( t(as.matrix(matrix_features_3[index,])) -  as.matrix(m_i[, 3] ) )  %*%  t(  t(as.matrix( matrix_features_3[index,])) - as.matrix(m_i[, 3] )   ) )
#		S_3 <- S_3 + (( as.matrix(matrix_features_3[index,]) -  as.matrix(m_i[, 3] ) )  %*%  t(  as.matrix( matrix_features_3[index,]) - as.matrix(m_i[, 3] )   ) )
	}				
	
	# -----------------------------------------
	#            calculate 'S_W'
	# -----------------------------------------	
	S_W <- S_1 + S_2 + S_3	

	matrix_identity <- matrix(0, ncol=ncol(dataset), nrow=ncol(dataset) )
	for ( index_x in 1: nrow(matrix_identity) ) {
		for ( index_y in 1:ncol( matrix_identity ) ){
			if ( index_x == index_y )
				matrix_identity[index_x, index_y] <- 1		
		}	
	}
	
		
	# -----------------------------------------
	#            calculate 'S_B'
	# -----------------------------------------
	print( dim(as.matrix(m_i[,3])) )
	print( dim(m) )
	S_B <- matrix( 0, ncol = ncol(dataset), nrow = ncol(dataset) )		
	for ( index in 1:3 ){
		S_B <- S_B + ( N_i[index] * ( as.matrix(m_i[, index] ) - m ) %*% t(  as.matrix(m_i[, index] ) - m  ) )	
	}		
	
	cat("Dimension of S_B = ", dim( S_B ))
	print( S_B )
	
	# --------------------------------------------------------------
	#            calculate 'the inverse of S_W' multiplied by 'S_B'
	# --------------------------------------------------------------
	S_W_inverse_S_B <- solve( S_W, S_B )
	
	S_W_inverse <- solve( S_W, matrix_identity )

	# -----------------------------------------------------
	#     compute the eigen values and eigen vectors
	# -----------------------------------------------------
	solution_eigen <- eigen( S_W_inverse_S_B )

	# ------------------------------------------------
	#     get the eigen values and eigen vectors
	# ------------------------------------------------	
	value_eigen <- solution_eigen$values
	vector_eigen <- solution_eigen$vectors
	
	# -------------------------------------------------------
	#     get the 'ndim' eigen values and eigen vectors
	# -------------------------------------------------------
	final_value_eigen   <- value_eigen[1:ndim]
	final_vector_eigen <- as.matrix(vector_eigen[, 1:ndim])
	
	# ----------------------------------------------------------------------------------------------
	#     transform the original data into new data in the reduced ndim-dimension space
	# ----------------------------------------------------------------------------------------------
	final_dataset <- t( final_vector_eigen ) %*% t( matrix_features )	
		
	return (t(final_dataset))
}

# ========================================================
#        Provided that we have two arguments:
#     1. proj  --> a [#data]-by-2 matrix representing eigen vectors
#	   2. class --> a [#data]-by-1 matrix representing 'class' for each row in 'proj'
# ========================================================
graph = function(proj, class)
{
	proj <- Re(proj)
	plot( proj[,1], proj[,2], xlab="x", ylab="y", xlim = c( min(proj[,1]), max(proj[,1]) ), ylim=c(min(proj[,2]), max(proj[,2])), col="pink", lty=1, pch=21 )
	text( proj[,1], proj[,2], class, col="blue" )
}