# ====================================
#        Lab #5         : Support Vector Machines
#        Name 			: Hendra Bunyamin
#        Student ID 	: 20105439
# ====================================
library(corpcor);
library(quadprog);

svm = function(filename)
{
	# read the data from filename
	data = read.table(filename)
	# data points: (data$x, data$y)
	# data class : data$c
	
	# =============================================
	#         our instance --> x1, x2, r
	#         vector w = the transpose of (w0, w1, w2)
	#                    w0 is a raw number
	#                     w = (  w1   w2  ) with w1 and w2 are raw numbers
	# =============================================
	
	# ==============================================
	#      Initialize all variables which represent the features and the classes
	#          (the purpose of this step is to make our code readable)
	# ==============================================
	x1 <- data$x
	x2 <- data$y
	r    <- data$c
	
	# ===============================================
	#      My assumption is 
	#          the classes are always 1 and 2
	#      Therefore, I am going to change the classes into (1 and -1)
	#
	#      Change the dataset class :
	#           class   1   --> class 1
	#           class   2   --> class -1
 	# ===============================================	
	for ( index in 1:length(r) ) {
		if ( r[index] == 2 )
			r[index] <- -1
	}
	
	# ===================================================
	#      Construct matrix_A
	#          transpose of matrix_A[row,] = (   r[row]     x1[row]     x2[row]   )
	#                  is a (number of Instances) by ( number of features + 1 ) matrix
 	# ===================================================		
	matrix_A <- cbind( rep(1, length(r)), x1, x2 )
	for ( index in 1:length(r) ){
	      matrix_A[index,] <- r[index] * matrix_A[index,]
	}	
	matrix_A <- t(matrix_A)	
	
	# =====================================
	#      Construct vector b0
	#          vector b is a (number of instances) by 1 vector
 	# =====================================
	b0 <- matrix( 1, length(r), 1 )
	
	# =====================================================
	#      Construct matrix Dmat
	#          the size of this matrix is (number of features) x (number of features)
 	# =====================================================
	Dmat <- matrix(0, ncol(data), ncol(data))
	Dmat[,2] <- c(0,1,0)
	Dmat[,3] <- c(0,0,1)
	
	# -----------------------------------------------
	#      make 'Dmat' a positive definite matrix
	# -----------------------------------------------		
	Dmat <- make.positive.definite(Dmat)
	
	# =====================================================
	#      Construct vector d
	#          the size of this matrix is (number of features + 1) x 1
 	# =====================================================	
	d <- matrix(0, ncol(data), 1)
	
	# ======================================
	# compute parameters w and w0 using svm algorithm
	# ======================================	
	SVM_result <- solve.QP( Dmat, d, matrix_A, bvec = b0 )
	# -----------------------------
	#      get the paramaters
	# -----------------------------
	SVM_parameters <- SVM_result$solution	

	# ---------------------------------------------------
	#      assign the paramaters to new variables
	# ---------------------------------------------------
	w0 <- SVM_parameters[1]
	w <- as.matrix(c( SVM_parameters[2], SVM_parameters[3] ))
	
	# =========================================
	#      Separate the instances based on the classes (1 and -1)
 	# =========================================		
	isFound = FALSE
	indexNegative = 0
	r <- as.matrix(r)
	index <- 1
	while ( index <= length(r) && !isFound ) {
		if ( r[index] == -1 ) {
			indexNegative = index
			isFound = TRUE
		}			
		index = index + 1
	}
	x1_positive   <- x1[1:(indexNegative-1)]
	x2_positive <- x2[1:(indexNegative-1)]
	
	x1_negative <- x1[indexNegative:length(r)]
	x2_negative <- x2[indexNegative:length(r)]
				
	# visualize the decision boundary and indicate the support vectors
	x1range <- seq(min(x1), max(x1), length = 100)		
	x2range <- seq(min(x2), max(x2), length = 100)			
	
	plot(0, 0, type="n", xlab="x1", ylab="x2", 
	                 ylim = c(min(x2), max(x2)),
					 xlim = c(min(x1), max(x1)),
					 main = "Support Vector Machines")	

	# ==================================
	#      Lay out all points in the cartesian coordinates
 	# ==================================	
	points( x1_positive, x2_positive, col="blue", pch=21 )
	points( x1_negative, x2_negative, col="red", pch=21 )	
	
	# ==========================================
	#      Draw a line which separates the dataset into two classes
 	# ==========================================
	abline( c( -1 * ( w0 / w[2] ), -1 * ( w[1] / w[2] ) ) )

	# ==================================================
	#      Collect the support vectors
	#          support vectors are instances which have
	#               ( r[index] * ( w0 + x1[index] * w1 + x2[index] * w2 ) ) <= 1
 	# ==================================================
	support_vectors <- NULL	
	for ( index in 1:nrow(data) ){
		temp <- r[index] * ( w0 + w[1] * data[index,1] + w[2] * data[ index,2 ] )					
		if ( (temp-1.0) <= 0.001 ){
			support_vectors <- rbind( support_vectors, c( data[index,1], data[index,2] ) )	
		}	
	}
	
	# ======================
	#      Plot all the support vectors
 	# ======================	
	points( support_vectors[,1], support_vectors[,2], col="orange", pch=20 )	
	
	# return the parameters w0 and w
	c(w0, w)
}

twonorm = function(filename)
{
	# read the data from filename
	data = read.table(filename)
	
	# =============================================
	#         our instance --> x1, x2, r
	#         vector w = the transpose of (w0, w1, w2)
	#                    w0 is a raw number
	#                     w = (  w1   w2  ) with w1 and w2 are raw numbers
	# =============================================
	
	# =================================================
	#      Initialize all variables which represent the features and the classes
	#          (the purpose of this step is to make our code readable)
	# =================================================
	x1 <- data$x
	x2 <- data$y
	r    <- data$c	
		
	# ===============================================
	#      My assumption is that
	#          the classes are always 1 and 2
	#      Therefore, I am going to change the classes into (1 and 0)
	#
	#      Change the dataset class :
	#           class   1   --> class 1
	#           class   2   --> class 0
 	# ===============================================	
	for ( index in 1:length(r) ) {
		if ( r[index] == 2 )
			r[index] <- 0
	}	
		
	# =========================================
	#		 Preprocessing:
	#      Separate the instances based on the classes (1 and 0)
 	# =========================================		
	isFound = FALSE
	indexZero = 0
	r <- as.matrix(r)
	index <- 1
	while ( index <= length(r) && !isFound ) {
		if ( r[index] == 0 ) {
			indexZero = index
			isFound = TRUE
		}			
		index = index + 1
	}
	x1_one   <- x1[1:(indexZero-1)]
	x2_one   <- x2[1:(indexZero-1)]
	
	x1_zero <- x1[indexZero:length(r)]
	x2_zero <- x2[indexZero:length(r)]	
	
	# ======================
	#		Estimate PHI parameter
 	# ======================	
	sum_class_one <- 0
	for ( index in 1:length(r) ){
		sum_class_one <- sum_class_one + r[index]
	}	
	PHI <- sum_class_one / length(r)
#	cat( "PHI = ", PHI, "\n" )
	
	# ======================
	#		Estimate MU for class 1 
 	# ======================
	sum_class_one_x <- matrix(0, 2, 1)
	for ( index in 1:length(x1_one) ) {
		sum_class_one_x <- sum_class_one_x + as.matrix(c( x1_one[index], x2_one[index] ))
	}
	MU_one <- sum_class_one_x / sum_class_one
		
	# ======================
	#		Estimate MU for class 0
 	# ======================	
	sum_class_zero_x <- matrix(0, 2, 1)
	for ( index in 1:length( x1_zero ) ) {
		sum_class_zero_x <- sum_class_zero_x + as.matrix(c( x1_zero[index], x2_zero[index] ))
	}
	# because it is a binary classification, 
	# the number of instances in class zero is the total number of instances - the number of instances in class one 
	MU_zero <- sum_class_zero_x / (length(r) - sum_class_one) 
	
	# ==============================
	#		Estimate Covariance Matrix parameters
 	# ==============================	
	
	# --------------------------------------
	#        Covariance matrix of class 1
	# --------------------------------------	
	X_one <- cbind( x1_one, x2_one )
	total_sum <- 0
	for ( index in 1:length(x1_one) ) {
		total_sum <- total_sum + (as.matrix(X_one[index,]) - MU_one) %*% t( as.matrix(X_one[index,]) - MU_one )
	}
	covariance_matrix_one <- total_sum / length(x1_one)
	
	# --------------------------------------
	#        Covariance matrix of class 0
	# --------------------------------------	
	X_zero <- cbind( x1_zero, x2_zero )
	total_sum <- 0
	for ( index in 1:length(x1_zero) ) {
		total_sum <- total_sum + (as.matrix(X_zero[index,]) - MU_zero) %*% t( as.matrix(X_zero[index,]) - MU_zero )
	}
	covariance_matrix_zero <- total_sum / length(x1_zero)	
	
	# ===================================
	#		Pool all the instances and estimate a common 
 	# ===================================
	covariance_matrix <- PHI * covariance_matrix_one + (1 - PHI) * covariance_matrix_zero
		
	# compute parameters w and w0 using parametric classification
	
	# =========================================================
	#		Implement the Linear Discriminant algorithm based on the text book (page 97)
 	# =========================================================	
	w_one  <- solve( covariance_matrix ) %*% MU_one
	w_zero <- solve( covariance_matrix ) %*% MU_zero
	
	w_one_zero  <- -0.5 * t(MU_one) %*% solve( covariance_matrix ) %*% MU_one + log( PHI )
	w_zero_zero <- -0.5 * t(MU_zero) %*% solve( covariance_matrix ) %*% MU_zero + log( 1-PHI )

	# ========================================================
	#		Pool all the instances and estimate a common covariance matrix for all classes
 	# ========================================================
	w <- w_one - w_zero
	w0 <- w_one_zero - w_zero_zero
							
	# visualize the decision boundary
	x1range <- seq(min(x1), max(x1), length = 100)		
	x2range <- seq(min(x2), max(x2), length = 100)			
	
	# =========================
	#		Display the cartesian coordinates
 	# =========================
	plot(0, 0, type="n", xlab="x1", ylab="x2", 
	                 ylim = c(min(x2), max(x2)),
					 xlim = c(min(x1), max(x1)),
					 main = "Parametric Classification Using Two Norms")	

	# ==================================
	#      Lay out all points in the cartesian coordinates
 	# ==================================	
	points( x1_one, x2_one, col="blue", pch=21 )
	points( x1_zero, x2_zero, col="red", pch=21 )
	
	# ==========================================
	#      Draw a line which separates the dataset into two classes
 	# ==========================================
	abline( c( -1 * ( w0 / w[2] ), -1 * ( w[1] / w[2] ) ) )
		
	# return the parameters w0 and w
	c(w0, w)
}

test = function()
{
	print(svm("data.dat"))
	dev.new()
	print(twonorm("data.dat"))
}
