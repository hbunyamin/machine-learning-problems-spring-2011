## =============================
##		Lab #6 : Multilayer Perceptrons
##				Name : Hendra Bunyamin
##				Student ID : 20105439
## =============================

## This function will be used to grade your homework
## DO NOT MODIFY THIS FUNCTION
grading = function()
{
	w = bp()
	dev.new()
	showImageMatrix(matrix(w$W0[1:64,1], nrow = 8, ncol = 8))
	dev.new()
	showImageMatrix(matrix(w$W0[1:64,2], nrow = 8, ncol = 8))
	print(w)
	print(sum(w$W0) + sum(w$W1))
	err(w)
}

## Sigmoid function
sigmoid = function(x)
{
	1.0 / (1.0 + exp(-x))
}

## Performance measuring purpose
err = function(w)
{
	data = read.table("elf.data")
	x = matrix(0, nrow = 300, ncol = 64)
	for(i in 1:300) x[i,] = as.numeric(data[i,1:64])
	class = as.numeric(data$V65)
	r = matrix(c(1, 0, 0, 0, 1, 0, 0, 0, 1), nrow = 3, ncol = 3)
	
	err = 0.0
	for(i in 1:300)
	{
		y = sigmoid(c(sigmoid(c(x[i,], 1) %*% w$W0), 1) %*% w$W1)
		err = err + ((y - r[class[i],]) %*% t(y - r[class[i],]))
	}
	
	err = err / 2;
		
	print(paste("err: ", err))
}

## This function shows the ith instance in the given dataset as an image
showData = function(i)
{
	data = read.table("elf.data")
	img = matrix(as.numeric(data[i,1:64]), 8, 8)
	img = img[,ncol(img):1];
	color = rgb(seq(0, 1, length = 256), seq(0, 1, length = 256), seq(0, 1, length = 256))
	image(1:8, 1:8, img, col = color, zlim = c(min(img), max(img)), xlab = "", ylab = "", axes = FALSE)
}

## This function shows the input 8x8 matrix as an image
showImageMatrix = function(img)
{
	img = img[,ncol(img):1];
	color = rgb(seq(0, 1, length = 256), seq(0, 1, length = 256), seq(0, 1, length = 256))
	image(1:8, 1:8, img, col = color, zlim = c(min(img), max(img)), xlab = "", ylab = "", axes = FALSE)
}

## This skeleton is prepared based on Figure 11.11 in the textbook
## Please refer to Figure 11.11 for your code
bp = function()
{
	## DO NOT EDIT ANY GIVEN PARAMETER
	## Initialize weights and parameters
	## eta		: learning rate
	## w0		: a 65x2 matrix of the weights from the input layer to the hidden layer
	## w1		: a 3x3 matrix of the weights from the hidden layer to the output layer
	eta = 0.01	
	initW = read.table("initW.data")
	w0 = matrix(initW[1:130,], nrow = 65, ncol = 2)
	w1 = matrix(initW[131:139,], nrow = 3, ncol = 3)
	
	## Read the given dataset and convert it to a matrix
	## x		: a matrix containing the dataset
	## class	: a correct class for each training instace
	data = read.table("elf.data")
	x = matrix(0, nrow = 300, ncol = 64)
	for(i in 1:300) x[i,] = as.numeric(data[i,1:64])
	class = as.numeric(data$V65)
	
	z <- matrix( 0, nrow = 300, ncol = 2 )

	y <- matrix( 0, nrow = 300, ncol = 3 )
	
	delta_w1 <- matrix(0, nrow = 3, ncol = 3)
	delta_w0 <- matrix( 0, nrow=65, ncol = 2 )
	
	r = matrix(c(1, 0, 0, 0, 1, 0, 0, 0, 1), nrow = 3, ncol = 3)	
	
													
	## DO NOT USE RANDOM ORDER FOR INSTANCE ORDERING ALTHOUGH IT IS USED IN THE TEXTBOOK
	## Since each people can use their own stopping criteria,
	## we use a very simple stopping criterion: stopping after 2000 iterations
	for(i in 1:2000)
	{	
		## Your code starts from here
		
		# =============================
		#		Index for the instances in the dataset
		# =============================
		for ( indexT in 1:300 ) {
		
			# =====================
			#         hidden layer values (z)
			# =====================			
			z[indexT,] <- sigmoid( c(x[indexT,], 1) %*% w0 )
			
			# ================
			#         output values (y)
			# ================		
			y[indexT,] <- sigmoid(c(z[indexT,],1) %*% w1 )
			
			# ====================
			#         Calculate the Delta w1
			# ====================			
			for ( indexV in 1:3 ) {
				tempMatrix <- eta * (r[class[indexT],indexV] - y[indexT, indexV]) * y[indexT, indexV] * ( 1 - y[indexT, indexV] ) * as.matrix( c( z[indexT,], 1 ) ) 
				delta_w1[,indexV] <- tempMatrix
			}			
			
			# ===================================
			#         Calculate the Delta w0
			#         This one is the correct one 
			#  		Previous version forget to put the "y * (1-y)"
			# ===================================			
			for ( indexW in 1:2 ) {
				totalSum <- 0
				for ( indexV in 1:3 ) {
					totalSum <- totalSum + ((r[class[indexT],indexV] - y[indexT, indexV]) * y[indexT, indexV] * (1-y[indexT, indexV]) * w1[indexW, indexV])
				}						
				tempMatrix <- eta * totalSum * z[indexT, indexW] * ( 1 - z[indexT, indexW] ) * as.matrix( c( x[indexT,], 1 ) )
				delta_w0[,indexW] <- tempMatrix
			}
												
			w1 <- w1 + delta_w1
			w0 <- w0 + delta_w0															
		}
				
		## Your code ends
		
		## Print information every 100 iteration to check whether it is running
		if(i%%100 == 0)
		{
			print(paste(i, "th iteration"))
			err(list(W0 = w0, W1 = w1))
			flush.console()
		}
	}
	
	## Return the result
	return(list(W0 = w0, W1 = w1))
}

###################################################################################################################
## Write your answer to the problem 2 here
###################################################################################################################
#   Answer:
#
#   We know our dataset contains 300 instances of 8 x 8 pixel images for block letters "E", "L", and "F" and the program we made tries to find the similarities between 
#   those three letters.
#
#   Furthermore, the result of our implementation is two images.
#   The first image (Device2 (inactive)) displays the similarity between the letter "E" and "L" which is the only lower horizontal bar and 
#    the second image (Device3 (ACTIVE) ) displays the similarities between the letter "E" and "F" which are the two upper horizontal bars
# 
###################################################################################################################