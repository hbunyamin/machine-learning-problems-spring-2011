## =============================
##		Lab #2 : Polynomial Regression
##				Name : Hendra Bunyamin
##				Student ID : 20105439
## =============================

## please do not delete this function
## this function will be used to grade your homework
## you can also check your results with this function
grading = function()
{
	dev.new()
	polygraphs("data.dat")
	MBVgraph()
	dev.new()
	ofpolygraphs()
}

## actual distribution
f = function(x)
{
    2 * sin(1.5 * x)
}

## this function is already fully implemented
## use this function as a guideline of your implementation
polygraphs = function(filename)
{
	par(mfrow = c(3, 3))
	data <- read.table(filename)
	xrange <- seq(min(data$x), max(data$x), length = 100)
	
	for(order in 0:7)
	{
		w <- polyfit(data, order)
		y <- predvalue(w, xrange)
		plot(data$x, data$y, main = paste("order = ", order), xlab = "x", ylab = "y", ylim = c(min(y, data$y), max(y, data$y)), col="red", lty = 1, pch = 3)
		lines(xrange, y, col = "blue")
	}
}

## HINT: type "?plot" in the R command line
## HINT: type "?lines" in the R command line
ofpolygraphs = function()
{
## ==============================
##     read all the 10 datasets
## ==============================
	data1 <- read.table("data1.dat")
	data2 <- read.table("data2.dat")	
	data3 <- read.table("data3.dat")	
	data4 <- read.table("data4.dat")
	data5 <- read.table("data5.dat")
	data6 <- read.table("data6.dat")
	data7 <- read.table("data7.dat")
	data8 <- read.table("data8.dat")
	data9 <- read.table("data9.dat")
	data10 <- read.table("data10.dat")

## ===============================
##     create a table 3 x 3 for displaying graphs
## ===============================
	par(mfrow = c(3, 3))	
				
## ==============================
##     				ORDER = 0
## ==============================

## ============================================================
##     create vectors which consist of  each weight for each dataset
##             w1 = weight vector for data set 1, w2 = weight vector for data set 2, and so on
## ============================================================
	w1 <- polyfit( data1, 0 )
	w2 <- polyfit( data2, 0 )
	w3 <- polyfit( data3, 0 )
	w4 <- polyfit( data4, 0 )
	w5 <- polyfit( data5, 0 )
	w6 <- polyfit( data6, 0 )
	w7 <- polyfit( data7, 0 )
	w8 <- polyfit( data8, 0 )
	w9 <- polyfit( data9, 0 )
	w10 <- polyfit( data10, 0 )

## ==============================
##     draw lines for each dataset
##			the color of dataset 1   = blue
##			the color of dataset 2   = gray
##			the color of dataset 3   = green
##			the color of dataset 4   = pink
##			the color of dataset 5   = yellow
##			the color of dataset 6   = purple
##			the color of dataset 7   = violet
##			the color of dataset 8   = red
##			the color of dataset 9   = black
##			the color of dataset 10 = brown
## ==============================	
	plot(0, 0, type="n", xlab="x", ylab="y", xlim = c(0, 5), 
	                 ylim = c(-3, 3), 
					 main="order = 0")
	
	lines(c(0,5), c(w1, w1), col="blue")
	lines(c(0,5), c(w2, w2), col="gray")
	lines(c(0,5), c(w3, w3), col="green")
	lines(c(0,5), c(w4, w4), col="pink")
	lines(c(0,5), c(w5, w5), col="yellow")
	lines(c(0,5), c(w6, w6), col="purple")
	lines(c(0,5), c(w7, w7), col="violet")
	lines(c(0,5), c(w8, w8), col="red")
	lines(c(0,5), c(w9, w9), col="black")
	lines(c(0,5), c(w10, w10), col="brown")	
				
## ==============================
##     				ORDER = 1
## ==============================

## ============================================================
##     create vectors which consist of  each weight for each dataset
##             w1 = weight vector for data set 1, w2 = weight vector for data set 2, and so on
## ============================================================
	w1 <- polyfit( data1, 1 )
	w2 <- polyfit( data2, 1 )
	w3 <- polyfit( data3, 1 )
	w4 <- polyfit( data4, 1 )
	w5 <- polyfit( data5, 1 )
	w6 <- polyfit( data6, 1 )
	w7 <- polyfit( data7, 1 )
	w8 <- polyfit( data8, 1 )
	w9 <- polyfit( data9, 1 )
	w10 <- polyfit( data10, 1 )
		
## ================================
##     create range of x values for each data set
## ================================	
	x1range <- seq(0, 5, length = 100)		
	x2range <- seq(0, 5, length = 100)		
	x3range <- seq(0, 5, length = 100)		
	x4range <- seq(0, 5, length = 100)		
	x5range <- seq(0, 5, length = 100)		
	x6range <- seq(0, 5, length = 100)		
	x7range <- seq(0, 5, length = 100)		
	x8range <- seq(0, 5, length = 100)		
	x9range <- seq(0, 5, length = 100)		
	x10range <- seq(0, 5, length = 100)		
		
## ================================
##     create predicted values for each datasets
## ================================
	y1 <- predvalue( w1, x1range )
	y2 <- predvalue( w2, x2range )
	y3 <- predvalue( w3, x3range )
	y4 <- predvalue( w4, x4range )
	y5 <- predvalue( w5, x5range )
	y6 <- predvalue( w6, x6range )
	y7 <- predvalue( w7, x7range )
	y8 <- predvalue( w8, x8range )
	y9 <- predvalue( w9, x9range )
	y10 <- predvalue( w10, x10range )	

	plot(0, 0, type="n", xlab="x", ylab="y", 
	                 ylim = c(-3, 3),
					 xlim = c(0, 5),
					 main = "order = 1")
	
## ==============================
##     draw lines for each dataset
##			the color of dataset 1   = blue
##			the color of dataset 2   = gray
##			the color of dataset 3   = green
##			the color of dataset 4   = pink
##			the color of dataset 5   = yellow
##			the color of dataset 6   = purple
##			the color of dataset 7   = violet
##			the color of dataset 8   = red
##			the color of dataset 9   = black
##			the color of dataset 10 = brown
## ==============================	
	lines( x1range, y1, col="blue")
	lines( x2range, y2, col="gray")
	lines( x3range, y3, col="green")
	lines( x4range, y4, col="pink")
	lines( x5range, y5, col="yellow")
	lines( x6range, y6, col="purple")
	lines( x7range, y7, col="violet")
	lines( x8range, y8, col="red")	
	lines( x9range, y9, col="black")	
	lines( x10range, y10, col="brown")	

## ==============================
##     				ORDER = 2
## ==============================

## ============================================================
##     create each vector which consists of  each weight for each dataset
##             w1 = weight vector for data set 1, w2 = weight vector for data set 2, and so on
## ============================================================
	w1 <- polyfit( data1, 2 )
	w2 <- polyfit( data2, 2 )
	w3 <- polyfit( data3, 2 )
	w4 <- polyfit( data4, 2 )
	w5 <- polyfit( data5, 2 )
	w6 <- polyfit( data6, 2 )
	w7 <- polyfit( data7, 2 )
	w8 <- polyfit( data8, 2 )
	w9 <- polyfit( data9, 2 )
	w10 <- polyfit( data10, 2 )

## ================================
##     create predicted values for each datasets
## ================================	
	y1 <- predvalue( w1, x1range )
	y2 <- predvalue( w2, x2range )
	y3 <- predvalue( w3, x3range )
	y4 <- predvalue( w4, x4range )
	y5 <- predvalue( w5, x5range )
	y6 <- predvalue( w6, x6range )
	y7 <- predvalue( w7, x7range )
	y8 <- predvalue( w8, x8range )
	y9 <- predvalue( w9, x9range )
	y10 <- predvalue( w10, x10range )	

	plot(0, 0, type="n", xlab="x", ylab="y", 
	                 ylim = c(-3, 3),
					 xlim = c(0, 5),
					 main = "order = 2")

## ==============================
##     draw lines for each dataset
##			the color of dataset 1   = blue
##			the color of dataset 2   = gray
##			the color of dataset 3   = green
##			the color of dataset 4   = pink
##			the color of dataset 5   = yellow
##			the color of dataset 6   = purple
##			the color of dataset 7   = violet
##			the color of dataset 8   = red
##			the color of dataset 9   = black
##			the color of dataset 10 = brown
## ==============================					 
	lines( x1range, y1, col="blue")
	lines( x2range, y2, col="gray")
	lines( x3range, y3, col="green")
	lines( x4range, y4, col="pink")
	lines( x5range, y5, col="yellow")
	lines( x6range, y6, col="purple")
	lines( x7range, y7, col="violet")
	lines( x8range, y8, col="red")	
	lines( x9range, y9, col="black")	
	lines( x10range, y10, col="brown")	

## ==============================
##     				ORDER = 3
## ==============================

## ============================================================
##     create each vector which consists of  each weight for each dataset
##             w1 = weight vector for data set 1, w2 = weight vector for data set 2, and so on
## ============================================================
	w1 <- polyfit( data1, 3 )
	w2 <- polyfit( data2, 3 )
	w3 <- polyfit( data3, 3 )
	w4 <- polyfit( data4, 3 )
	w5 <- polyfit( data5, 3 )
	w6 <- polyfit( data6, 3 )
	w7 <- polyfit( data7, 3 )
	w8 <- polyfit( data8, 3 )
	w9 <- polyfit( data9, 3 )
	w10 <- polyfit( data10, 3 )

## ================================
##     create predicted values for each datasets
## ================================		
	y1 <- predvalue( w1, x1range )
	y2 <- predvalue( w2, x2range )
	y3 <- predvalue( w3, x3range )
	y4 <- predvalue( w4, x4range )
	y5 <- predvalue( w5, x5range )
	y6 <- predvalue( w6, x6range )
	y7 <- predvalue( w7, x7range )
	y8 <- predvalue( w8, x8range )
	y9 <- predvalue( w9, x9range )
	y10 <- predvalue( w10, x10range )	

	plot(0, 0, type="n", xlab="x", ylab="y", 
	                 ylim = c(-3, 3),
					 xlim = c(0, 5),
					 main = "order = 3")
	
## ==============================
##     draw lines for each dataset
##			the color of dataset 1   = blue
##			the color of dataset 2   = gray
##			the color of dataset 3   = green
##			the color of dataset 4   = pink
##			the color of dataset 5   = yellow
##			the color of dataset 6   = purple
##			the color of dataset 7   = violet
##			the color of dataset 8   = red
##			the color of dataset 9   = black
##			the color of dataset 10 = brown
## ==============================					 	
	lines( x1range, y1, col="blue")
	lines( x2range, y2, col="gray")
	lines( x3range, y3, col="green")
	lines( x4range, y4, col="pink")
	lines( x5range, y5, col="yellow")
	lines( x6range, y6, col="purple")
	lines( x7range, y7, col="violet")
	lines( x8range, y8, col="red")	
	lines( x9range, y9, col="black")	
	lines( x10range, y10, col="brown")	


## ==============================
##     				ORDER = 4
## ==============================

## ============================================================
##     create each vector which consists of  each weight for each dataset
##             w1 = weight vector for data set 1, w2 = weight vector for data set 2, and so on
## ============================================================
	w1 <- polyfit( data1, 4 )
	w2 <- polyfit( data2, 4 )
	w3 <- polyfit( data3, 4 )
	w4 <- polyfit( data4, 4 )
	w5 <- polyfit( data5, 4 )
	w6 <- polyfit( data6, 4 )
	w7 <- polyfit( data7, 4 )
	w8 <- polyfit( data8, 4 )
	w9 <- polyfit( data9, 4 )
	w10 <- polyfit( data10, 4 )
	
## ================================
##     create predicted values for each datasets
## ================================	
	y1 <- predvalue( w1, x1range )
	y2 <- predvalue( w2, x2range )
	y3 <- predvalue( w3, x3range )
	y4 <- predvalue( w4, x4range )
	y5 <- predvalue( w5, x5range )
	y6 <- predvalue( w6, x6range )
	y7 <- predvalue( w7, x7range )
	y8 <- predvalue( w8, x8range )
	y9 <- predvalue( w9, x9range )
	y10 <- predvalue( w10, x10range )	

	plot(0, 0, type="n", xlab="x", ylab="y", 
	                 ylim = c(-3, 3),
					 xlim = c(0, 5),
					 main = "order = 4")
					 
## ==============================
##     draw lines for each dataset
##			the color of dataset 1   = blue
##			the color of dataset 2   = gray
##			the color of dataset 3   = green
##			the color of dataset 4   = pink
##			the color of dataset 5   = yellow
##			the color of dataset 6   = purple
##			the color of dataset 7   = violet
##			the color of dataset 8   = red
##			the color of dataset 9   = black
##			the color of dataset 10 = brown
## ==============================					 	
	lines( x1range, y1, col="blue")
	lines( x2range, y2, col="gray")
	lines( x3range, y3, col="green")
	lines( x4range, y4, col="pink")
	lines( x5range, y5, col="yellow")
	lines( x6range, y6, col="purple")
	lines( x7range, y7, col="violet")
	lines( x8range, y8, col="red")	
	lines( x9range, y9, col="black")	
	lines( x10range, y10, col="brown")	

## ==============================
##     				ORDER = 5
## ==============================

## ============================================================
##     create each vector which consists of  each weight for each dataset
##             w1 = weight vector for data set 1, w2 = weight vector for data set 2, and so on
## ============================================================
	w1 <- polyfit( data1, 5 )
	w2 <- polyfit( data2, 5 )
	w3 <- polyfit( data3, 5 )
	w4 <- polyfit( data4, 5 )
	w5 <- polyfit( data5, 5 )
	w6 <- polyfit( data6, 5 )
	w7 <- polyfit( data7, 5 )
	w8 <- polyfit( data8, 5 )
	w9 <- polyfit( data9, 5 )
	w10 <- polyfit( data10, 5 )

## ================================
##     create predicted values for each datasets
## ================================		
	y1 <- predvalue( w1, x1range )
	y2 <- predvalue( w2, x2range )
	y3 <- predvalue( w3, x3range )
	y4 <- predvalue( w4, x4range )
	y5 <- predvalue( w5, x5range )
	y6 <- predvalue( w6, x6range )
	y7 <- predvalue( w7, x7range )
	y8 <- predvalue( w8, x8range )
	y9 <- predvalue( w9, x9range )
	y10 <- predvalue( w10, x10range )	

	plot(0, 0, type="n", xlab="x", ylab="y", 
	                 ylim = c(-3, 3),
					 xlim = c(0, 5),
					 main = "order = 5")
					 
## ==============================
##     draw lines for each dataset
##			the color of dataset 1   = blue
##			the color of dataset 2   = gray
##			the color of dataset 3   = green
##			the color of dataset 4   = pink
##			the color of dataset 5   = yellow
##			the color of dataset 6   = purple
##			the color of dataset 7   = violet
##			the color of dataset 8   = red
##			the color of dataset 9   = black
##			the color of dataset 10 = brown
## ==============================					 	
	lines( x1range, y1, col="blue")
	lines( x2range, y2, col="gray")
	lines( x3range, y3, col="green")
	lines( x4range, y4, col="pink")
	lines( x5range, y5, col="yellow")
	lines( x6range, y6, col="purple")
	lines( x7range, y7, col="violet")
	lines( x8range, y8, col="red")	
	lines( x9range, y9, col="black")	
	lines( x10range, y10, col="brown")	


## ==============================
##     				ORDER = 6
## ==============================

## ============================================================
##     create each vector which consists of  each weight for each dataset
##             w1 = weight vector for data set 1, w2 = weight vector for data set 2, and so on
## ============================================================
	w1 <- polyfit( data1, 6 )
	w2 <- polyfit( data2, 6 )
	w3 <- polyfit( data3, 6 )
	w4 <- polyfit( data4, 6 )
	w5 <- polyfit( data5, 6 )
	w6 <- polyfit( data6, 6 )
	w7 <- polyfit( data7, 6 )
	w8 <- polyfit( data8, 6 )
	w9 <- polyfit( data9, 6 )
	w10 <- polyfit( data10, 6 )

## ================================
##     create predicted values for each datasets
## ================================		
	y1 <- predvalue( w1, x1range )
	y2 <- predvalue( w2, x2range )
	y3 <- predvalue( w3, x3range )
	y4 <- predvalue( w4, x4range )
	y5 <- predvalue( w5, x5range )
	y6 <- predvalue( w6, x6range )
	y7 <- predvalue( w7, x7range )
	y8 <- predvalue( w8, x8range )
	y9 <- predvalue( w9, x9range )
	y10 <- predvalue( w10, x10range )	

	plot(0, 0, type="n", xlab="x", ylab="y", 
	                 ylim = c(-3, 3),
					 xlim = c(0, 5),
					 main = "order = 6")

## ==============================
##     draw lines for each dataset
##			the color of dataset 1   = blue
##			the color of dataset 2   = gray
##			the color of dataset 3   = green
##			the color of dataset 4   = pink
##			the color of dataset 5   = yellow
##			the color of dataset 6   = purple
##			the color of dataset 7   = violet
##			the color of dataset 8   = red
##			the color of dataset 9   = black
##			the color of dataset 10 = brown
## ==============================					 					 
	lines( x1range, y1, col="blue")
	lines( x2range, y2, col="gray")
	lines( x3range, y3, col="green")
	lines( x4range, y4, col="pink")
	lines( x5range, y5, col="yellow")
	lines( x6range, y6, col="purple")
	lines( x7range, y7, col="violet")
	lines( x8range, y8, col="red")	
	lines( x9range, y9, col="black")	
	lines( x10range, y10, col="brown")	


## ==============================
##     				ORDER = 7
## ==============================

## ============================================================
##     create each vector which consists of  each weight for each dataset
##             w1 = weight vector for data set 1, w2 = weight vector for data set 2, and so on
## ============================================================
	w1 <- polyfit( data1, 7 )
	w2 <- polyfit( data2, 7 )
	w3 <- polyfit( data3, 7 )
	w4 <- polyfit( data4, 7 )
	w5 <- polyfit( data5, 7 )
	w6 <- polyfit( data6, 7 )
	w7 <- polyfit( data7, 7 )
	w8 <- polyfit( data8, 7 )
	w9 <- polyfit( data9, 7 )
	w10 <- polyfit( data10, 7 )

## ================================
##     create predicted values for each datasets
## ================================		
	y1 <- predvalue( w1, x1range )
	y2 <- predvalue( w2, x2range )
	y3 <- predvalue( w3, x3range )
	y4 <- predvalue( w4, x4range )
	y5 <- predvalue( w5, x5range )
	y6 <- predvalue( w6, x6range )
	y7 <- predvalue( w7, x7range )
	y8 <- predvalue( w8, x8range )
	y9 <- predvalue( w9, x9range )
	y10 <- predvalue( w10, x10range )	

	plot(0, 0, type="n", xlab="x", ylab="y", 
	                 ylim = c(-3, 3),
					 xlim = c(0, 5),
					 main = "order = 7")

## ==============================
##     draw lines for each dataset
##			the color of dataset 1   = blue
##			the color of dataset 2   = gray
##			the color of dataset 3   = green
##			the color of dataset 4   = pink
##			the color of dataset 5   = yellow
##			the color of dataset 6   = purple
##			the color of dataset 7   = violet
##			the color of dataset 8   = red
##			the color of dataset 9   = black
##			the color of dataset 10 = brown
## ==============================					 					 
	lines( x1range, y1, col="blue")
	lines( x2range, y2, col="gray")
	lines( x3range, y3, col="green")
	lines( x4range, y4, col="pink")
	lines( x5range, y5, col="yellow")
	lines( x6range, y6, col="purple")
	lines( x7range, y7, col="violet")
	lines( x8range, y8, col="red")	
	lines( x9range, y9, col="black")	
	lines( x10range, y10, col="brown")	
	
}

## HINT: the x-axis should start from 0. don't forget it!
MBVgraph = function()
{
	plot(0, 0, type="n", xlab="Order", ylab="Error", ylim = c(0, 8), xlim = c(0, 7))

## =================
##     create the x values
## =================	
	x_value <- c( 0, 1, 2, 3, 4, 5, 6, 7 )
	
## ===========================
##     create the y values which are MSE
## ===========================
	y_value <- c( MSE( 0 ), MSE( 1 ), MSE( 2 ), MSE( 3 ), MSE( 4 ), MSE( 5 ), MSE( 6 ), MSE( 7 ) )
	
## =================
##     draw lines for MSE
##			the color is blue
## =================
	lines( x_value, y_value, col="blue", lty=2 )
	
## ============================
##     create the y values which are Bias^2
## ============================	
	y_value <- c( biassq( 0 ), biassq( 1 ), biassq( 2 ), biassq( 3 ), biassq( 4 ), biassq( 5 ), biassq( 6 ), biassq( 7 ) )	

## =================
##     draw lines for Bias^2
##			the color is red
## =================	
	lines( x_value, y_value, col="red", lty=1 )	

## ==============================
##     create the y values which are Variance
## ==============================
	y_value <- c( variance( 0 ), variance( 1 ), variance( 2 ), variance( 3 ), variance( 4 ), variance( 5 ), variance( 6 ), variance( 7 ) )		

## =====================
##     draw lines for Variance
##			the color is dark green
## =====================
	lines( x_value, y_value, col="darkgreen", lty=3 )		

## ====================================
##     create legend for the description of the graph
## ====================================
	legend(0, 7, c("MSE", "Bias^2", "Variance"), col = c("blue","red","darkgreen"),
        lty = c(2, 1, 3),
        merge = TRUE, bg = 'gray90')	
}

## HINT: type "?solve" in the R command line
polyfit = function(data, order)
{
	result <- 0

## ===================
##     check the order value
##    		i've made 3 cases 
##				1. order = 0
##				2. order = 1
##				3. order > 1
## ===================
	if ( order == 0 ) {   
		## ======================================================
		##    order = 0 --> return a sample mean vector (mean of corresponding label y)
		## ======================================================		
		result <- mean( data[, length(data)] )
		## ======================================================
		##     order = 1 --> return a vector [w0 w1] from w0 + w1 * x
		##			This is a simple linear regression
		## ======================================================		
	} else if ( order == 1 ) { 
		dataModel <- lm( y~x, data=data )
		result <- matrix(c( dataModel$coefficients[1], dataModel$coefficients[2] ), nrow = 2, ncol = 1)

		## ======================================================
		##     order > 1 --> return a vector of weight 
		##			for example: 
		##					order = 2
		##						[w0 w1 w2] from w0 + w1 * x + w2 * x^2
		##					This is a simple quadratic regression
		## ======================================================		
	} else if ( order > 1 ) {
		## ======================================================
		##    create a vector w which is the solution of linear equation system
		##		(t(D) x D ) x w = t(D) x y
		##				t(D) = transpose of matrix D
		##				x       = multiply operator
		##				y		 = the y-value from our data set
		##					(based on the formula from the textbook, page 75)
		## ======================================================
		numberOfRow <- dim(data)[1]
		matriks <- matrix( c(1:1), nrow=numberOfRow, ncol = 1 )
		matriks <- cbind(matriks, data$x)
		for ( index in 2 : order ) {
			temp <- matrix( data$x^index, nrow=numberOfRow, ncol = 1  )
			matriks <- cbind(matriks, temp)		
		}		
		## ======================================================
		##    use the 'solve' function to obtain the solution of a linear equation system 
		## ======================================================		
		result <- solve( t(matriks) %*% matriks, t(matriks) %*% data$y )
	}		
	return (result)
}

predvalue = function(w, x)
{
	## ======================================================================
	##    totalVector = the vector whose values are the predicted values from x based on weight vector, w
	## ======================================================================
	totalVector <- vector(length=length(x))
	
	for ( indexOfX in 1 : length(x) ) {
		total <- 0
		indexOfW <- length(w)
		while ( indexOfW >= 1 ) {
			total <- total + w[indexOfW] * (x[indexOfX] ^ (indexOfW - 1))
			indexOfW <- indexOfW - 1		
		}
		totalVector[indexOfX] <- total		
	}	
	return (totalVector)
}

## HINT: for three functions, refer to p76 ~ p77 of the textbook
biassq = function(order)
{
## ============================
##     create a matrix w given the 'order'
## ============================
	w <- wmatrix(order)
	
## ============================
##                    read 'data.dat'
## ============================	
	ourData <- read.table( "data.dat" ) 
	
## ============================
##        get the x-value from our data
## ============================	
	xOurData <- ourData$x

## ================================================
##     compute g_bar	
##			g_bar = the mean of all predicted values from 10 datasets
## ================================================
	g1 <- predvalue( w[1,], xOurData )
	g2 <- predvalue( w[2,], xOurData )
	g3 <- predvalue( w[3,], xOurData )
	g4 <- predvalue( w[4,], xOurData )
	g5 <- predvalue( w[5,], xOurData )
	g6 <- predvalue( w[6,], xOurData )
	g7 <- predvalue( w[7,], xOurData )
	g8 <- predvalue( w[8,], xOurData )
	g9 <- predvalue( w[9,], xOurData )
	g10 <- predvalue( w[10,], xOurData )	
    g_bar <- 0.1 * ( g1 + g2 + g3 + g4 + g5 + g6 + g7 + g8 + g9 + g10 )

## ===================================================
##     compute f_value
##			f_value is the 'true' value, usually we don't know this 'true' value
## ===================================================	
	f_value <- 	f(xOurData)

## ===================================================
##     compute biassq
##			(1/20) * sigma_t_from_1_to_20 ( g_bar[t] - f_value[t] )^2
## ===================================================	
	total_sum <- 0
	for ( index in 1 : length(xOurData) ){
		total_sum = total_sum + (( g_bar[index] - f_value[index] )^2)
	}
	return ( total_sum * 0.05 )
}


variance = function(order)
{
	w <- wmatrix(order)
	ourData <- read.table( "data.dat" ) 	
	
	xOurData <- ourData$x

## ===================================================
##     compute g whose each column is predicted values from each dataset
##			1st column  = predicted values from dataset 1
##			2nd column = predicted values from dataset 2
##			...
##			10th column = predicted values from dataset 10
## ===================================================
	g <- as.matrix(predvalue( w[1,], xOurData ))
	for ( index in 2 : 10 ) {
		temp <- as.matrix(predvalue( w[index,], xOurData ))
		g <- cbind( g, temp )
	}
	
## ================================================
##     compute g_bar	
##			g_bar = the mean of all predicted values from 10 datasets
## ================================================
	total_g <- 0
	for ( index in 1:10 ){
		total_g <- total_g + g[, index]
	}
	g_bar <- 0.1 * total_g

## ================================================================
##     compute variance
##			(1/20) * (1/10) sigma_t_from_1_to_20 sigma_i_from_1_10 ( g[t, i] - g_bar[t] )^2
## ================================================================
	totalOuter <- 0
	for ( indexOfData in 1:20 ) {
		totalInner <- 0
		for ( indexDataSet in 1:10 ) {
			totalInner <- totalInner + ( g[indexOfData, indexDataSet] - g_bar[indexOfData] )^2						
		}	
		totalOuter <- totalOuter + totalInner
	}		
	return ( totalOuter / ( 20 * 10 ) )	
}

## don't calculate MSE using the formula Bias^2 + Variance
## implement this function according to its own definition
MSE = function(order)
{
	w <- wmatrix(order)
	ourData <- read.table( "data.dat" ) 	
	
	xOurData <- ourData$x
	
## ===================================================
##     compute g whose each column is predicted values from each dataset
##			1st column  = predicted values from dataset 1
##			2nd column = predicted values from dataset 2
##			...
##			10th column = predicted values from dataset 10
## ===================================================
	g <- as.matrix(predvalue( w[1,], xOurData ))
	for ( index in 2 : 10 ) {
		temp <- as.matrix(predvalue( w[index,], xOurData ))
		g <- cbind( g, temp )
	}
	
## ===================================================
##     compute f_value
##			f_value is the 'true' value, usually we don't know this 'true' value
## ===================================================
	f_value <- 	f(xOurData)
	
## ===================================================================
##     compute MSE
##			MSE = (1/20) * (1/10) * sigma_t_from_1_20 sigma_i_from_1_10 ( g[t, i] - f_value[t] )^2 
## ===================================================================
	totalOuter <- 0
	for ( indexOfData in 1:20 ) {
		totalInner <- 0
		for ( indexDataSet in 1:10 ) {
			totalInner <- totalInner + ( g[indexOfData, indexDataSet] - f_value[indexOfData] )^2
		}	
		totalOuter <- totalOuter + totalInner
	}
		
	return ( totalOuter / ( 20 * 10 ) )		
}

## ======================================================
##    create a matrix 'w' whose each row is a weight vector for each data set
##			1st row  = weight vector for data set 1
##			2nd row = weight vector for data set 2
##			3rd row = weight vector for data set 3
##			...
##			10th row = weight vector for data set 10
## ======================================================
wmatrix = function(order) {
	data1 <- read.table("data1.dat")
	data2 <- read.table("data2.dat")	
	data3 <- read.table("data3.dat")	
	data4 <- read.table("data4.dat")
	data5 <- read.table("data5.dat")
	data6 <- read.table("data6.dat")
	data7 <- read.table("data7.dat")
	data8 <- read.table("data8.dat")
	data9 <- read.table("data9.dat")
	data10 <- read.table("data10.dat")
	
	w1 <- polyfit( data1, order )
	w2 <- polyfit( data2, order )
	w3 <- polyfit( data3, order )
	w4 <- polyfit( data4, order )
	w5 <- polyfit( data5, order )
	w6 <- polyfit( data6, order )
	w7 <- polyfit( data7, order )
	w8 <- polyfit( data8, order )
	w9 <- polyfit( data9, order )
	w10 <- polyfit( data10, order )

	w <- matrix( w1, nrow = 1, ncol = (order+1) )
	w <- rbind( w, t(w2))
	w <- rbind( w, t(w3))
	w <- rbind( w, t(w4))
	w <- rbind( w, t(w5))
	w <- rbind( w, t(w6))
	w <- rbind( w, t(w7))
	w <- rbind( w, t(w8))
	w <- rbind( w, t(w9))
	w <- rbind( w, t(w10))	
	
	return (w)
}