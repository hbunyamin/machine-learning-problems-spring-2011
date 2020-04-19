# ====================================
#        Lab #4         : Clustering & Image Segmentation
#        Name 			: Hendra Bunyamin
#        Student ID 	: 20105439
# ====================================

# Require the package of "ReadImages"
library(ReadImages);

# Read image file
# input  : filename - string of filename
# output : srcImg   - image matrix
#          data     - (nData x 3) matrix of data points 
#                     nData = width of image x height of image
#          width    - width of image
#          height   - height of image
genData = function(filename)
{
  img = read.jpeg(filename);
  w = nrow(img);
  h = ncol(img);
  z = matrix(data = img, w * h, 3);
  for (x in 1:w) {
    for (y in 1:h) {
      k = (y - 1) * w + x;
      z[k, ] = c(t(img[x, y, ]));
    }
  }
  return(list(srcImg = img, data = z, width = w, height = h));
}

# K-means clustering algorithm
# input  : data      - (nData x nDim) matrix of data points
#          nClusters - number of clusters
#          Mu        - (nClusters x nDim) matrix of initial centers
#          nIters    - maximum number of iterations allowed
# output : centers   - (nClusters x nDim) matrix of computed centers
#          cluster   - (nData x 1) vector of integers indicating the cluster to which each point is allocated
Kmeans = function(data, nClusters, Mu, nIters)
{
  print("K-means clustering");
  nData = nrow(data);
  nDim = ncol(data);
  
  # implement K-means clustering algorithm
   	
  # the number of iteration begins from 1
  iter <- 1	 

  # ===============================
  #          Initialize all the output variables 	
  # ===============================	
  computedCenters <- Mu
  clusters <- matrix( 0, nrow = nData, ncol = 1 )
  
  # ====================
  #          The iteration begins	
  # ====================
  while ( iter <= nIters ){
	
  # ==========================================
  #          Find the minimum distance between instance and Mu
  # ==========================================
	for ( indexI in 1:nData ) {
		minDistance <- 99999				
		for ( indexJ in 1:nClusters  ) {
			distance <- dist( rbind(data[indexI,], computedCenters[indexJ,]) , method="euclidean" )
			if ( distance < minDistance ){
				minDistance <- distance
				clusters[ indexI ] <- indexJ
			}		
		}				
	}		
	
   # ==========================================
   #          Sum all instances according to the cluster they belong
   # ==========================================	
	for ( indexJ in 1:nClusters ) {
		sumData <- 0
		countData <- 0
		for ( indexI in 1:nData ) {
			if ( clusters[indexI] == indexJ ) {
				sumData <- sumData + data[indexI,]
				countData <- countData + 1
			}		
		}

		# =======================
		#          Compute the new mean
		# =======================		
		newMean <- sumData / countData
		computedCenters[indexJ,] <- newMean
	}		
	iter = iter + 1
  }    
  return(list(centers = computedCenters, cluster = clusters));
}

# EM algorithm for Gaussian mixtures
# input  : data      - (nData x nDim) matrix of data points
#          nClusters - number of clusters
#          Mu        - (nClusters x nDim) matrix of initial centers
#          Sigma     - (nDim x nDim x nClusters) array of initial covariance
#          Pi        - (nClusters x 1) vector of priors (mixture propotions)
#          nIters    - maximum number of iterations allowed
# output : centers   - (nClusters x nDim) matrix of computed centers
#          cluster   - (nData x 1) vector of integers indicating the cluster to which each point is allocated
EM4GM = function(data, nClusters, Mu, Sigma, Pi, nIters)
{
  print("EM for Gaussian mixtures");
  nData = nrow(data);
  nDim = ncol(data);
  
  # implement EM algorithm for Gaussian mixtures
  iter <- 1
  
  # =========================================
  #         Assign all parameters to new variables
  #           These new variables will be used for each iteration
  # =========================================
  computedCenters <- Mu
  computedPi <- Pi
  computedSigma <- Sigma
  
  # ========================
  #         w is the weight matrix 
  # ========================
  w <- matrix( 0, nrow= nData, ncol = nClusters )  
  clusters <- matrix( 0, nrow = nData, ncol = 1 )
    
  while ( iter <= nIters ) {
  
	# ============
	#      E-Step
	# ============	
	for ( indexData in 1:nData ) {
		totalDataProb <- 0
		
		# -------------------------------------------------------------------------------------------------------
		#      Calculate the total probability which will be the denominator for computing weight matrix
		# -------------------------------------------------------------------------------------------------------
		for ( indexCluster in 1:nClusters ) {
			totalDataProb <- totalDataProb + (mvnpdf( as.matrix(data[indexData,]), as.matrix(computedCenters[indexCluster,]), 
  	                                                                                                             as.matrix(computedSigma[,,indexCluster]  )) * computedPi[indexCluster])
		}
		
		# ---------------------------------------------------------------------------
		#      Calculate the weight matrix for each instance and each cluster
		# ---------------------------------------------------------------------------		
		for ( indexCluster in 1:nClusters ) {
			numerator <- mvnpdf( as.matrix(data[indexData,]), as.matrix(computedCenters[indexCluster,]),  as.matrix(computedSigma[,,indexCluster]  )) * computedPi[indexCluster]			
			w[indexData,indexCluster] <- numerator /  totalDataProb 
		}	
	}
	
	
	# ==========================================
	#                                             M-Step
	# ==========================================
	for ( indexCluster in 1:nClusters ) {
		# --------------------------------------------------
		#            Calculate new Pi
		# --------------------------------------------------		
		sumW <- 0
		for ( indexData in 1:nData ) {
			sumW <- sumW + w[indexData, indexCluster]
		}
		computedPi[indexCluster] <- sumW /nData
		
		# --------------------------------------------------
		#            Calculate new Mu
		# --------------------------------------------------		
		sumNumerator <- 0
		sumDenominator <- 0
		for ( indexData in 1:nData ) {
			sumNumerator <- sumNumerator + w[indexData, indexCluster] * as.matrix(data[indexData,])
			sumDenominator <- sumDenominator + w[indexData, indexCluster]
		}
		newMuVector <-  sumNumerator / sumDenominator
		computedCenters[indexCluster,] <- t(newMuVector)
					
		# --------------------------------------------------
		#            Calculate new Sigma
		# --------------------------------------------------	
		sumNumerator <- 0
		for ( indexData in 1:nData ) {
			sumNumerator <- sumNumerator + w[indexData, indexCluster] * (as.matrix(data[indexData,]) - as.matrix(computedCenters[indexCluster,] )) %*% t(as.matrix(data[indexData,]) - as.matrix(computedCenters[indexCluster,]) )
		}		
		newSigmaMatrix <- sumNumerator / sumDenominator
		computedSigma[,,indexCluster] <- newSigmaMatrix
	}			
	iter <- iter + 1
  }
  
  cat( "EM for Gaussian mixtures has finished computing :)\n" )
  
  # ========================================================
  #              Assign the cluster with the biggest probability  to each instance 
  # ========================================================
  for ( indexData in 1: nData ) {
	maxProb <- -1
    candidateCluster <- 0  
	
	for ( indexCluster in 1 :nClusters ) {
		if ( w[indexData, indexCluster] > maxProb ) {
			maxProb <- w[indexData,indexCluster]
			candidateCluster <- indexCluster
		}		
	}
	clusters[ indexData ] <- candidateCluster
  }
     
  return(list(centers = computedCenters, cluster = clusters));
}

# Draw the result of the image segmentation
# input  : srcImg  - image matrix of source image
#          width   - width of image
#          height  - height of image
#          centers - (nClusters x nDim) matrix of computed centers
#          cluster - (nData x 1) vector of integers indicating the cluster to which each point is allocated
drawResult = function(srcImg, width, height, centers, cluster)
{
  imgdata = array(0, dim=c(width, height, 3));
  for (x in 1:width) {
    for (y in 1:height) {
      k = (y - 1) * width + x;
      id = cluster[k];
      if (length(centers[id, ]) == 0) {
        print(paste(k, id));
      }
      imgdata[x, y, ] = as.matrix(centers[id, ]);
    }
  }
  op = par(mfcol = c(1, 2));
  plot(srcImg, main = "Source image");
  plot(imagematrix(imgdata), main = "Segmented image");
}

# Multivariate Normal Probability Density Function 
# input  	: x      - (nDim x 1) data vector
#          	 mu   - (nDim x 1)  mean vector
#          	 sigma     - (nDim x nDim) covariance matrix
# output : the density of multivariate normal distribution - (scalar value) 
mvnpdf <- function( x, mu, sigma ){
	# =========================
	#           Create an Identity Matrix
	# =========================
	matrix_identity <- matrix(0, ncol=ncol(sigma), nrow=ncol(sigma) )
	for ( index_x in 1: nrow(matrix_identity) ) {
		for ( index_y in 1:ncol( matrix_identity ) ){
			if ( index_x == index_y )
				matrix_identity[index_x, index_y] <- 1		
		}	
	}			
	
	# ==========================
	#           Compute the Inverse of Sigma
	# ==========================			
	inverseSigma <- solve( sigma, matrix_identity )		
	
	# ===============================================
	#           Compute the density of the Multivariate Normal Distribution
	# ===============================================
	return (  ( 1 / (    (2*pi)^( nrow(sigma) / 2 ) * sqrt( det(sigma) )   )   ) * exp( -0.5 * t( x - mu ) %*% inverseSigma %*% ( x - mu ) )  )	
}

# -------------------------------------
#       example of test code
# -------------------------------------
#filename = "./test.jpg";
filename = "./elephant.jpg";
x = genData(filename);

nClusters = 2;
nIters = 1;						

nData = nrow(x$data);

nDim = ncol(x$data);

MU = matrix(runif(nClusters * nDim, 0, 1), nClusters, nDim);
SIGMA = array(rep(diag(1, nDim, nDim), nClusters), dim = c(nDim, nDim, nClusters));
PI = runif(nClusters, 0, 1);
PI = PI / sum(PI);

E = EM4GM(x$data, nClusters, MU, SIGMA, PI, nIters);
drawResult(x$srcImg, x$width, x$height, E$centers, E$cluster);

K = Kmeans(x$data, nClusters, MU, nIters);
drawResult(x$srcImg, x$width, x$height, K$centers, K$cluster);
