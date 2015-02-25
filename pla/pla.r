#
#  Run the linear perceptron algorithm on a random set of points
#  For simplicity sake, the sets of points are on a 2D plane and are linearly separable by the diagonal X=Y
#

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  require(grid)

  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)

  numPlots = length(plots)

  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                    ncol = cols, nrow = ceiling(numPlots/cols))
  }

 if (numPlots==1) {
    print(plots[[1]])

  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))

    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}


# Function to generate a point:
generatePoint <- function(){
  target = sample(1:2, 1);
  targetVal = ifelse(target==1 ,-1, 1);
  x1 = sample(10:100, 1);
  x2 = sample(1:(x1-5), 1);

  x = ifelse(targetVal==1, x1, x2);
  y = ifelse(targetVal==1, x2, x1);

  point = c(1,x,y, targetVal);
  return(point)
}

# Function to generate df of points:
generatePoints <- function(numOfPoints) {
  df = data.frame();
  
  for (i in 1:numOfPoints ) {
    point = generatePoint();
    df = rbind(df,point);
  }
  colnames(df) = c("X0", "X1", "X2", "Target");
  return (df);
}

# Function to draw all the points:
drawPoints <- function(DF) {
  library(ggplot2);
  p = qplot(X1, X2, data=DF, color=Target) + theme(legend.position="none") ;
  return (p);
}

# Function to generate the line (W0, W1, W2)
getLineInfo <- function(W) {
	slope = -W[2]/W[3];
	b = -W[1]/W[2];
	return (c(slope, b));
}


# Function to draw the line
drawSeparatingLine <- function(W, myPlot, misfitPoint, title) {
  library(ggplot2);
  lineInfo = getLineInfo(W);
  myPlot = myPlot + geom_abline(intercept = lineInfo[2], slope = lineInfo[1]);
  myPlot = myPlot + annotate("text", x = 45, y = 100, label = title);
  if (!is.null(misfitPoint)) {
   myPlot = myPlot + annotate("text", label = "X", x = misfitPoint$X1, y = misfitPoint$X2, size = 5, colour = "red");
  }
  
  return (myPlot);
}


findMisfitPoint <- function(W, DF) {
	# Loop hrough the points until finding one that doesn't fit
    for (i in (1:nrow(DF))) {
       val = DF[i,]$X0 * W[1] + DF[i,]$X1 * W[2] + DF[i,]$X2 * W[3];
       
       sig = ifelse(val>=0, 1, -1);
       if (sig != DF[i,]$Target) {
         return (DF[i,]);
        }
    }
    return (NULL);
}


adjustWeights <- function(W, DF, misFitPoint) {
	if (is.null(misFitPoint)) {
	  return (NULL);
	}

	# Adjust weigths:
	y = misFitPoint$Target;
	X = c(misFitPoint$X0, misFitPoint$X1, misFitPoint$X2);
	W = W + y * X;
  return (W);
}


# Run the LPA and return a multiplot showing the decision boundary at each step
linearPerceptronAlgorithm <- function(numOfPoints, maxNumberOfCycles){
  DF = generatePoints(numOfPoints);
  pointsPlot = drawPoints(DF);
  #W =  sample(1:10, 3, replace=T);
  W =  sample(-10:10, 3, replace=T);
  weightsCounter = 0;
  plotsList = list();
  misFitPoint = NULL
  i =0
  while (!is.null(W) && i<maxNumberOfCycles) {  
    pltName = paste( 'plotNum_', i, sep = '' );
    newPlot = drawSeparatingLine(W, pointsPlot, misFitPoint, pltName);
    #pltName = paste( 'plotNum_', i, sep = '' );
    #print (paste('made plot ' , pltName, sep=' '));
    plotsList[[pltName]] = newPlot;
    misFitPoint = findMisfitPoint(W, DF);    
    W = adjustWeights(W, DF, misFitPoint);
    i = i+1;
  }
  return (multiplot(plotlist=plotsList, cols=5));
}

linearPerceptronAlgorithm(30, 25)
