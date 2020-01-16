{
    library(ggplot2)

    # we will work with points 1 to 250 (cm)
    scale.points <- c(1:250) 

    # we create a dataframe for plotting
    example.height <- data.frame(x=scale.points) 

    # we use sapply, which is a vectorized function application; see help if you don't understand it

    # we add y, which is just the probability density function described above (normal distribution)
    example.height$y <- sapply(example.height$x, function(x) {dnorm(x, mean=180, sd=10)}) 

    # this starts the plot creation
    g1 <- ggplot(example.height, aes(x=x, y=y)) 

    # we make the plot more pretty: we specify it should fill in area and add labels
    g1 <- g1 + geom_area(fill="green", alpha=.4)  + xlab("height") + ylab("P") + theme_gray(20) 

    g1

    literal.listener <- function(x, threshold, densityf, cumulativef) {
                    ifelse(
                         x>=threshold,
                         densityf(x)/(1-cumulativef(threshold)),
                         0
                                  )

    }

    threshold <- 170

    example.height$updated <- sapply(example.height$x, function(x) {literal.listener(x=x, threshold=threshold, densityf=function(x) {dnorm(x, 180, 10)}, cumulativef=function(x) {pnorm(x, 180, 10)} )}) 

    # this starts the plot creation
    g1 <- ggplot(example.height, aes(x=x, y=y)) 
    g1 <- g1 + geom_area(fill="green", alpha=.4) 

    # we add the result of updated belief
    g1 <- g1 + geom_area(aes(y=updated),fill="steelblue", alpha=.4) 
    g1 <- g1 + xlab("height") + ylab("P") + theme_gray(20) 

    g1

    expected.success <- function(threshold, scale.points, densityf, cumulativef) {

        ifelse(threshold>min(scale.points), sum(sapply(scale.points[scale.points<threshold], function(x) {densityf(x) * densityf(x)})), 0) + 
            sum(sapply(scale.points[scale.points>=threshold], function(x) {densityf(x) * literal.listener(x, threshold, densityf, cumulativef)}))

    }
}
    
#Task 1
{
    utility <- function(threshold, scale.points, coverage.parameter, densityf, cumulativef) {
        expected.success(threshold, scale.points, densityf, cumulativef) + 
            coverage.parameter * (1 - cumulativef(threshold))
    }

    probability.threshold <- function(threshold, scale.points, lambda, coverage.parameter, densityf, cumulativef) {

        exp(lambda * utility(threshold, scale.points, coverage.parameter, densityf, cumulativef)) / 
            sum(sapply(scale.points, 
                       function(x){exp(lambda * utility(x, scale.points, 
                                                        coverage.parameter, densityf, cumulativef))}))
    }


    use.adjective <- function(degree, scale.points, lambda, coverage.parameter, densityf, cumulativef) {
        
        denom <- sum(sapply(scale.points, function(x){
            exp(lambda * utility(x, scale.points, coverage.parameter, densityf, cumulativef))}))
        
        sum(sapply(scale.points[scale.points <= degree], 
                  function(x){exp(lambda * 
                                      utility(x, scale.points, coverage.parameter, 
                                              densityf, cumulativef))})) / denom 
    }

    # Help - tests you should pass

    #probability.threshold is a probability, so if you sum up all values it generates, the result should be 1
    round(sum(sapply(1:10, function(x) {probability.threshold(x, 1:10, 50, 0, function(x) {dnorm(x, 5, 1)}, function(x) {pnorm(x, 5, 1)})}))) == 1

    #for narrow normal distribution, prob. threshold should be max just one value above the average
    which(sapply(1:10, function(x) {probability.threshold(x, 1:10, 50, 0, function(x) {dnorm(x, 5, 1)}, function(x) {pnorm(x, 5, 1)})})==max(sapply(1:10, function(x) {probability.threshold(x, 1:10, 50, 0, function(x) {dnorm(x, 5, 1)}, function(x) {pnorm(x, 5, 1)})}))) == 6

    #use.adjective should be very unlikely on values 5 and smaller and very likely afterwards
    round(sapply(1:10, function(x) {use.adjective(x, 1:10, 50, 0, function(x) {dnorm(x, 5, 1)}, function(x) {pnorm(x, 5, 1)})})[5], 3) == 0.005
    round(sapply(1:10, function(x) {use.adjective(x, 1:10, 50, 0, function(x) {dnorm(x, 5, 1)}, function(x) {pnorm(x, 5, 1)})})[6], 3) == 1
    
    # probability threshold plot
    thres <- sapply(1:250, function(x){probability.threshold(x, scale.points, 50, 0, function(x) {dnorm(x, 180, 10)}, function(x) {pnorm(x, 180, 10)})})
    thres.data <- data.frame()
    thres.data <- as.data.frame(cbind(scale.points, thres))
    names(thres.data)[names(thres.data) == "scale.points"] <- "x"
    names(thres.data)[names(thres.data) == "thres"] <- "y"
    max.thres <- max(thres.data$y)
    thres.value <- thres.data$x[thres.data$y == max.thres]
    ggplot(thres.data, aes(x = x, y = y)) + 
        geom_area(fill="green", alpha=.7)  + xlab("height") + ylab("P") + theme_gray(20)
    
    # use adjective plot
    adj <- sapply(1:250, function(x){use.adjective(x, scale.points, 50, 0, function(x) {dnorm(x, 180, 10)}, function(x) {pnorm(x, 180, 10)})})
    adj.data <- data.frame()
    adj.data <- as.data.frame(cbind(scale.points, adj))
    names(adj.data)[names(adj.data) == "scale.points"] <- "x"
    names(adj.data)[names(adj.data) == "adj"] <- "y"
    # find the right degree
    max.degree <- max(adj.data$y)
    degree.value <- adj.data$x[adj.data$y == max.degree]
    ggplot(adj.data, aes(x = x, y = y)) + 
        geom_area(fill="green", alpha=.7)  + xlab("height") + ylab("P") + theme_gray(20)
}

# Task 2:
{
    # Explore expected.success and use.adjective for various prior distribution functions.
    # For this, assume that coverage.parameter $c$ is at 0 and lambda is at 50.

    ## IQ
    # mean = 100, sd = 5, range = 70-130
    iq.points = c(70:130)
    
    iq.es <- sapply(70:130, function(x){expected.success(x, iq.points, function(x) {dnorm(x, 100, 5)}, function(x) {pnorm(x, 100, 5)})})
    iq.es.data <- data.frame()
    iq.es.data <- as.data.frame(cbind(iq.points, iq.es))
    names(iq.es.data)[names(iq.es.data) == "iq.points"] <- "x"
    names(iq.es.data)[names(iq.es.data) == "iq.es"] <- "y"
    iq.es.max.degree <- max(iq.es.data$y)
    iq.es.degree.value <- iq.es.data$x[iq.es.data$y == iq.es.max.degree]
    ggplot(iq.es.data, aes(x = x, y = y)) + geom_area(fill="green", alpha=.7)  + xlab("height") + ylab("P") + theme_gray(20)
    
    iq.adj <- sapply(70:130, function(x){use.adjective(x, iq.points, 50, 0, function(x) {dnorm(x, 100, 5)}, function(x) {pnorm(x, 100, 5)})})
    iq.adj.data <- data.frame()
    iq.adj.data <- as.data.frame(cbind(iq.points, iq.adj))
    names(iq.adj.data)[names(iq.adj.data) == "iq.points"] <- "x"
    names(iq.adj.data)[names(iq.adj.data) == "iq.adj"] <- "y"
    iq.adj.max.degree <- max(iq.adj.data$y)
    iq.adj.degree.value <- iq.adj.data$x[iq.adj.data$y == iq.adj.max.degree]
    ggplot(iq.adj.data, aes(x = x, y = y)) + geom_area(fill="green", alpha=.7)  + xlab("height") + ylab("P") + theme_gray(20)
    
    
    ## Waiting time
    # mean = 2, variance = 2, shape = 2, scale = 1
    wait.points <- c(1:30)
    
    wait.es <- sapply(1:30, function(x){expected.success(x, wait.points, function(x) {dgamma(x, shape = 2, scale = 1)}, function(x) {pgamma(x, shape = 2, scale = 1)})})
    wait.es.data <- data.frame()
    wait.es.data <- as.data.frame(cbind(wait.points, wait.es))
    names(wait.es.data)[names(wait.es.data) == "wait.points"] <- "x"
    names(wait.es.data)[names(wait.es.data) == "wait.es"] <- "y"
    wait.es.max.degree <- max(wait.es.data$y)
    wait.es.degree.value <- wait.es.data$x[wait.es.data$y == wait.es.max.degree]
    ggplot(wait.es.data, aes(x = x, y = y)) + geom_area(fill="green", alpha=.7)  + xlab("height") + ylab("P") + theme_gray(20)
    
    wait.adj <- sapply(1:30, function(x){use.adjective(x, wait.points, 50, 0, function(x) {dgamma(x, shape = 2, scale = 1)}, function(x) {pgamma(x, shape = 2, scale = 1)})})
    wait.adj.data <- data.frame()
    wait.adj.data <- as.data.frame(cbind(wait.points, wait.adj))
    names(wait.adj.data)[names(wait.adj.data) == "wait.points"] <- "x"
    names(wait.adj.data)[names(wait.adj.data) == "wait.adj"] <- "y"
    wait.adj.max.degree <- max(wait.adj.data$y)
    wait.adj.degree.value <- wait.adj.data$x[wait.adj.data$y == wait.adj.max.degree]
    ggplot(wait.adj.data, aes(x = x, y = y)) + geom_area(fill="green", alpha=.7)  + xlab("height") + ylab("P") + theme_gray(20)
    
    data.adjective <- read.csv(file="adjective-data.csv", header=TRUE)

    gaussian.dist <-   c(1,2,3,4,5,6,5,4,3,2,1,0,0,0)
    left.skew.dist <-  c(2,5,6,6,5,4,3,2,1,1,1,0,0,0)
    moved.dist <-      c(0,0,0,1,2,3,4,5,6,5,4,3,2,1)
    right.skew.dist <- c(1,1,1,2,3,4,5,6,6,5,2,0,0,0)

    sapply(1:14, function(x) {round(length(rgamma(360, shape=1, scale=100)[which(round(rgamma(360, shape=4, scale=1.5)) == x)])/10)})


    data.gaus <- data.adjective[data.adjective$Distribution=="gaussian",]
    data.left <- data.adjective[data.adjective$Distribution=="left",]
    data.moved <- data.adjective[data.adjective$Distribution=="moved",]

    library(ggplot2)
    library(gridExtra)
    p.g <- ggplot(data.gaus,aes(x=Stimulus,y=100*percentage,colour=Adjective))+geom_line()+ ylab("P") +ggtitle("gaussian")
    p.l <- ggplot(data.left,aes(x=Stimulus,y=100*percentage,colour=Adjective))+geom_line()+ggtitle("left skewed")
    p.m <- ggplot(data.moved,aes(x=Stimulus,y=100*percentage,colour=Adjective))+geom_line()+ggtitle("moved")

    grid.arrange(p.g,p.l,p.m,ncol=2)
}
    
# Task 3:
{
    # check cor between predicted and observed data
    cor(..., ..., method="pearson")

    #install.packages("BayesianTools")
    library(BayesianTools)

    prior <- createUniformPrior(lower=c(0,0.1), upper=c(0.1,1), best=NULL)

    data.gaus.big <- subset(data.gaus, Adjective == "big")

    likelihood <- function(param1) {

        collect <- 0
    
        for (i in 1:14) {
            collect  <- collect + dnorm(data.gaus.big$percentage[i], mean=param1[1]+param1[2], sd=0.1, log=TRUE)

        }

        return(collect)
    }

    bayesianSetup <- createBayesianSetup(likelihood = likelihood, prior = prior)

    iter = 10000
    settings = list(iterations = iter, message = FALSE)
    out <- runMCMC(bayesianSetup = bayesianSetup, settings = settings)

    summary(out)
}

# Task 4
{
    prior <- createUniformPrior(lower=c(-1,1), upper=c(1,50), best=NULL)

    likelihood <- function(param1) {

        collect <- 0
    
        for (i in 1:14) {
            collect  <- collect + dnorm(data.gaus.big$percentage[i], mean=..., sd=0.1, log=TRUE)

        }

        return(collect)
    }

    #bayesianSetup <- createBayesianSetup(likelihood = likelihood, prior = prior)

    #iter = 10000

    #settings = list(iterations = iter, message = FALSE)

    #out <- runMCMC(bayesianSetup = bayesianSetup, settings = settings)
}

# Task 5
{
    
}

