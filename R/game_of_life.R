game_of_life <- function (size=10,iterations=25,initial=10,history=TRUE,locked=TRUE,time=0.8,test=FALSE) {
  #Input evaluation#
  if ((sum(size,iterations,initial)) %% 1 != 0) {
    stop("Wrong input values. Please, enter integers as arguments.")
  }
  if (any(size<=0,iterations<=0,initial<=0,time<=0)) {
    stop("Wrong input values. Please, enter only positive values.")
  }
  if (initial>size*size) {
    stop("Wrong input values. Please introduce an initial value lower than size*size.")
  }
  #Library checking and variable initialization
  if (require("gplots")== FALSE){cat(sprintf("\n The 'gplots' package is being installed. Please run the function again")); install.packages("gplots")}
  library("gplots",quietly=TRUE)
  his <- NULL # For cell population history
  gen <- 0 # To count the number of generations
##FIRST MODULE##
  #Distributing the initial generation##
  world <- rep(0,size*size)
  firstG <- sample(size*size,initial)
  for (n in 1:initial) {
    world[firstG[n]] <- 1 # Assign 1 to as many cells as "initial"
  }
  dim(world) <- c(size,size)
  his[[1]] <- world # Save the initial gen for plotting later
  if (test == TRUE) {
  cat("\n","Matrix generation 0","\n")
  print(world)
  }
 #Plotting the distribution of generation 0#
  heatmap.2(world,main="Generation 0",key=FALSE,density.info="none",trace="none",col=c("white","steelblue"),Colv=FALSE,Rowv=FALSE,dendrogram="none",sepcolor="white",colsep=1:ncol(world),rowsep=1:nrow(world))
  Sys.sleep(time) # Pause the function to allow visualization
##SECOND MODULE##
  #Counting the neighbours of every cell#
  for (g in 1:iterations) {
    gen <- gen + 1 # Number of generations for history plotting
    nei <- world # Number of neighbours
    uni <- cbind(world,world,world)
    uni <- rbind(uni,uni,uni) # Creates a "universe" of matrices by adding world to itself on all sides
    for (i in 1:size){
      for (j in 1:size){
        x <- i+size
        y <- j+size
        nei[i,j] <- sum(uni[x-1,y-1],uni[x,y-1],uni[x+1,y-1],uni[x-1,y],uni[x+1,y],uni[x-1,y+1],uni[x,y+1],uni[x+1,y+1])
      }
    }
    if (test == TRUE) {
    cat(sprintf("Matrix of neighbours %d",g-1),"\n")
    print(nei)
    cat("\n")
    }
  #Distribution of live cells in the next gen#
    vnei <- as.vector(nei)
    vworld <- as.vector(world)
    n = 0 #Counter for the cell loop. It advances one step in the world vector every cycle.
    for (c in vworld) {
      n = n+1
      if (c == 1) { # A live cell...
        if (vnei[n] <= 1) {
          vworld[n] <- 0
        }
        if (vnei[n] >= 2) {
          vworld[n] <- 1
        }
        if (vnei[n] > 3) {
          vworld[n] <- 0
        }
      }
      if (c == 0) { # An empty cell...
        if (vnei[n] == 3) {
          vworld[n] <- 1
        }
      }
    }
    #Checking that new generations are not the same (OPTIONAL)#
    if (locked == TRUE && all.equal(vworld,as.vector(world)) == TRUE) {
      cat(sprintf("The cell population has reached a stalemate at generation %d and the process will be terminated. ",g))
      his[[g+1]] <- world
      break
    }
    #Plotting the new distribution#
    world <- vworld
    dim(world) <- c(size,size)
    if (test == TRUE) { # For testing purposes
      cat (sprintf("Matrix generation %d",g),"\n")
      print(world)
      cat ("\n")
    }
    heatmap.2(world,main=sprintf("Generation %d",g),key=FALSE,density.info="none",trace="none",col=c("white","steelblue"),Colv=FALSE,Rowv=FALSE,dendrogram="none",sepcolor="white",colsep=1:ncol(world),rowsep=1:nrow(world))
    his[[g+1]] <- world
    Sys.sleep(time)
    #Endgame messages#
    if (sum(world) == 0) {
      cat (sprintf("Game over: all cells are dead. A total of %d generations have been simulated.",gen))
      cat("\n")
      break
    }
    if (g == iterations) {
      cat (sprintf("The requested number of iterations has been reached. A total of %d generations have been simulated.",gen))
      cat("\n")
    }
  }
  ##Plotting the life of the cells (OPTIONAL)##
  if (history == TRUE){
    Sys.sleep(time)
    cat("The evolution of the cell population is now shown.")
    field <- round(sqrt(gen)+1) # Adding 1 fixes issues with 1 single gen
    par(mfrow=c(field,field),mar=c(2,3,3,2))
    for (u in 1:(gen+1)) {
      rev <- t(his[[u]])[,nrow(his[[u]]):1] # Reverse to plot correctly
      image(rev,col=c("white","steelblue"),main=sprintf("Gen %d",u-1))
    }
  }
}
