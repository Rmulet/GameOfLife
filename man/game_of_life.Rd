\name{game_of_life}
\alias{game_of_life}
\title{Function to create a simulator of simple cells
}
\description{The program is a basic simulator of living cells on a two-dimensional space. Each cell is mapped on a grid of square cells where each of the items interacts with each neighbours. Once the initial distribution of living cells has been defined, the program has to determine the next state of each living cell. For each generation, game_of_life generats a plot showing the distribution of cells on the grid.
}
\usage{
game_of_life(size=10,iterations=25,initial=10,history=TRUE,locked=TRUE,time=0.8,test=FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{size}{Size of the grid as defined by one of its sides. The value must be a positive integer. It defaults to 10.
  }
  \item{iterations}{Number of iterations, i.e. of times the program will attempt to calculate a new generation of cells. However, it will stop when they are all dead. The number must be a positive integer - by default, it is set to 25.
  }
  \item{initial}{Number of cells which are initially alive. It must be a positive integer lower than the number of cells (size*size). Please bear in mind that values that are greatly inferior or superior to the size value will likely result in a quick death of the cells.
  }
  \item{history}{Enables the visualization of all the plots generated during the execution of the program at end of the simulation. This characteristic can be of help in analysing the evolution of the cell population. By default, it is set to TRUE.
  }
  \item{locked}{Stops the execution of the function if no progress is made in new generations, i.e. the positions of the cells are the same. By default, it is set to FALSE.
  }
  \item{time}{Controls the delay length every time the program plots the distribution of cells. It defaults to 0.8.
  }
  \item{test}{Activates testing mode. The function will show the matrices (world and nei) created in each generation. By default, it is set to FALSE.
  }
}
\value{The function returns up to three types of output: 1) A plot showing the distribution of cells in each generation; 2) Optionally, a plot displaying the evolution of the cell population over each generation; 3) Optionally, the raw matrices corresponding to the cell population and their number of neighbours
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Roger Mulet
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
game_of_life(size=9,iterations=5,initial=10,history=TRUE,test=FALSE,locked=TRUE)
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
