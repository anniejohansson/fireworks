\name{Make fireworks}
\alias{Make fireworks}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Generate random fireworks
}
\description{
This function allows you to create a generated image of fireworks made with numbers drawn from a random beta distribution.
}
\usage{
Make fireworks(background, n)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{background}{
Choose a background color for the fireworks by inputting a string of any of the base R colors. e.g. "midnightblue". Mode needs to be character.
}
  \item{n}{
Choose how many colors of the rainbow the fireworks should have. Mode needs to be numeric.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
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
Annie Johansson
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
make_fireworks("midnightblue", 20)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
