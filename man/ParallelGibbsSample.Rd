% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ParallelGibbsSample.R
\name{ParallelGibbsSample}
\alias{ParallelGibbsSample}
\title{Setup hierarchical Dirichlet processes and run parallel Gibbs sampling chains}
\usage{
ParallelGibbsSample(
  input.catalog,
  seedNumber = 1,
  K.guess,
  multi.types = FALSE,
  verbose = FALSE,
  burnin = 5000,
  burnin.multiplier = 2,
  post.n = 200,
  post.space = 100,
  post.cpiter = 3,
  post.verbosity = 0,
  CPU.cores = 20,
  num.child.process = 20,
  gamma.alpha = 1,
  gamma.beta = 20,
  checkpoint = TRUE
)
}
\arguments{
\item{input.catalog}{Input spectra catalog as a matrix or
in \code{\link[ICAMS]{ICAMS}} format.}

\item{seedNumber}{A random seed that ensures ensures reproducible
results.}

\item{K.guess}{Suggested initial value of the number of
raw clusters. Usually, the number of raw clusters is
roughly twice the number
of extracted signatures.
Passed to hdpx::dp_activate as argument initcc.}

\item{multi.types}{A logical scalar or
a character vector.

If \code{FALSE}, The HDP analysis
will regard all input spectra as one tumor type, and the
HDP structure will have one parent node for all tumors.

If \code{TRUE}, Sample IDs in \code{input.catalog} must have the
form \emph{sample_type}::\emph{sample_id}.

If a character vector, then its length must be \code{ncol(input.catalog}),
and each value is the sample type of the corresponding column in \code{input.catalog},
e.g. \code{c(rep("Type-A", 23), rep("Type-B", 10))} for 23 Type-A samples and 10 Type-B
samples.

If not \code{FALSE}, HDP will have one parent node for each sample type
and one grandparent node.}

\item{verbose}{If \code{TRUE} then \code{message} progress information.}

\item{burnin}{The number of burn-in iterations in
one batch. The total number of burnin iterations is
\code{burnin * burnin.multiplier}.}

\item{burnin.multiplier}{Run \code{burnin.multiplier} rounds of \code{burnin}
iterations.
If \code{checkpoint} is \code{TRUE}, save the burnin chain (see
parameter \code{checkpoint}.)
The diagnostic plot \code{diagnostics.likelihood.pdf} can help
determine if the chain is stationary.
The burnin can be continued from a checkpoint file
with \code{\link{ExtendBurnin}} (see argument \code{checkpoint}).}

\item{post.n}{The number of posterior samples to collect.}

\item{post.space}{The number of iterations between collected samples.}

\item{post.cpiter}{The number of iterations of concentration
parameter samplings to perform after each iteration.}

\item{post.verbosity}{Verbosity of debugging statements.
No need to change except for development purposes.}

\item{CPU.cores}{Number of CPUs to use; this should be no
more than \code{num.child.process}.}

\item{num.child.process}{Number of posterior sampling chains; can set to
1 for testing. We recommend 20 for real data analysis}

\item{gamma.alpha}{Shape parameter of
the gamma distribution prior for the Dirichlet process concentration
parameters \eqn{\alpha_0} and all \eqn{\alpha_j} in
Figure B.1 of \itemize{
\item https://www.repository.cam.ac.uk/bitstream/handle/1810/275454/Roberts-2018-PhD.pdf
}}

\item{gamma.beta}{Inverse scale parameter (rate parameter) of
the gamma distribution prior for the Dirichlet process concentration
parameters:
\eqn{\beta_0} and all \eqn{\beta_j} in
Figure B.1 of \itemize{

\item https://www.repository.cam.ac.uk/bitstream/handle/1810/275454/Roberts-2018-PhD.pdf
}
We recommend gamma.alpha = 1 and gamma.beta = 20 for
single-base-substitution signature extraction;
gamma.alpha = 1 and gamma.beta = 50  for doublet-base-substitution
and indel signature extraction}

\item{checkpoint}{If \code{TRUE}, then \itemize{
\item Checkpoint each final Gibbs sample
chain to the current working directory, in a file called
mSigHdp.sample.checkpoint.\emph{x}.Rdata, where
\emph{x} depends on \code{seedNumber}.
\item Periodically checkpoint the burnin state
to the current working directory, in files called
mSigHdp.burnin.checkpoint.\emph{x}.Rdata,
where \emph{x} depends on the \code{seedNumber}.
}}
}
\value{
Invisibly,
the clean \code{chlist} (output of \code{CleanChlist}).
This is a list of \code{\link[hdpx]{hdpSampleChain-class}}
objects (see package hdpx).
}
\description{
Setup hierarchical Dirichlet processes and run parallel Gibbs sampling chains
}
\keyword{internal}
