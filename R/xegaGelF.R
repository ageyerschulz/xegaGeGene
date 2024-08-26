


#
# (c) 2021 Andreas Geyer-Schulz
#     Grammatical Evolution in R. V0.1
#     Layer: Gene-Level Functions
#            Binary representation of genes.
#     Package: xegaGeGene
#

#' The local function list lFxegaGeGene.
#'
#' @description
#' We enhance the configurability of our code by introducing 
#'  a function factory. The  function factory contains
#'  all the functions that are needed for defining
#'  local functions in genetic operators. The local function  
#'  list keeps the signatures of functions (e.g. mutation functions)
#'  uniform and small. At the same time, variants of functions
#'  can use different local functions. 
#'
#' @details
#'    We use the local function list for 
#'    \enumerate{
#'    \item
#'       replacing all constants by constant functions.
#'       
#'       Rationale: We need one formal argument (the local function list lF)
#'       and we can dispatch multiple functions. E.g.  \code{lF$verbose()}
#'   \item    
#'       dynamically binding a local function with a definition from a
#'       proper function factory. E.g., the selection methods 
#'       \code{lF$SelectGene()} and \code{lF$SelectMate()}.
#'       
#'  \item gene representations which require special functions to handle them:
#'        For example,
#'        \code{lF$InitGene()}, \code{lF$DecodeGene()}, \code{lF$EvalGene()},
#'        \code{lF$ReplicateGene()}, ...
#'       
#'  } 
#'
#' @importFrom xegaBNF compileBNF
#' @importFrom xegaBNF booleanGrammar
#' @importFrom xegaSelectGene parm
#' @importFrom xegaSelectGene envXOR
#' @export
lFxegaGeGene<-list(
penv=xegaSelectGene::envXOR,
Grammar=xegaBNF::compileBNF(xegaBNF::booleanGrammar()),
Codons=xegaSelectGene::parm(25),
CodonPrecision=xegaSelectGene::parm(16),
BitsOnGene=xegaSelectGene::parm(25*16),
CodonLCM=xegaSelectGene::parm(2*3*2*5*7*2*3*11),
replay=xegaSelectGene::parm(0),
verbose=xegaSelectGene::parm(4),
MaxDepth=xegaSelectGene::parm(5),
CBestFitness=xegaSelectGene::parm(100),
CWorstFitness=xegaSelectGene::parm(-100),
MutationRate1=xegaSelectGene::parm(0.01),
MutationRate2=xegaSelectGene::parm(0.20),
BitMutationRate1=xegaSelectGene::parm(0.01),
BitMutationRate2=xegaSelectGene::parm(0.20),
Max=xegaSelectGene::parm(1), 
Offset=xegaSelectGene::parm(1),
Eps=xegaSelectGene::parm(0.01),
Elitist=xegaSelectGene::parm(TRUE),
TournamentSize=xegaSelectGene::parm(2),
SelectGene=xegaSelectGene::SelectGeneFactory(method="PropFitDiff"),
SelectMate=xegaSelectGene::SelectGeneFactory(method="Uniform"),
EvalGene=xegaSelectGene::EvalGeneFactory(method="EvalGeneU")
)

