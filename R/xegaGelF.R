


#
# (c) 2021 Andreas Geyer-Schulz
#     Grammatical Evolution in R. V0.1
#     Layer: Gene-Level Functions
#            Binary representation of genes.
#     Package: xegaGeGene
#

#' The local function list lFxegaGeGene.
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

