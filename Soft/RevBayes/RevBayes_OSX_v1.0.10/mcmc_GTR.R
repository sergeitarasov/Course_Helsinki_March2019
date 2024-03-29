################################################################################
#
# RevBayes Example: Bayesian inference of phylogeny using a GTR+Gamma+Inv
#                   substitution model on a single gene.
#
# authors: Sebastian Hoehna, Michael Landis, and Tracy A. Heath
#
################################################################################


### Read in sequence data for the gene
data = readDiscreteCharacterData("data/primates_and_galeopterus_cytb.nex")

# Get some useful variables from the data. We need these later on.
n_species <- data.ntaxa()
n_branches <- 2 * n_species - 3
taxa <- data.taxa()


mvi = 1
mni = 1


######################
# Substitution Model #
######################

# specify the stationary frequency parameters
pi_prior <- v(1,1,1,1) 
pi ~ dnDirichlet(pi_prior)
moves[mvi++] = mvBetaSimplex(pi, weight=2.0)
moves[mvi++] = mvDirichletSimplex(pi, weight=1.0)


# specify the exchangeability rate parameters
er_prior <- v(1,1,1,1,1,1)
er ~ dnDirichlet(er_prior)
moves[mvi++] = mvBetaSimplex(er, weight=3.0)
moves[mvi++] = mvDirichletSimplex(er, weight=1.5)


# create a deterministic variable for the rate matrix, GTR
Q := fnGTR(er,pi) 



##############
# Tree model #
##############

out_group = clade("Galeopterus_variegatus")
# Prior distribution on the tree topology
topology ~ dnUniformTopology(taxa, outgroup=out_group)
moves[mvi++] = mvNNI(topology, weight=5.0)
moves[mvi++] = mvSPR(topology, weight=1.0)

# Branch length prior
for (i in 1:n_branches) {
    bl[i] ~ dnExponential(10.0)
    moves[mvi++] = mvScale(bl[i])
}

TL := sum(bl)

psi := treeAssembly(topology, bl)




###################
# PhyloCTMC Model #
###################

# the sequence evolution model
seq ~ dnPhyloCTMC(tree=psi, Q=Q, type="DNA")

# attach the data
seq.clamp(data)


############
# Analysis #
############

mymodel = model(psi)

# add monitors
monitors[mni++] = mnScreen(TL, printgen=1000)
monitors[mni++] = mnFile(psi, filename="output/primates_cytb_GTR.trees", printgen=10)
monitors[mni++] = mnModel(filename="output/primates_cytb_GTR.log", printgen=10)

# run the analysis
mymcmc = mcmc(mymodel, moves, monitors)
mymcmc.burnin(10000,200)
mymcmc.run(30000)




# summarize output
trace = readTreeTrace("output/primates_cytb_GTR.trees")
# and then get the MAP tree
map_tree = mapTree(trace,"output/primates_cytb_GTR_MAP.tre")
consensusTree(trace, file="output/primates.majrule.tre")

# you may want to quit RevBayes now
q()

