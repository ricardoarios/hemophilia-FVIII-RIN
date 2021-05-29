# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <https://www.gnu.org/licenses/>.
#
# This code is released as part of the manuscript
# "Protein residue network analysis reveals fundamental properties of the human coagulation factor VIII", submitted in 2021.
#
# Authors: Tiago J. S. Lopes, Ricardo Rios, Tatiane Nogueira, Rodrigo F. Mello
#
# This code will generate the FVIII RIN and calculate its centrality measures.
# Additionally, it will generate the correlation plot displayed in Figure 2b
# (the correlation between centrality measures of the RIN).
#
# The output is an igraph network and a data frame with the centrality measures.
# The figure is a SVG file.
#

library(igraph)
library(corrplot)
library(gplots)

mydata = read.table("../datasets/2r7e_network_formatted.csv", header=T, sep="\t", stringsAsFactors = F)

mydata = unique(mydata[,1:4])

aa_id = unique(cbind(c(mydata$node1, mydata$node2), c(mydata$res1, mydata$res2)))

# Generate the network
net = graph.data.frame(mydata, F)
net = simplify(net)

# Calculate centrality measures
dg = degree(net)
bt = betweenness(net, normalized = T)
cl = closeness(net, normalized = T)
burts = constraint(net)
pr = page_rank(net)$vector
auth = authority_score(net)$vector
kcore = coreness(net)

# Create the final data frame
net.centr = data.frame(node=as.numeric(aa_id[,1]), res=aa_id[,2], dg=NA, bt=NA, cl=NA, burts=NA, pr=NA, auth=NA, kcore=NA)

## Assign the centrality measures

net.centr$dg = dg[match(net.centr$node, names(dg))]
net.centr$bt = bt[match(net.centr$node, names(bt))]
net.centr$cl = cl[match(net.centr$node, names(cl))]
net.centr$burts = 1/burts[match(net.centr$node, names(burts))]
net.centr$pr = pr[match(net.centr$node, names(pr))]
net.centr$auth = auth[match(net.centr$node, names(auth))]
net.centr$kcore = kcore[match(net.centr$node, names(kcore))]

svg("Figure_1b_correlation_plot.svg", width = 6, height = 6)

# Generate the correlation plot/heatmap
corrM = cor(net.centr[,3:9], method="spearman")
corrplot(corrM, method="color", type="lower", order="hclust", tl.col="black", tl.srt=45, addCoef.col = "black", col=redblue(30))

dev.off()






















