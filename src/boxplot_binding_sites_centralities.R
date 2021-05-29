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
# This code will produce the boxplots of Figure 5b.
# Here we depict the centrality measures of the binding site residues
# and their immediate neighbors in the FVIII RIN.
#
# The output is a SVG file.

source("../src/generate_net_and_centralities.R")

binding_tmp = read.table("../datasets/binding_sites_only.csv", header=T, sep="\t", quote="")
binding = data.frame(node = intersect(binding_tmp$node, net.centr$node))

a = make_ego_graph(net, 1, match(intersect(binding$node, V(net)$name), V(net)$name))

# Combine an Ego graph
subgraph_list_df <- lapply(a, as_data_frame)
subgraph_df <- do.call(rbind, subgraph_list_df)
subgraph <- graph_from_data_frame(subgraph_df , directed = FALSE)
subgraph = simplify(subgraph)

result = data.frame(node=V(subgraph)$name, binding_type = NA, degree=NA, bt=NA, cl=NA, burts=NA)

result$degree = net.centr$dg[match(result$node, net.centr$node)]
result$bt = net.centr$bt[match(result$node, net.centr$node)]
result$cl = net.centr$cl[match(result$node, net.centr$node)]
result$burts = net.centr$burts[match(result$node, net.centr$node)]

pos = match(binding$node, result$node)
result$binding_type = "Neighbor Res."
result$binding_type[pos] = "Binding site"

result$binding_type = factor(result$binding_type, levels = c("Neighbor Res.", "Binding site"))

svg("Figure_5b_boxplot.svg", width = 3, height=9)

par(mfrow=c(3,1))

boxplot(result$degree~result$binding_type, col="white", las=1, ylab="Degree", xlab="")
stripchart(result$degree~result$binding_type, add=T, vertical=T, method="jitter", pch=19, col=c("blue", "magenta"))

boxplot(log(result$bt)~result$binding_type, col="white", las=1, ylab="log(Betweenness)", xlab="")
stripchart(log(result$bt)~result$binding_type, add=T, vertical=T, method="jitter", pch=19, col=c("blue", "magenta"))

boxplot(result$cl~result$binding_type, col="white", las=1, ylab="Closeness", xlab="")
stripchart(result$cl~result$binding_type, add=T, vertical=T, method="jitter", pch=19, col=c("blue", "magenta"))

dev.off()