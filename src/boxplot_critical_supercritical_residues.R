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
# This code will produce the results depicted in Figure 4c.
# 
# First it identifies the key residues of the FVIII RIN (as described in the manuscript),
# and then displays their relative surface exposed area (RSA).
#
# The output is a SVG file.

rsa = read.table("../datasets/Relative_Surf_Area_2R7E.csv", sep="\t", header = T)

source("../src/pareto_front.R"); 
source("../src/generate_net_and_centralities.R")

rsa$dg = NA
rsa$bt = NA
rsa$cl = NA

rsa$dg[match(net.centr$node, rsa$pos)] = net.centr$dg
rsa$bt[match(net.centr$node, rsa$pos)] = net.centr$bt
rsa$cl[match(net.centr$node, rsa$pos)] = net.centr$cl

# Remove rows with NAs
rsa = rsa[-unique(which(is.na(rsa), arr.ind = T)[,1]),]

hdhb = which(rsa$dg >= 12 & log(rsa$bt) > -6)
ldhb = which(rsa$dg <= 3 & log(rsa$bt) > -6)
ldlb = which(rsa$dg <= 3 & log(rsa$bt) <= -13)

# Find the supercritical residues

supercritical = pareto_front(as.matrix(rsa[,3:5]))
supercritical_pos = as.numeric(rownames(supercritical))

df = data.frame(Group="LDLB", rsa=rsa$rsa[ldlb])
tmp1 = data.frame(Group="LDHB", rsa=rsa$rsa[ldhb])
tmp2 = data.frame(Group="HDHB", rsa=rsa$rsa[hdhb])
tmp3 = data.frame(Group="Supercritical", rsa=rsa$rsa[supercritical_pos])

df = rbind(df, tmp1, tmp2, tmp3)

df$Group <- factor(df$Group, levels=names(sort(table(df$Group))))

svg("Figure_4c_stripchart.svg", width=7, height=5)
stripchart(df$rsa~df$Group, vertical=T, method="jitter", pch=19, las=1, col=c("red", "blue", "magenta", "orange"),
           ylab="Relative Exposure")

grid(lwd = 1.2)

dev.off()

