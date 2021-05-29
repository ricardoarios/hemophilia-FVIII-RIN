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
# This code is used to match the positions of the in vitro assays to the RIN
# dataset.
# It assigns a "High" or "Low" chromogenic activity.


upperLimit = 50
lowerLimit = 50

# First, match the A2 domain to the centrality measures

a2 = read.table("../datasets/A2_mutagenesis.csv", header=T, sep="\t", quote="", stringsAsFactors = F)

pos_a2 = match(intersect(net.centr$node, a2$Mature_res_num), a2$Mature_res_num)
pos_net = match(intersect(net.centr$node, a2$Mature_res_num), net.centr$node)

# The chromogenic activity
net.centr$a2_activity = NA
net.centr$a2_level = NA

net.centr$a2_activity[pos_net] = a2$Activity[pos_a2]
net.centr$a2_level[which(net.centr$a2_activity > upperLimit)] = "High"
net.centr$a2_level[which(net.centr$a2_activity < lowerLimit)] = "Low"

# The antigen activity
net.centr$a2_antigen = NA
net.centr$a2_ag_level = NA

net.centr$a2_antigen[pos_net] = a2$Antigen[pos_a2]
net.centr$a2_ag_level[which(net.centr$a2_antigen >= upperLimit)] = "High"
#net.centr$a2_ag_level[which(net.centr$a2_antigen <= upperLimit & net.centr$a2_antigen >= lowerLimit)] = "Medium"
net.centr$a2_ag_level[which(net.centr$a2_antigen < lowerLimit)] = "Low"
#net.centr$a2_ag_level[which(is.na(net.centr$a2_antigen & net.centr$node >=376 & net.centr$node <= 649))] = "No mutant"
#net.centr$a2_ag_level[which(net.centr$a2_antigen == 100 & net.centr$node >=376 & net.centr$node <= 649)] = "WT"


# Next, match the C2 domain to the centrality measures

c2 = read.table("../datasets/C2_mutagenesis.csv", header=T, sep="\t", quote="", stringsAsFactors = F)

pos_c2 = match(intersect(net.centr$node, c2$Mature_res_num), c2$Mature_res_num)
pos_net = match(intersect(net.centr$node, c2$Mature_res_num), net.centr$node)

# The chromogenic activity
net.centr$c2_activity = NA
net.centr$c2_level = NA

net.centr$c2_activity[pos_net] = c2$Activity[pos_c2]
net.centr$c2_level[which(net.centr$c2_activity >= upperLimit)] = "High"
#net.centr$c2_level[which(net.centr$c2_activity <= upperLimit & net.centr$c2_activity >= lowerLimit)] = "Medium"
net.centr$c2_level[which(net.centr$c2_activity < lowerLimit)] = "Low"
#net.centr$c2_level[which(is.na(net.centr$c2_activity & net.centr$node >= 2173 & net.centr$node <= 2325))] = "No mutant"
#net.centr$c2_level[which(net.centr$c2_activity == 100 & net.centr$node >= 2173 & net.centr$node <= 2325)] = "WT"

# The antigen activity
net.centr$c2_antigen = NA
net.centr$c2_ag_level = NA

net.centr$c2_antigen[pos_net] = c2$Antigen[pos_c2]
net.centr$c2_ag_level[which(net.centr$c2_antigen >= upperLimit)] = "High"
#net.centr$c2_ag_level[which(net.centr$c2_antigen <= upperLimit & net.centr$c2_antigen >= lowerLimit)] = "Medium"
net.centr$c2_ag_level[which(net.centr$c2_antigen < lowerLimit)] = "Low"
#net.centr$c2_ag_level[which(is.na(net.centr$c2_antigen & net.centr$node >= 2173 & net.centr$node <= 2325))] = "No mutant"
#net.centr$c2_ag_level[which(net.centr$c2_antigen == 100 & net.centr$node >= 2173 & net.centr$node <= 2325)] = "WT"



find_cutoff = function(dataset){
    upperCut = 90
    lowerCut = 10
    
    result = data.frame(upperCut=NA, lowerCut = NA, a2_mean = NA, c2_mean=NA)
    
    while(upperCut > 30){
        lowerCut = 10
        while(lowerCut < upperCut-10){
            dataset$a2_level[which(dataset$a2_activity >upperCut)] = "High"
            dataset$a2_level[which(dataset$a2_activity <=upperCut & dataset$a2_activity>=lowerCut)] = "Medium"
            dataset$a2_level[which(dataset$a2_activity < lowerCut)] = "Low"
            
            dataset$c2_level[which(dataset$c2_activity >upperCut)] = "High"
            dataset$c2_level[which(dataset$c2_activity <=upperCut & dataset$c2_activity >= lowerCut)] = "Medium"
            dataset$c2_level[which(dataset$c2_activity < lowerCut)] = "Low"
            
            a = TukeyHSD(aov(dataset$dg~dataset$a2_level))[[1]]
            b = TukeyHSD(aov(dataset$dg~dataset$c2_level))[[1]]
            
            tmpResult = data.frame(upperCut = upperCut, lowerCut = lowerCut, a2_mean = mean(a[,4]),
                                   c2_mean = mean(b[,4]))
            
            print(tmpResult)
            result = rbind(result, tmpResult)
            
            lowerCut = lowerCut + 1
        }
        upperCut = upperCut - 1
    }
    
    return(result)
    
}




























