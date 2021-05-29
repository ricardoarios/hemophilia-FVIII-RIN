### Here is a description of each script used in the study.

> - **boxplot_binding_sites_centralities.R:** This code will produce the boxplots of Figure 5b. Here we depict the centrality measures of the binding site residues and their immediate neighbors in the FVIII RIN.
> - **boxplot_critical_supercritical_residues.R:** First it identifies the key residues of the FVIII RIN (as described in the manuscript), and then displays their relative surface exposed area (RSA).
> - **generate_net_and_centralities.R:** This code will generate the FVIII RIN and calculate its centrality measures. Additionally, it will generate the correlation plot displayed in Figure 2b (the correlation between centrality measures of the RIN). The output is an igraph network and a data frame with the centrality measures.
> - **pareto_front.R:** This code will find the Pareto front of a matrix.
> - **plot_classification_correlations.R:** This code will produce the scatterplot of Figure 3b. Here we depict the correlation of the output of different classifiers. Each classifier produces the predicted probability of severe or mild/moderate hemophilia symptoms.
> - **boxplot_centrality_a2_c2_values.R:** This code will produce the output of Figure 2 of the manuscript. Here we depict the centrality measures of the residues of the A2 and the C2 domains,  after a massive alanine screening assay (see Methods in the manuscript). It also tests the significance of the results.
> - **boxplot_predicted_chromog_in_vitro.R:** This code will produce the Figure 3c of the manuscript. Here we depict a boxplot and a stripchart of the predicted chromogenic activity of mutant constructs, compared to reported in vitro activities.
> - **match_a2_c2_net.R:** This code is used to match the positions of the in vitro assays to the RIN dataset. It assigns a "High" or "Low" chromogenic activity.
> - **plot_centrality.R:** This code will produce the plots depicted in Figures 4d and 4e. These functions depict a centrality measure (closeness), of the different domains of the FVIII protein.
> - **rsa_vs_pred_chromogenic_activity.R:** This code will produce the scatterplot of Figure 3e. Here we depict the relative exposure surface area and the predicted chromogenic activities of mutant constructs.

If you find any issues with the code, please contact us: tiago-jose@ncchd.go.jp, ricardoar@ufba.br.

On the behalf of all of the authors, we appreciate your interest in the FVIII RIN and hope it is useful to your research.
