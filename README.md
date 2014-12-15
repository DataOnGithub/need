tl;dr - In deciding whether to oppose the generous Medicaid dollars offered as part of the Affordable Care Act, governors are extremely responsive to the political context but unresponsive to the level of need in their state.

The latest version of the manuscript is available [here](http://www.carlislerainey.com/files/need.pdf) and the appendix is [here](http://www.carlislerainey.com/files/need_appendix.pdf). Replicating these results requires the state-level opinion data set available [here](https://github.com/carlislerainey/ACA_Opinion/blob/master/Data/mrp_est.csv), but the R scripts automatically grab these data.

To replicate the results in the paper, you'll need to change the working directories at the top of the R scripts in the `R_Code` directory and run the script `do_all.R`. This is computationally intensive, so it takes about four hours. If you care about just the main model, you can run just `mcmc.R`. The scripts `rc_alternative_strategies.R` and `random_forest.R` reproduce the two robustness checks discussed at the end of the main text. The remaining `rc_*.R` scripts conduct the miscellanous robustness checks discussed in the appendix.


 [This data is mirrored and can be queried via API here](https://www.exversion.com/data/collection/view/548f03d477990)