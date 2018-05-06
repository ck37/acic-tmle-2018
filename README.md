# Targeted learning with censored outcomes

**2018 Atlantic Causal Inference Conference Data Analysis
Challenge**

**Under development**

**Description**: Targeted minimum loss-based estimation (TMLE) was implemented
using ...

**Acknowledgments**: We thank ...

**Expected runtime**: TBD
covariates.

**Notes**: ...

## Requirements

* R 3.2 or later, R 3.3+ recommended.
* Java JDK for rJava
* R Packages:
    * CRAN: bartMachine, caret, devtools, doMC, earth, ggplot2, glmnet, kernlab,
       mgcv, nnet, randomForest, ranger, RhpcBLASctl, xgboost
    * Github: [ecpolley/SuperLearner](https://github.com/ecpolley/SuperLearner),
      [ck37/ck37r](https://github.com/ck37/ck37r)
* Hardware assumptions: 4 CPU cores available for multi-threaded algorithms
    (BART, Ranger, XGBoost), 16GB+ RAM, and a UNIX-based operating system.

## How to run

_Minimal_:

* Make sure java JDK is installed and R can load rJava & bartMachine packages.
* Run setup.R to install other necessary packages: `make setup`
* Modify targeted_learning.R settings at the top of the file if necessary.
* ./targeted_learning.R inputData outfile1 outfile2

_Analysis of 2016 or 2017-pre data_:

* Unzip [2017 data](http://faculty.chicagobooth.edu/richard.hahn/pre_data.tar.gz) into `inbound/pre_data/`
* Unzip [2016 data](https://drive.google.com/file/d/0B8TUkApaUlsGekFSblJWa25NM1E/edit) into `inbound/data-2016/`
* Run import-2016.R to import the 2016 data: `make import-2016`
* Run test-2016.R to conduct a single test analysis of 2016: `make test-2016`
* Run analyze-2016.R to analyze all 2016 files using targeted_learning.R: `make analyze-2016`
* Run import-2017.R to import the 2017 data.

## Subdirectory layout

* Data - working RData files generated during analysis, not tracked via git.
* Exports - exported files (cvs, tsvs, etc.) that are not tracked via git.
* Inbound - input datasets that are not tracked via git.
* Lib - R source code that defines functions; all .R files are loaded.
* Output - log output files from Savio jobs  etc.
* Scripts - shell (BASH) scripts.
* Simulations - simulation studies.

## Troubleshooting

Please feel free to post any issues to the issue queue or email us.

#### rJava issues

There can be issues installing and using rJava for bartMachine. If necessary, one edit from Vince Dorie for cluster usage is to manually load libjvm.so:
```r
# Update this path to the appropriate one for your system.
dyn.load("/usr/lib/jvm/java-1.8.0-ibm-1.8.0.3.10-1jpp.2.el7_2.x86_64/jre/lib/amd64/compressedrefs/libjvm.so")
```

## References

Balzer, L. B., Petersen, M. L., & Laan, M. J. (2016). [Targeted estimation and inference for the sample average treatment effect in trials with and without pair‐matching](http://onlinelibrary.wiley.com/doi/10.1002/sim.6965/full). Statistics in medicine, 35(21), 3717-3732.

Chipman, H. A., George, E. I., & McCulloch, R. E. (2010). [BART: Bayesian additive regression trees](http://projecteuclid.org/euclid.aoas/1273584455). The Annals of Applied Statistics, 4(1), 266-298.

Dorie, V., Hill, J., Shalit, U., Scott, M., & Cervone, D. (2017). [Automated versus do-it-yourself methods for causal inference: Lessons learned from a data analysis competition](https://arxiv.org/abs/1707.02641). arXiv preprint arXiv:1707.02641.

Green, D. P., & Kern, H. L. (2012). [Modeling heterogeneous treatment effects in survey experiments with Bayesian additive regression trees](https://academic.oup.com/poq/article/76/3/491/1893905/Modeling-Heterogeneous-Treatment-Effects-in-Survey). Public opinion quarterly, nfs036.

Hill, J. L. (2011). [Bayesian nonparametric modeling for causal inference](http://amstat.tandfonline.com/doi/abs/10.1198/jcgs.2010.08162). Journal of Computational and Graphical Statistics, 20(1), 217-240.
 
Hubbard, A. E., Jewell, N. P., & van der Laan, M. J. (2011). [Direct effects and effect among the treated](https://link.springer.com/chapter/10.1007/978-1-4419-9782-1_8). In Targeted Learning (pp. 133-143). Springer New York.

Kapelner, A., & Bleich, J. (2014). [bartmachine: Machine learning with bayesian additive regression trees](https://arxiv.org/abs/1312.2171). arXiv preprint arXiv:1312.2171.

Luedtke, A. R., & van der Laan, M. J. (2016). [Super-learning of an optimal dynamic treatment rule](https://www.degruyter.com/view/j/ijb.2016.12.issue-1/ijb-2015-0052/ijb-2015-0052.xml). The international journal of biostatistics, 12(1), 305-332.

Polley, E., LeDell, E., Kennedy, C., Lendle, S., & van der Laan, M. J. (2017). [R Package ‘SuperLearner’](https://cran.r-project.org/package=SuperLearner). Development version 2.0-22.

Polley, E., & van der Laan, M. (2009). [Selecting optimal treatments based on predictive factors](http://biostats.bepress.com/cgi/viewcontent.cgi?article=1247&context=ucbbiostat). Design and Analysis of Clinical Trials with Time-to-Event Endpoints, 441-454.

van der Laan, M. J., & Gruber, S. (2016). ["One-Step Targeted Minimum Loss-based Estimation Based on Universal Least Favorable One-Dimensional Submodels"](http://biostats.bepress.com/ucbbiostat/paper347). U.C. Berkeley Division of Biostatistics Working Paper Series. Working Paper 347.

van der Laan, M. J., Polley, E. C., & Hubbard, A. E. (2007). [Super learner](https://www.degruyter.com/view/j/sagmb.2007.6.1/sagmb.2007.6.1.1309/sagmb.2007.6.1.1309.xml). Statistical applications in genetics and molecular biology, 6(1).

van der Laan, M. J., & Rose, S. (2011). [Targeted learning: causal inference for observational and experimental data](http://www.targetedlearningbook.com/). Springer Science & Business Media.

## License

&copy; 2018 

The contents of this repository are distributed under the MIT license. See file
`LICENSE` for details.
