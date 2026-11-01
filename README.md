# Beyond Agreement: Measurement Multiverses for Deductive Coding

This repository accompanies a proposal submitted to the *Organization Science* special issue **“AI-Assisted Frontiers in Organizational Science.”**

It contains a proof-of-concept R artifact for treating human coders and LLM configurations as coding instruments, estimating systematic variation in their decisions, generating alternative measurements for a declared target coding process, and propagating those measurements through downstream analyses.

## Repository contents

- `qualbias.R` — prototype implementation  
- `illustration.R` — executable demonstration using synthetic data  
- `Technical_Appendix.pdf` — statistical framework and function documentation  
- `Proposal.pdf` — project proposal  

## Running the illustration

Install the required R packages:

```r
install.packages(c("glmnet", "Matrix", "softImpute", "ggplot2"))
```

Place `qualbias.R` and the illustration script in the same directory, then run the illustration script in R.

## Prototype status

This artifact demonstrates the project’s core methodological ideas and is not yet a fully validated, general-purpose package. Current results are conditional on the coding design, model specification, declared target process, and examined coding regimes. Planned development will strengthen identification diagnostics, uncertainty propagation, intervention-based measurement generation, and out-of-sample validation.
