# Sorites project

## Files

* `data_summary.Rmd`
	- ☐ shows data collected so far and fit of log-normal curves
    - summarizes ☑ results from sorites judgments collected so far, with a ☑ clear indication of the precise wording used for each.
* `data/`: contains data from all experiments
	- `sorites/`: data from experiments where we elicit ratings for "goodness" of sorites premises
	- `priors/`: data from experiments where we elicit prior probabilities of items costing different amounts
	- each experiment is labeled with a unique number as an identifier.
	- data filenames are `data_exp{NUMBER}_{YEAR}_{MONTH}_{DATE}_{HOUR}.csv`
* `experiments/`: contains html files to run experiment as participants saw them
* `models/`: contains webppl models

## Motivation

We consider the following argument:

**Premise 1:** The Sears Tower is tall.

**Premise 2:** A building that is 1m shorter than a tall building is tall.

**Conclusion:** Every building in Chicago is tall.

When people hear this argument, they tend to think that premise 1 (the “concrete” premise) and premise 2 (the “inductive”
premise) are both clearly true, but that the conclusion – which would naturally follow from the premises according to first
order logic – is clearly false.

We explain people’s reactions to statements of this kind with a formal model of scalar adjective interpretation and show that their reactions are sensitive to the prior distribution on building heights and to the change in height ε given in the inductive premise (in this case, 1 meter).

We do all of this in the domain of prices.

