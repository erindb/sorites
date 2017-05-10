# Sorites project

## Files

* `data_summary.Rmd`
	- shows data collected so far and fit of log-normal curves
    - summarizes results from sorites judgments collected so far, with a clear indication of the precise wording used for each.
* `data/`: contains data collected from all experiments
	- `sorites/`: raw data from experiments where we elicit ratings for "goodness" of sorites premises
	- `priors/`: raw data from experiments where we elicit prior probabilities of items costing different amounts
	- `reformat_data.R`: script to parse experiment data into tidy format

## Motivation

We consider the following argument:

**Premise 1:** The Sears Tower is tall.

**Premise 2:** A building that is 1m shorter than a tall building is tall.

**Conclusion:** Every building in Chicago is tall.

When people hear this argument, they tend to think that premise 1 (the “concrete” premise) and premise 2 (the “inductive”
premise) are both clearly true, but that the conclusion – which would naturally follow from the premises according to first
order logic – is clearly false.

We explain people’s reactions to statements of this kind with a formal model of scalar adjective interpretation and show that their reactions are sensitive to the prior distribution on building heights and to the change in height ε given in the inductive premise (in this case, 1 meter).

We actually do all of this in the domain of prices.

# Directory structure

* **`data`**: Anonymized data files are in `data` directory.
* **`experiments`**: A copy of the HTML and JavaScript files for running each of the experiments is in `experiments` directory. This directory also includes a subdirectory detailing many of the prior experiments using an old naming scheme.
* **`model`**: An attempt at a webppl model of adjectives taking in some fit parameters for the prior. Really, the prior parameters should be inferred from the prior elicitation experiments results within the webppl model.
* **`paper`**: Draft of the paper for this project
* **`writeups`**: Brief .Rmd summaries and graphs for each experiment. These contain more details about the design of experiments.
