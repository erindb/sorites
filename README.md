# Sorites project

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

# Directory structure

## Files

* **`data_summary.Rmd`**
	- ☐ shows data collected so far and fit of log-normal curves
    - summarizes ☑ results from sorites judgments collected so far, with a ☑ clear indication of the precise wording used for each.
* **`data/`**: contains data from all experiments
	- `sorites/`: data from experiments where we elicit ratings for "goodness" of sorites premises
	- `priors/`: data from experiments where we elicit prior probabilities of items costing different amounts
	- each experiment is labeled with a unique number as an identifier.
	- data filenames are `data_exp{NUMBER}_{YEAR}_{MONTH}_{DATE}_{HOUR}.csv`
* **`experiments`**: A copy of the HTML and JavaScript files for running each of the experiments is in `experiments` directory. This directory also includes a subdirectory detailing many of the prior experiments using an old naming scheme.
* **`models/`**: contains webppl models
* **`model/`**: An older attempt at a webppl model of adjectives taking in some fit parameters for the prior. Really, the prior parameters should be inferred from the prior elicitation experiments results within the webppl model.
* **`paper`**: Draft of the paper for this project
* **`writeups`**: Brief .Rmd summaries and graphs for each experiment. These contain more details about the design of experiments.

# Main Experiments

* Priors: Actual source of prior data: Experiment 9 (or Experiment 6?)
	- Experiment 6 prior elicitation has all 5 items, but only 10 Ss.
	- Experiment 9 prior elicitation has only 3 items, but 36 Ss.
* Final sorites experiments: Experiments 10 and 11
	- Experiment 10 uses a conditional statement for the inductive premise ("If a laptop is expensive, then another laptop that costs $E less is also expensive.")
	- Experiment 11 uses a relative clause for the inductive premise ("A laptop that costs $E less than an expensive laptop is also expensive.")
	- Both experiments used relative clauses for the concrete premise ("A laptop that costs $V is expensive.")

# All Experiments

* Pilot experiments
	* Experiment 0: sorites premises where the dollar amounts were not etreme enough to get a variety of judgments (first 30 Ss in file)
	* Experiment 1: sorites premises with more extreme values but still not full range of ratings
	* Experiment 2: binned free response prior experiment
	* Experiment 3: binned sliders prior experiment with 10 bins per item
	* Experiment 4: binned sliders prior experiment with 20 bins per item
	* Experiment 5: binned sliders prior experiment with 40 bins per item
	* Experiment 6: binned sliders prior experiment with varied bin numbers, prior and posterior. **this is the experiment that insipired the dollar amounds in the next sorites premises experiments**
	* Experiment 7: sorites premises experiment with new dollar amounts
	* Experiment 8: a prior elicitation experiment where in one condition, we asked about bins individually
* Sorites priors experiments
	* **ONE PRIOR ELICITATION EXPT** Experiment 9: 3 domain bins prior elicitation experiment
* Sorites premises experiments
	* **ACTUAL SORITES EXPT** Experiments 10 & 11: sorites premises experiment with full range of responses, two different ways of phrasing the inductive premise. Also within this directory is a folder `older-writeups` which contains descriptions of many of the experiments as well as some old model results.

# To do

* [x] Document nicely give a number
* [ ] add in concrete premise
	* [ ] compare model to empirical for concrete
	* [ ] compare prior dist params with and without joint inference
* [ ] Model comparison with unlifted L1 and L0 versions of speaker models
	* [ ] use webppl AIS (use sherlock)
	* [ ] could run into variance issues. if so, we could simplify prior param inference (e.g. outside of AIS, empirical, MAP)
* [ ] pin down semantics of inductive premise
