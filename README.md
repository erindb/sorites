# Sorites project

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

* get Justine's data and check lognormal fit against inferred parameters for Experiment 6a and Experiment 9 [E]
* implement alternative models [M]
* look at distributions of responses for sorites [M] 

Bayesian data analysis and model comparison.

Alternative models:

* quantile model
* unlifted threshold variable
* listener models + transformation:
	* literal (L0) model
	* listener (L1) model
* speaker models:
	* s1
	* s2