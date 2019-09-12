## Models

### Installation

	cd node_modules/utils
	npm install

### Usage

e.g. `time webppl final_expts_model.wppl --require utils 1`

Options:

* `--fit_bins`: if this flag is present, we will run mapData over binned histogram priors data
* `--fit_concrete`: if this flag is present, we will run mapData over sorites responses for the concrete premise
* `--fit_inductive`: if this flag is present, we will run mapData over sorites responses for the inductive premise
* `--iterations 10`: number of iterations for MCMC
* `--lag 1`: lag for MCMC
* `--inductive_version s1_inductive`: version of inductive premise model (writeup is for `s1_inductive`)
