Code adapted from [MH's Generics Project](https://github.com/mhtess/generic-interpretation).

## Modeling new generic interpretation data

- `generics-bda-L0-iMH.wppl`: 
	- to run: `webppl generics-bda-L0-iMH.wppl --require utils uncertain chainNum` (where `chainNum` is just used for id purposes)
	- lesioned form of the model can be run using the following:
		- fixed-threshold at 0: `webppl cimpian-bda-L0-iMH.wppl --require utils some chainNum`
		- fixed-threshold at 0.5: `webppl cimpian-bda-L0-iMH.wppl --require utils most chainNum` (note that with a fixed-threshold at 0.5, some responses will be literally impossible; thus, this model inherits and extra "noise" parameter)

### Utils package

This holds much of the shared code for use with the models.

Contents:
  - `utils.wppl`: helper functions (e.g., `discretizedBeta`, `avoidEnds`)
  - `utils.js`: helper functions for reading and writing data (and any function of the form `utils.fn()`)