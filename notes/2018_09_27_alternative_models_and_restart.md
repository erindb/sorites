# 2018 september 27

## alternative models

"X is tall" means height(x) > $\theta$

Where does $\theta$ come from?

Our model:

$\theta$ is a specific height, inferred pragmatically, s.t. someone would bother to say "X is tall", based on RSA, a uniform prior over $\theta$, and the distribution of heights of the relevant category of X. $\theta$ is inferred by L1 and assumed to be known to S1 and L0. 

Other models:

* Same as above, but $\theta$ is *unlifted*, inferred by L0
* $\theta$ is a specific *quantile* of the relevant distribution, either fixed lexically or inferred pragmatically (Michael & Anthea)
* listener models + transformation:

* * literal (L0) model
  * listener (L1) model

## restart

### basic outline

vagueness

* borderline cases
* context sensitivity
* sorites

new empirical data!!!

open mind ~4K words, philosophy audience

### model comparisons

* unlifted
* s1/s2

### rationale for experiment

**Premise 1:** The Sears Tower is tall.

As we vary height of X, we get **borderline cases**.

As we change category, we get **context sensitivity**.

**Premise 2:** A building that is 1m shorter than a tall building is tall.

As we change $\varepsilon$, we get **sorities**.

### example figures

Fig 13 in generics

prior dist params with and without joint inference

Also, Michael does empirical hist and model density, which is nice.

### To Do

* ☐ Document nicely give a number

* ☐ add in concrete premise
  * compare model to empirical for concrete
  * compare prior dist params with and without joint inference
* ☐ Model comparison with unlifted L1 and L0 versions of speaker models
  * use webppl AIS (use sherlock)
  * could run into variance issues. if so, we could simplify prior param inference (e.g. outside of AIS, empirical, MAP)
* ☐ pin down semantics of inductive premise