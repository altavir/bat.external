# Global model description considerations

## Ideas:

Any Bayesian problem could be represented by marginalization of some distribution. Consider the distribution $L(\theta,\nu)$ and prior probability $\pi(\nu)$, where vector $\theta$ represents parameters of interest and vector $\nu$ - nuisance parameters. A problem could be written as following integral:
$$
  L(\theta) = \int{L(\theta,\nu) \cdot \pi(\nu)}.
$$

Let us consider example cases:

* **basic integration**: in this case dimension of $\theta$ is zero and result of integration is a number. Prior probability in this case could simplify calculations if it is a simple function.

* **parameter estimation**: in this case dimension of $\theta$ is usually 1, meaning that result is a univariate function. Prior probability could both represent physical information and support function for integration. In some cases, the integration could split by steps.

* **decision tree**: The same as above, but prior probability represents relation between parameters. For example it could look like
$$
  \pi(a, b, c) = \pi(a|b)\pi(b|c).
$$

## What do we need for objective function?

**TODO**

Consider factorization of function and prior as a special case.


## What do we need for prior probability?

Basically, prior probability could represent two different things: additional physical information and relation between parameters. Additional information usually comes in a form of continuous function and therefore have the same representation as objective function. The relations a constraints could be separated in different classes:

* fixed relations like $ a = f(b, c, ...)$

* boundaries like $ a < f(b, c, ...)$

* distributions like $ p(a| b, c, ...)$

Prior should be passes not as a single function, but as a set of constraints since different type of constraints could be evaluated by integrator in a different way. Since all relations are supposed to be presented as dependence of one parameter on others, one could construct a graph of dependencies of parameters and check it for cycles. Obviously, cyclic graphs could not be resolved. Acyclyc dependency graph could be transformed to step-by-step integration or global integration.


## What additional information could be provided for model?

* derivatives

* global boundaries

* approximate function shape

* preferred algorithm

* parameters for specific algorithms

* convergence criterion
