---
author:
- 'A. Khudyakov'
bibliography:
- 'bib/bayes.bib'
- 'bib/MCMC.bib'
- 'bib/books.bib'
- 'bib/CS.bib'
title: Short and incomplete history of practical Bayesian inference
---

Introduction
============

Bayesian inference is well known and widely used technique but its usage
is very unevenly distributed. For example in field of particle physics
it's only starting to see adoption. Its successes only became possible
due to massive amount of computational power available today. This is
incomplete review of history computational methods and software for
Bayesian inference. It's not complete but hopefully all most notable
libraries/probabilistic programming languages (PPL) are present.

History of Bayesian inference started in XVIII century when Reverend
Thomas Bayes discovered Bayes theorem which was published after his
death in 1763:

$$P(A|B) = \frac{ P(B|A)P(A) }{ P(B) }$$

If we interpret probabilities as state of knowledge we can use rule
above to update our knowledge about system. If for example $\theta$ is
set of parameters we want to estimate and $X$ is experimental data we
can replace $A$ with $\theta$ and $B$ with $X$ getting:

$$
p(\theta|X)
  = \frac{p(X|\theta)p(\theta)}{p(X)}
  = \frac{p(X|\theta)p(\theta)}{\int d\theta\, p(X|\theta)p(\theta)}
$$

Unfortunately integral in the denominator is not analytically tractable
in most cases. So one either limited to conjugated priors[^1] or have to
use various approximations. That naturally greatly limited possible
uses.

Revolution came in 1990s when it was realized by Gelfand and
Smith[@gelfand1990sampling] that Markov chain Monte Carlo (MCMC) could
be used to sample from posterior distribution without knowing
normalization factor. Then BUGS[@gilks1994language] DSL[^2] was
demonstrated at the 4th Bayesian Valencia meeting in April 1991. Notably
according to [@lunn2009bugs] it was developed independently from
Gelfand.

After that field saw a lot of development, new algorithms were devised,
new libraries and PPLs were created. Understanding of Markov chains
improved too. This review concentrates on features of software. Much
more detailed history of use MCMC in statistics is presented in ch. 2 of
"Handbook of MCMC"[@brooks2011handbook].

Markov chain Monte-Carlo
========================

Computational heart of Bayesian inference is MCMC. Here is quick
overview of existing algorithms.

Markov chain is stochastic process where probability of transition only
depends on current state. Some of them are ergodic which means limiting
probability of being in state $X$ does not depend on starting point. It
could be proven that MC is ergodic if it's irreducible, recurrent, and
aperiodic. MCMC is primarily about constructing such chains and ensuring
that they mix well. In other words they're able to explore distribution
in reasonable time.

Metropolis-Hastings-Green algorithm
-----------------------------------

Metropolis-Hastings[@metropolis1953equation][@hastings1970monte]
([Green]{style="color: red"}[$^{\mbox{\tiny{CITE?}}}$]{style="color: red"})
algorithm is widely used one. Let assume that one want to sample from
distribution $\pi(x)$.

1.  Proposal

2.  Acceptance

3.  Repeat

Gibbs sampler
-------------

Hamiltonian Monte-Carlo
-----------------------

Software
========

Several different models exist. We only will concern ourselves with
directed Bayesian networks.

[Definition, Local Markov assumption]{style="color: red"}

[Example & Pic]{style="color: red"}

[Special case: simple inference, hierarchical Bayes]{style="color: red"}

BUGS
----

BUGS[@gilks1994language] is probably most influential software package
for Bayesian inference. It become first widely used PPL and led to
creation of several derivative languages. BUGS is acronym: Bayesian
Inference Using Gibbs Sampling although Metropolis-Hastings samples have
been added later. Its development started at 1989 and it was publicly
demonstrated in 1991 (for more details about history see retrospective
paper[@lunn2009bugs]).

It's declarative external DSL. Language is very simple and allows to
specify probabilistic models concisely. It has stochastic variables,
deterministic variables and arrays. Unfortunately formal specification
of language seemingly doesn't exists so we have to use examples. Here is
specification of linear regression:

          model {
            for (i in 1:N) {
              y[i]  ~ dnorm(mu[i], tau)
              mu[i] <- alpha + beta * x[i]
            }
            alpha     ~ dnorm(m.alpha, p.alpha)
            beta      ~ dnorm(m.beta,  p.beta)
            log.sigma ~ dunif(a, b)
            sigma    <- exp(log.sigma)
            sigma.sq <- pow(sigma, 2)
            tau      <- 1 / sigma.sq
          }

Here `~` means "distributed as" (stochastic variables) and `<-`
denotes functional dependence (deterministic variables). Since language
is declarative order of declarations doesn't matter (except in some
corner cases). This also means that value could only be assigned once.
Values `x` and `N` are not defined in model and therefore must be
supplied to interpreter as external data.

BUGS is interpreted language. So model specification is first parsed,
then DAG is built from nodes (variables). Then it decides what is best
way to sample each stochastic variable and convert to some low-level
representation which is later used for execution.

Being interpreter BUGS is not known for outstanding performance. For
example it have to store each element of array as separate variable and
update them in turn.

Actually BUGS is whole family of PPL. Original BUGS was written in
MODULA-2. It was superseded by WinBUGS in 1997 which has powerful GUI
which allowed to specify models graphically and was much more
comprehensive. As its names says it's only available for Windows since
it's written in Component Pascal and depends on BlackBox framework[^3].
In next decade development started to diverge.

JAGS[@plummer2003jags] as BUGS clone written in C++ was presented in
2003. Its goals was ability to add new distributions easily, to make it
possible to use it on UNIX-like systems and possibly to vectorize
operations on arrays.

Then development on OpenBUGS started in 2004. Initially it had following
goals: a) decouple user interface from core functionality, b) to allow
use of BUGS from other environments c) allow more platform independence.
Second one resulted in BRugs which allowed interactive use from R.

Stan
----

Stan[@carpenter2016stan][@t2015stan] is another PPL. Just as BUGS it's
an external DSL and there're some similarities in syntax, but it's
imperative language while BUGS is declarative. It also have "blessed"
interfaces for python, R, and some other languages.

Unlike BUGS stan is compiled language. Stan programs are transpiled to
C++ programs which in turn are compiled by g++/clang++. This allows to
obtain good performance at cost of convoluted compilation pipeline. C++
templates are used extensively. All linear algebra is built upon
eigen[$^{\mbox{\tiny{CITE?}}}$]{style="color: red"} library.

Another notable feature is use of Hamiltonian
Monte-Carlo[@duane1987hybrid] with No U-Turns Sampler
(NUTS)[@hoffman2014NUTS]. It's probably most efficient form of MCMC for
sampling from posterior with continuous parameters. It however cannot
handle discrete parameters. At this point only stan, PyMC3 and
LaplaceDemon package for R use this variant of MCMC. Another limitation
of Hamiltonian Monte-Carlo is need to know derivatives of probability
density. Stan uses automatic differentiation to calculate them.

### Types

Now lets turn to language. It's explicitly statically typed. Primitive
types are `real` for IEEE754 doubles and `int` for 32-bit integers.
Integers are automatically promoted to reals but not vice versa. Other
data types are arrays, vectors, and matrices. Vector and matrices are
distinct from arrays and only they could be used in vector algebra
routines. They're also restricted to reals as their elements.

It's also possible to put constraints on possible values that variable
could contain. Notably arbitrary expressions could be used as
constraints. So for example:

      data {
        int<lower=1> N;
        real y[N];
      }
      parameters {
        real<lower=min(y), upper=max(y)> phi;
      }

declares integer variable `N` which must be at least 1 and array of
reals `y`. Then `phi` is parameter variable which is constrained be in
range of values in `y` array.

This doesn't exhaust list of types supported by stan language. It
distinguish between row/column vectors and have specific data types for
unit vectors, covariance matrices, simplexes, etc. Reader should consult
language manual[@t2015stan] for details.

### Program structure

Stan program is structured as sequence of blocks. All blocks are
optional and could be omitted. However they order is fixed and they must
appear in same order as in listing below. Scope of all variables extends
to all subsequent blocks.

      functions {
        // ... function declarations and definitions ...
      }
      data {
        // ... declarations ...
      }
      transformed data {
        // ... declarations ... statements ...
      }
      parameters {
        // ... declarations ...
      }
      transformed parameters {
        // ... declarations ... statements ...
      }
      model {
        // ... declarations ... statements ...
      }
      generated quantities {
        // ... declarations ... statements ...
      }

Function block is used to define user functions. Data block defines
external data for the model. Transformed data contains transformation of
data into form more convenient for the model and executed only once.
Parameters declare model's parameters. It's what is sampled or
optimized. Transformed parameters allows to defined variables in terms
of data and parameters to be reused later. Model block contain
description of model and generated quantities are values which are
generated once per sample and saved for output alongside with
parameters.

PyMC
----

PyMC[@patil2010pymc] is python library for performing Bayesian
inference. Its development started in 2003 as effort to make MCMC more
accessible for non-statisticians. Unlike BUGS and stan it's not a
standalone language but a library which could be easily embedded in
larger python applications.

To achieve good performance library makes used of
NumPy[$^{\mbox{\tiny{CITE?}}}$]{style="color: red"}and hand-written
FORTRAN routines. Thus a long as bulk of operations was vectorized it
performed better than BUGS otherwise it was comparable with it.

In user code each stochastic variable is represented as instance of
`Stochastic` class and deterministic relations between parameters are
modelled as instances of `Deterministic` class. Library provide several
methods of construction of `Stochastic`/`Deterministic` values. There's
number built-in subclasses of `Stochastic` (normal, Poisson, etc.) and
ordinary python functions could be turned into objects by use of
decorators[$^{\mbox{\tiny{CITE?}}}$]{style="color: red"}. Also
unobserved parameters and observed data are modelled using same
`Stochastic` values. Observed values just have experimental data
attached to them. Overall API makes heavy use of python's dynamic
features.

MCMC method employed by PyMC is mix of Gibbs sampling and
Metropolis-Hastings. Variables are sampled in turn like in Gibbs but
vector-valued use MH to update. It's possible to assign sampler to
variable manually but library tries best to select suitable one by
default. It's possible to add new sampler without modifying library [Is
description of algorithm correct?]{style="color: red"}

Another feature of library is pluggable "backend". In PyMC parlance
"backend" stores data generated by chain. Storage in RAM, python
pickles, text files, SQLite database and HDF5 is supported on of the
box.

PyMC3
-----

PyMC3[@salvatier2016pymc3] is further development of PyMC. It was first
released in 2017 so quite recent development. It's current state of art
for MCMC in python and old PyMC now is of historical interest. It's
rework of both API which now allows to specify models in more compact
way and internals. Notable addition is use of NUTS[@hoffman2014NUTS]
sampler for Hamiltonian
Monte-Carlo[$^{\mbox{\tiny{CITE?}}}$]{style="color: red"}which greatly
improved speed of convergence.

To boost performance it uses
Theano[@bergstra2010theano][@arXiv1211.5590] which allows to generate
efficient machine code from python. Most notably it's used to evaluate
gradients which are needed for HMC using automatic differentiation.

Infer.NET
---------

Infer.NET[@InferNET14] is C\# library which is developed at Microsoft
research. Its development started in 2008, and first non-beta release
was made in 2014. It's library as well but unlike PyMC it's statically
typed.

For inference it uses Gibbs sampling so it's not very interesting from
algorithmic point of view. It also supports approximate deterministic
algorithms: expectation propagation[@minka2001expectation] and
variational message passing[@winn2005variational].

Anglican
--------

Anglican[@tolpin2016anglican] is embedded DSL for clojure and offers
interoperability with other JVM languages.

\bibliographystyle{unsrt}

[^1]: Posterior distribution have same form as prior.

[^2]: DSL --- domain specific language.

[^3]: `http://blackboxframework.org`
