---
title: Example problems for BAT
---

\maketitle
This collection of random problem which are intended to guide API design
for BAT2. Following notation is used:

-   $x \sim F(...)$ means that value $x$ is random and distributed
    according to $F$.

-   $x = y$ means that $x$ is equal to $y$ and dependence is
    deterministic.

-   $\operatorname{Observed}$ mean that value is observed in experiment.
    Actual value of data is not of interest and therefore omitted.

-   Any value that do not appear on the left side of equation is assumed
    to be known constant.

Please note that no distinction is made between observed value and
parameter in problem statement. Both are random quantities except former
have experimental data attached and should be treated differently during
inference.

Simple histogram fitting
========================

$$
  \begin{aligned}
    \vec\theta \sim& \,\mbox{Some fixed prior} \\
    n_i        \sim& \operatorname{Poisson}[ f_i(\vec\theta) ] \\
    &\operatorname{Observed}[n_i]\\
  \end{aligned}
$$

Very simple and standard problem. It's usually solved by numeric
optimization of likelihood function (which corresponds to flat prior).
MCMC is only worthwhile if likelihood have complicate shape or if one
want to use MCMC to get good initial approximation.

From API design point of view main challenge is what is elegant and
performant way to incorporate vector-valued observables.

Deconvolution
=============

$$
  \begin{aligned}
    \alpha   \sim& \,\mbox{Improper flat prior} \\
    \vec\phi \sim& \mathcal{N}(0, \alpha\Omega) \\
    \vec{f}  \sim& \mathcal{N}(K\vec\phi, \Sigma) \\
    &\operatorname{Observed}[\vec{f}] \\
  \end{aligned}
$$

This is pretty standard hierarchical Bayes model. Main problem is to
allow specifying distributions for $\vec\phi$ and $\vec{f}$ differently.
