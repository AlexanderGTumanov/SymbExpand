# Symbol expansion for Mathematica
A package for series expanding the symbols of polylogarithmic functions in Wolfram Mathematica and implementing the C-space of 3-point form factors, defined first in 2204.11901.
## Installation
Clone this repository using
```console
git clone https://github.com/AlexanderGTumanov/symbol-expansion-for-mathematica.git
```
This will create a new directory ``symbol-expansion-for-mathematica`` in the current working directory. Alternatively, you can manually download the ``SymbolExpansion.m`` and  ``CFrontSpace.m`` files. The ``CFrontSpace.m`` package depends on functions from SymbolExpansion.m and will automatically load it upon initialization. Therefore, you can load both packages with:
```mathematica
SetDirectory["symbol-expansion-for-mathematica"];
<<CFrontSpace`;
```
If ``CFrontSpace.m`` is not needed, load only ``SymbolExpansion.m``:
```mathematica
SetDirectory["symbol-expansion-for-mathematica"];
<<SymbolExpansion`;
```
## Introduction
In perturbative quantum field theory, observables are hypothesized to reside in spaces of polylogarithmic functions. In the simplest cases — such as scattering amplitudes and form factors of local operators with a small number of external momenta — these spaces are known to consist exclusively of generalized polylogarithms, without any elliptic behavior.

These function spaces naturally possess Hopf algebra structures, which enable the transformation of functional analysis problems into linear algebra problems. This is achieved through the **symbol map**, which uniquely[^1] represents a polylogarithmic function as a tensor-product-like object:

$$
\sum\limits_{I_n}\ c_{i_1\ldots i_n}\ a_{i_1}\otimes \ldots \otimes a_{i_n}\ .
$$

The length of the tensor product, $n$, corresponds to the **transcendentality** of the polylogarithmic function. Its entries, $a_i$​, are known as **symbol letters** — functions of external kinematics that encode the locations of the observable's branch points. The set of all symbol letters forms the **alphabet**, a fundamental characteristic of the observable that, in most relevant cases, remains unchanged at any given loop order. In pure function spaces, the coefficients $c_{i_1\ldots i_n}$​​ are rational numbers. More generally, they can be rational functions of the external kinematics.

This representation gives rise to powerful bootstrap techniques, which aim to compute observables by determining $c_{i_1\ldots i_n}$ through the imposition of constraints. This often requires expanding the observable around certain kinematic limits. In the symbol language, such expansions can be challenging, especially when performed around a branch point, where logarithmic singularities arise. This package provides tools to automate this process.

## Usage
The symbol tensor product is represented by the ``SMB`` function. The expression ``SMB[a[1], ..., a[n]]`` corresponds to $a_1\otimes\ldots\otimes a_n$. In non-pure function spaces, rational prefactors should be wrapped with the ``\[ScriptCapitalR][_]`` function. The package provides the following operations on these objects:

* ``SimplifySymbol[expr]``: Simplifies the arguments of all tensor products within expr using symbol simplification rules (``RulesSymbol``). It then collects and simplifies all prefactors.
* ``MultiplySymbol[expr]``: Merges products of symbols and ``\[ScriptCapitalR]`` functions in each term of ``expr`` according to symbol multiplication rules.
* ``Convert[funct]`` Computes the symbol of a polylogarithmic function.
* ``ParallelConvert[funct]`` Computes the symbol of a polylogarithmic function using parallel evaluation to speed up the calculation.

**Note:** The ``Convert`` and ``ParallelConvert`` functions currently support only classical polylogarithms. To compute symbols of a broader class of functions, consider using external packages such as [PolyLogTools](https://gitlab.com/pltteam/plt).

* ``ClipLastEntry[expr,entry]``: Removes ``letter`` from the end of all symbols in ``expr`` that end with it and sets the remaining terms to zero. If ``entry`` is a list, the function removes multiple last entries at once.
* ``ClipFirstEntry[expr,entry]``: Removes ``letter`` from the beginning of all symbols in ``expr`` that begin with it and sets the remaining terms to zero. If ``entry`` is a list, the function removes multiple first entries at once.
* ``StandardizeArgument[expr,x]``: rewrites expr in the form $b\ x^a\ (1 + \ldots)$, preparing it for expansion in ``x``. The function returns ``{a,b}``.
[^1]: Up to transcendental constatnts.
