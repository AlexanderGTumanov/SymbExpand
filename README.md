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

* ``SimplifySymbol[expr]``: Simplifies the arguments of all tensor products within ``expr`` using symbol simplification rules (``RulesSymbol``). It then collects and simplifies all prefactors.
* ``MultiplySymbol[expr]``: Merges products of symbols and ``\[ScriptCapitalR]`` functions in each term of ``expr`` according to symbol multiplication rules.
* ``Convert[funct]`` Computes the symbol of a polylogarithmic function.
* ``ParallelConvert[funct]`` Computes the symbol of a polylogarithmic function using parallel evaluation to speed up the calculation.

**Note:** The ``Convert`` and ``ParallelConvert`` functions currently support only classical polylogarithms. To compute symbols of a broader class of functions, consider using external packages such as [PolyLogTools](https://gitlab.com/pltteam/plt).

* ``SMBtoEQN[expr]``: Decomposes the equation ``expr = 0`` into its independent components, corresponding to individual symbol terms and, in the non-pure case, their rational prefactors.
* ``SMBtoEQN[expr,vars]`` Decomposes the equation ``expr = 0`` into its independent components, corresponding to individual symbol terms and, in the non-pure case, their rational prefactors. It then further splits the resulting equations based on the variables in ``vars``.
* ``ParallelSMBtoEQN[expr]``: Uses parallel evaluation to decompose the equation ``expr = 0`` into its independent components, corresponding to individual symbol terms and, in the non-pure case, their rational prefactors.
* ``ParallelSMBtoEQN[expr,vars]`` Uses parallel evaluation to decompose the equation ``expr = 0`` into its independent components, corresponding to individual symbol terms and, in the non-pure case, their rational prefactors. It then further splits the resulting equations based on the variables in ``vars``.
* ``ClipLastEntry[expr,entry]``: Removes ``entry`` from the end of all symbols in ``expr`` that end with it and sets the remaining terms to zero. If ``entry`` is a list, the function removes multiple last entries at once.
* ``ClipFirstEntry[expr,entry]``: Removes ``entry`` from the beginning of all symbols in ``expr`` that begin with it and sets the remaining terms to zero. If ``entry`` is a list, the function removes multiple first entries at once.

The package provides tools for constructing and constraining bases in the symbol space. It supports two notations: a list of basis vectors and a linear combination of basis vectors with coefficients ``\[ScriptS][i]``. The following functions are available for basis manipulation:

* ``RemoveNumericFactors[list]``: Removes overall numeric factors from a list of symbols.
* ``ToSymbol[list]``: Converts a list of vectors in the symbol space into a linear combination of these vectors.
* ``ToList[symbol]``: Converts a linear combination of symbols into a list.
* ``ToBasis[expr]``: Reduces ``expr``, whether given as a list of symbols or as a linear combination of them, to a linearly independent subset.
* ``Impose[expr,constr]``: Restricts ``expr``, whether given as a list of symbols or as a linear combination of them, to the subset of symbols that satisfy the constraints specified by ``constr``.

Lastly, the package provides the expansion tools themselves:

* ``StandardizeArgument[letter,x]``: Rewrites ``letter`` in the form $b\ x^a\ (1 + \ldots)$, preparing it for expansion in ``x``. The function returns ``{a,b}``.
* ``RemoveLogs[expr,x]`` rewrites ``expr`` to explicitly manifest its logarithmic divergences around the branch point ``x = 0``, expressing them as powers of ``log[x]`` . If ``x = 0`` is not a branch point, it returns ``expr`` unchanged.
* ``ParallelRemoveLogs[expr,x]``: Uses parallel evaluation to rewrite ``expr`` to explicitly manifest its logarithmic divergences around the branch point ``x = 0``, expressing them as powers of ``log[x]`` . If ``x = 0`` is not a branch point, it returns ``expr`` unchanged.
* ``SymbolD[expr,{x,n}]``: Computes the ``n``th derivative of ``expr`` over ``x`` if ``x = 0`` is not a brach point of the function. If it is, the function returns the regular part of the derivative needed to construct the power expansion around this point.
* ``ParallelSymbolD[expr,x]``: Uses parallel evaluation to compute the ``n``th derivative of ``expr`` over ``x`` if ``x = 0`` is not a brach point of the function. If it is, the function returns the regular part of the derivative needed to construct the power expansion around this point.
* ``SymbolSeries[expr,{x,n}]``: Returns the expansion of ``expr`` around ``x = 0`` up to order ``n``.
* ``ParallelSymbolSeries[expr,{x,n}]``: Uses parallel evaluation to return the expansion of ``expr`` around ``x = 0`` up to order ``n``.
* ``SymbolSeriesCoefficient[expr,{x,n}]``: Returns the coefficient of $x^n$ of the series expansion of ``expr`` around ``x = 0``.
* ``ParallelSymbolSeriesCoefficient[expr,{x,n}]``: Uses parallel evaluation to return the coefficient of $x^n$ of the series expansion of ``expr`` around ``x = 0``.

**Note:** The package is not designed to expand expressions whose power series contain terms of negative degree. To process such expressions, the user should first multiply them by a suitable power of the expansion parameter to eliminate all singular behavior.

## $\mathcal{C}$ front space

The simplest and one of the most important function spaces that arise in $\mathcal{N} = 4$ super Yang-Mills theory is the so-called $\mathcal{C}$-space, which is known to contain all three-point form factors of protected local operators within the theory. The package ``CFrontSpace.m`` implements the direct construction of this space by imposing its defining constraints.

The package is formulated in terms of the six-letter alphabet $\mathcal{a},\mathcal{b},\mathcal{c},\mathcal{d},\mathcal{e},\mathcal{f}$ but also allows transitions to other variables, namely the Mandelstam variables $u_1,u_2,u_3$ ($u,v,w$) and the Form Factor OPE varibles $S, TT = T^2$. Transitions between these notations can be accomplished using the replacement rules ``UtoL``, ``LtoU``, ``UtoO``, ``LtoO``. Apart from these, the package contains the following tools:

* ``DihedralOrbit[expr]``: Generates the set of images of ``expr`` under dihedral transformations.
* ``IncreaseTranscendentality[expr]``: Increases the transcendentality of ``expr``, whether given as a list of symbols or as a linear combination of them, by appending an additional entry to each symbol."
* ``CDihedral[expr]``: Generates the dihedral constraints on ``expr``.
* ``CClassicality[expr]``: Generates the classicality (absence of non-classical polylogarithms) constraints on the transcendentality-four part of ``expr``.

**Note:** The $\mathcal{C}$ space is constructed recursively, by appending letters at the end through the use of ``IncreaseTranscendentality`` function and imposing constraints at each step. Both the dihedral and the classicality constraints are not included in this definition and should therefore be only imposed at the very end, prior to matching the ansatz with the data.

* ``CIntegrability[expr]``: generates the integrability constraints (requirement that the symbol corresponds to an actual polylogarithmic function) on the last two symbol entries of ``expr``.
* ``CNonAdjacency[expr]``: Generates the non-adjacency constraints on the last two and three symbol entries of ``expr``.
* ``CFirstEntry[expr]``: Generates the first-entry constraints on expr up to transcendentality four.

**Note:** Because the first-entry constraints are implemented only up to transcendentality four, beyond this point, the package produces slightly larger spaces.

* ``\[ScriptCapitalC][n]`` Returns a list of basis elements in the $\mathcal{C}$-space of transcendentality ``n``.

[^1]: Up to transcendental constatnts.
