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
