# Symbol expansion for Mathematica
A package for series expanding the symbols of polylogarithmic functions in Wolfram Mathematica and implementing the C-space of 3-point form factors, defined first in 2204.11901.
## Installation
Clone this repository using
```console
git clone https://github.com/AlexanderGTumanov/symbol-expansion-for-mathematica.git
```
This will create a new directory ``symbol-expansion-for-mathematica`` in the current working directory. Alternatively, you can manually download the ``SymbolExpansion.m`` and  ``CFrontSpace.m`` files. In your notebook file, add
```mathematica
SetDirectory["symbol-expansion-for-mathematica"];
<<SymbolExpansion`;
<<CFrontSpace`;
```
