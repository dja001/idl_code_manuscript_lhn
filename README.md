# IDL code repository

This repository contains all the IDL code that has been used for the manuscript titled:
"Latent Heat Nudging in the Canadian Regional Deterministic Prediction System."


## Description

IDL scripts are divided into two directories. 

* article_lhn contains all the code necessary for 
  * the generation of 10~km reflectivity composites 
  * the generation of scores from various forecasts
  * the generation of all figures found in the manuscript except Fig. 1.

* utils contains multi-purpose scripts that are used by many of the routines above 



## Installing

No installation is required. Just make sure that both directories are accessible to IDL. 

For convenience, you may add:

<code> !PATH = !PATH + ':'+EXPAND_PATH('+path/to/article_lhn/')</code>

<code> !PATH = !PATH + ':'+EXPAND_PATH('+path/to/utils/')      </code>

to an .idl_startup.pro file

## Authors

**Dominik Jacques** 

**Daniel Michelson** has actively participated in this project by running BALTRAD and generating 
2.5~km quality-controlled reflectivity composites. 

Contributions from Jean-François Caron and Luc Fillion are also acknowledged.

## License

This project is licensed under the MIT License - see the [LICENSE.md](LICENSE.md) file for details

