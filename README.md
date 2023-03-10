<p align="center">
  <a href="" rel="noopener">
 <img width=200px height=200px src="./com/cobol.png" alt="Cobol logo"></a>
</p>

<h3 align="center">COBOL Project - Update accounts</h3>

<div align="center">

[![Status](https://img.shields.io/badge/status-active-success.svg)]()
[![GitHub Issues](https://img.shields.io/github/issues/Emericdefay/poe-cobol-project.svg)](https://github.com/Emericdefay/poe-cobol-project/issues)
[![GitHub Pull Requests](https://img.shields.io/github/issues-pr/Emericdefay/poe-cobol-project.svg)](https://github.com/Emericdefay/poe-cobol-project/pulls)
[![License](https://img.shields.io/badge/license-MIT-blue.svg)](/LICENSE)

</div>

---

<p align="center"> Atomic transactions of FLUX file orders
    <br> 
</p>

<h2> 📝 Table of Contents </h2>

- [🧐 About ](#-about-)
- [🎈 Explanations ](#-explanations-)
  - [1. GKCTRL01](#1-gkctrl01)
  - [1t. TSTTRL01](#1t-tsttrl01)
  - [2. MAXXX](#2-maxxx)
  - [2t. TSTMAJ01](#2t-tstmaj01)
  - [3. MFMAJCPT](#3-mfmajcpt)
  - [4. GKMAJ001](#4-gkmaj001)
- [✍️ Authors ](#️-authors-)

## 🧐 About <a name = "about"></a>

The purpose of this project is to apply FLUX changes if FLUX file is not corrupted.

## 🎈 Explanations <a name="Explanations"></a>

### 1. GKCTRL01

The purpose of GKCTRL01 is to check the FLUX file. If this file is corrupted or not. If FLUX is clean and ready to be used, GKCTRL01 do not manipulate RETURNCODE (RC=00)  
Otherwise it gives :
- RC = 1 : DDNAME issue
- RC = 2 : Length DSNAME issue
- RC = 3 : Other open file issue
- RC = 4 : FileIN empty issue
- RC = 5 : number operands != footer expected operands
- RC = 6 : F1-MONTANT-OPER != F1-MT-GLOBAL 
- RC = 7 : No header issue
- RC = 8 : No footer issue

### 1t. TSTTRL01

Its purpose is to test all cases of GKCTRL01, to be sure that at any stage
of the project, GKCTRL01 will assure its goal.

### 2. MAXXX

MAxxx are accessors routines that handle SQL request.
`xxx` is replaced by :
- CPT : Accessing ACCOUNT   table
- DEV : Accessing DEVISE    table
- HIS : Accessing HISTORY   table
- OPE : Accessing OPERATION table

Each accessor has authorisations to do particular requests :  
You can find out those authorisation here : [METHODS allowed](https://github.com/Emericdefay/poe-cobol-project/pull/2)

### 2t. TSTMAJ01

Its purpose is to check accessors' authorisations according to the check list above.

### 3. MFMAJCPT

This routine check if everything in the action is OK, then update the account

### 4. GKMAJ001

This routine read file line by line to feed a data struct. Then call another MFMAJCPT to update account with actions interpreted on those lines.

## ✍️ Authors <a name = "authors"></a>

- [@Emericdefay](https://github.com/Emericdefay) - Program proposed
- [@BOUZIANI Mustafa](#) - Idea & Initial exercice
- [@Global Knowledge](https://www.globalknowledge.com/fr-fr) - Formation
