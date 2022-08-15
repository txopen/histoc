## R CMD check results
There were no ERRORs nor WARNINGs. 

There were three notes:

### 1st Note
This note happens on all platforms.  

```
* checking CRAN incoming feasibility ... NOTE
  Maintainer: 'Bruno Lima <balima78@gmail.com>'
  
  New submission
  
  Possibly misspelled words in DESCRIPTION:
    Histocompatibility (2:8, 12:77)
```
This word is not misspelled so this note can be ignored.

### 2nd Note
This note happens on the platform Windows Server 2022, R-devel, 64 bit.  

```
* checking for detritus in the temp directory ... NOTE
Found the following files/directories:
  'lastMiKTeXException'
```
As noted in [R-hub issue #503](https://github.com/r-hub/rhub/issues/503), this could be due to a bug/crash in MiKTeX and can likely be ignored.


### 3rd Note
This note happens on the platform Fedora Linux, R-devel, clang, gfortran.

```
* checking HTML version of manual ... NOTE
Skipping checking HTML validation: no command 'tidy' found
```

Which seems to indicate that this machine does not have tidy installed, i.e., the note does not seem to be related to our package.

## Downstream dependencies
This is the first time we try to publish in CRAN, and because of that we can't run the command provided by revdepcheck as described [here](https://r-pkgs.org/release.html#release-deps) in the R Packages manual.