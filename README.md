# Atomic_Subroutines-Part_3a--How_To_Process_Array_Data
Fortran 2008 coarray programming with unordered execution segments (user-defined ordering) - Atomic Subroutines - Part 3a: How to process array data - allow for safe remote communication among a number of coarray images.

# Overview
This GitHub repository contains an example program that processes array data remotely through Fortran 2008 atomic subroutines (atomic_define, atomic_ref). The example program is only a minor modification and extension of part 3 that can be found here: https://github.com/MichaelSiehl/Atomic_Subroutines--Part_3--How_To_Process_Array_Data.<br />
<br />
The modified example program does apply a simple coarray programming technique which prevents that coarray values are getting overwritten by other coarray images (i.e. remote processes) and thus, allows for safe communication among a number of coarray images where multiple images do remotely write to the same single remote coarray image. The basics of the programming technique are described here: https://github.com/MichaelSiehl/Atomic_Subroutines--Using_Coarray_Arrays_to_Allow_for_Safe_Remote_Communication.

# How it works
The relevant codes are in the Main.f90 and OOOPimsc_admImageStatus_CA.f90 source code files.<br />
The example program should be compiled and run with 4 coarray images using OpenCoarrays/Gfortran.<br />
<br />
As with the example program from part 3, we fill a small integer array (vector) with five values (1,2,3,4,5) on coarray image 1 and then do transfer these values to coarray images 2, 3, and 4 (internally, calls to atomic_define are used for all the remote data transfers). Then (phase 2), new with the example program here, we do a reverse remote transfer of the array data from coarray images 2, 3, and 4 to coarray image 1 resp. (On the coarray images 2, 3, and 4, we do fill the array with values 2, 3, and 4 resp.)
See this output from a program run:<br />

```fortran
first phase: remote array transfer done: on image / array data           2           1           2           3           4           5
 first phase: remote array transfer done: on image / array data           3           1           2           3           4           5
 first phase: remote array transfer done: on image / array data           4           1           2           3           4           5
 second phase: reverse remote array transfer done: on image / array data           1           2           2           2           2           2
 second phase: reverse remote array transfer done: on image / array data           1           3           3           3           3           3
 second phase: reverse remote array transfer done: on image / array data           1           4           4           4           4           4
```
