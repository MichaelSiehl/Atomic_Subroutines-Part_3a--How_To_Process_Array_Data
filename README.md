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

Below is the code of our modified main program:

```fortran
program Main
  use OOOGglob_Globals
  use OOOEerro_admError
  use OOOPimsc_admImageStatus_CA
  implicit none
  !
  integer(OOOGglob_kint) :: intNumberOfRemoteImages
  integer(OOOGglob_kint), dimension (1:3) :: intA_RemoteImageNumbers ! please compile and run the
                                                                     ! program with 4 coarray images
  integer(OOOGglob_kint) :: intSetFromImageNumber
  integer(OOOGglob_kint), dimension (1:5) :: intA_TestArrayForRemoteTransfer
  integer(OOOGglob_kint) :: intCount
  integer(OOOGglob_kint) :: intA
  !*********************************
  if (this_image() == 1) then
    !
    intNumberOfRemoteImages = 3
    intA_RemoteImageNumbers = (/2,3,4/)
    intA_TestArrayForRemoteTransfer = (/1,2,3,4,5/)
    !
    ! This routine call is to synchronize and distribute the TestArray (using atomic subroutines)
    ! to the involved remote images (2,3,4) (first phase):
    call OOOPimsc_SynchronizeAndDistributeTheTestArray_CA (OOOPimscImageStatus_CA_1, &
            intNumberOfRemoteImages, intA_RemoteImageNumbers, intA_TestArrayForRemoteTransfer)
    !
    ! initiate and wait for reverse remote transfer (second phase):
    do intCount = 1, intNumberOfRemoteImages
      intSetFromImageNumber = intA_RemoteImageNumbers(intCount)
      call OOOPimsc_InitiateAndWaitForTestArrayTransfer_CA (OOOPimscImageStatus_CA_1, intSetFromImageNumber, &
                                                               intA_TestArrayForRemoteTransfer)
      write(*,*) 'second phase: reverse remote array transfer done: on image / array data', &
                                  this_image(), intA_TestArrayForRemoteTransfer
    end do
    !
  !***********************************
  else ! this_image > 1
    intSetFromImageNumber = 1
    !
    ! This routine call is to synchronize and receive the TestArray (using atomic subroutines)
    ! on the involved images (2,3,4) (first phase):
    call OOOPimsc_InitiateAndWaitForTestArrayTransfer_CA (OOOPimscImageStatus_CA_1, intSetFromImageNumber, &
                                                             intA_TestArrayForRemoteTransfer)
    write(*,*) 'first phase: remote array transfer done: on image / array data', this_image(), intA_TestArrayForRemoteTransfer
    !
    ! synchronize and distribute the reverse remote transfer (second phase):
    intNumberOfRemoteImages = 1
    intA_RemoteImageNumbers(1) = 1
    intA = this_image()
    intA_TestArrayForRemoteTransfer = (/intA,intA,intA,intA,intA/)
    call OOOPimsc_SynchronizeAndDistributeTheTestArray_CA (OOOPimscImageStatus_CA_1, &
            intNumberOfRemoteImages, intA_RemoteImageNumbers, intA_TestArrayForRemoteTransfer)
    !
  !************************************
  end if
  !
end program Main
```

# - Code changes in the OOOPimsc_admImageStatus_CA module
Only minor changes where required to allow for safe reverse remote transfer from several coarray images (2,3,4) to the same single remote image (1) (in the source code these changes are marked with date stamp '171006'):<br />
<br />
1. To allow for safe remote array data transfer from multiple coarray images (2,3,4) to the same single remote coarray image (1), we do extend the array member (of the derived type coarray definition) by an additional (first) dimension:
```fortran
type, public :: OOOPimsc_adtImageStatus_CA
.
  integer(atomic_int_kind), dimension (1:OOOGglob_NumImages_UpperBound, 1:5) :: mA_atomic_intTestArray
.
end type OOOPimsc_adtImageStatus_CA
```
This newly added dimension (1:OOOGglob_NumImages_UpperBound) gives not only additional PGAS memory to hold the values from all the remote images (on image 1) but also additional distinct remote communication channels for safe transfer of the array data from the distinct coarray images (2,3,4) through atomic_define.<br />
<br />
2. The OOOPimscSAElement_atomic_intTestArray_CA access routine for the derived type coarray array component then uses 'this_image()' to access it's own unique remote communication channel with atomic_define:
```fortran
subroutine OOOPimscSAElement_atomic_intTestArray_CA (Object_CA, intArrayElementSyncStat, &
                                intImageNumber, intArrayIndex, logExecuteSyncMemory)
.
.
    call atomic_define(Object_CA % mA_atomic_intTestArray(this_image(),intArrayIndex), intArrayElementSyncStat)
.
    call atomic_define(Object_CA [intImageNumber] % mA_atomic_intTestArray(this_image(),intArrayIndex), intArrayElementSyncStat)
.
```
<br />
3. The procedure interface of the OOOPimscGAElement_check_atomic_intTestArray_CA accessor and it's call to atomic_ref require only a minor extension to allow for access to any of the distinct remote communication channels for the array data:<br />

```fortran
logical(OOOGglob_klog) function OOOPimscGAElement_check_atomic_intTestArray_CA (Object_CA, intCheckArrayElementSyncStat, &
                                        intArrayIndex, intRemoteImageNumber, intAdditionalAtomicValue, logExecuteSyncMemory)
.
integer(OOOGglob_kint), intent (in) :: intRemoteImageNumber
.
.
  call atomic_ref(intArrayElementSyncStat, Object_CA % mA_atomic_intTestArray(intRemoteImageNumber, intArrayIndex))
.
```
<br />
4. And finally, the customized synchronization procedure for the array data OOOPimscSpinWaitArrayRang1Sync_atomic_intTestArray_CA requires only a small adaption for it's call to the above accessor. The procedure interface of the synchronization routine remains unchanged:<br />

```fortran
subroutine OOOPimscSpinWaitArrayRang1Sync_atomic_intTestArray_CA (Object_CA, intCheckArrayElementSyncStat, &
                  intRemoteImageNumber, intArrayUpperBound, intA_ArrayElementSyncStatAndItsAdditionalAtomicValue, &
                  logExecuteSyncMemory)
.
.
          if (OOOPimscGAElement_check_atomic_intTestArray_CA (OOOPimscImageStatus_CA_1, &
                           intCheckArrayElementSyncStat, intArrayIndex, intRemoteImageNumber, &
                           intAdditionalAtomicValue = intAtomicValue, logExecuteSyncMemory = .false.))
```
