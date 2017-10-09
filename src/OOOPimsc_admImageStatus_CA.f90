! https://github.com/MichaelSiehl/Atomic_Subroutines--Part_3a--How_To_Process_Array_Data

module OOOPimsc_admImageStatus_CA
!************************************************
! Namespace: OOOP - Parallel
!************************************************
! Abstact Data Type Short Name: OOOPimsc
!********************************************************
! Abstract Data Type (ADT):         OOOPimsc_adtImageStatus_CA
! Abstract Data Type Module (adm):  OOOPimsc_admImageStatus_CA.f90
!********************************************************
! Purpose:                    ImageStatus_CA-Object
! Language:                   mainly Fortran 95 with Fortran 2008 coarrays
! Programmer:                 Michael Siehl
! Date:                       September 2017
!********************************************************
! Naming Conventions:
!
!  for scalar members:
!                             m: object member
!                             S: setter, G: getter,
!                             S_atomic: the setter operates on atomic values using atomic_define and SYNC MEMORY
!                             G_check_atomic: the getter only checks local PGAS memory for specific values atomically
!
!  for array members:
!                             A: array
!                             mA: array member
!                             SA: set array, GA: get array,
!
!  for elements of array members:
!                             SAElement: set only one array element
!                             GAElement: get only one array element
!
!                             99: signals a static array member which has an upper array bound
!                                 larger than necessary; the upper bound is given by a global parameter
!
!  other naming conventions:
!                             _CA: coarray routine / coarray declaration
!                             SYNC_: synchronization routine
!
!                             Enum: enumeration
!
!                             OO: public (outer) scope (the two leading namespace letters)
!                             II: private (inner) scope
!                             UU: sub-object
!********************************************************
!___________________________________________________________

use OOOGglob_Globals
use OOOEerro_admError
use, intrinsic :: iso_fortran_env
!___________________________________________________________

implicit none
!___________________________________________________________

private
!__________________________________________________________
!
! service routines for enumeration handling:
public :: OOOPimsc_PackEnumValue, OOOPimsc_UnpackEnumValue
! *****
!
! access and synchronization routines for atomic scalar
! and atomic static array members:
public :: OOOPimscSAElement_atomic_intImageActivityFlag99_CA, & ! set array element
          OOOPimscGAElement_check_atomic_intImageActivityFlag99_CA, & ! get (check) array element
          OOOPimscSpinWaitBulkSync_atomic_intImageActivityFlag99_CA ! customized sychronization procedure
!
public :: OOOPimscSAElement_atomic_intTestArray_CA, &
          OOOPimscGAElement_check_atomic_intTestArray_CA, &
          OOOPimscSpinWaitArrayRang1Sync_atomic_intTestArray_CA ! customized synchronization procedure
                                                                ! for the TestArray component
! *****
!
! Encapsulate access to the SYNC MEMORY statement herein,
! counting/tracking of the execution segments on each image locally
! and access routines for the mA_atomic_intImageSyncMemoryCount99 member:
public :: OOOPimsc_subSyncMemory, OOOPimscSAElement_atomic_incr_intImageSyncMemoryCount99_CA, &
          OOOPimscGAElement_atomic_intImageSyncMemoryCount99_CA
!***

! logic codes for remote array data transfer:
! executed on image 1:
public :: OOOPimsc_SynchronizeAndDistributeTheTestArray_CA
! executed on images 2,3,4:
public :: OOOPimsc_InitiateAndWaitForTestArrayTransfer_CA

!***
! coarray ADT management:
public :: OOOPimsc_StructureConstructor_CA
!___________________________________________________________
!
!***************************
!****  Error Handling:  ****
!***************************
!***
! local ADT:
private :: IIimsc_ErrorHandler
!***
! coarray ADT:
private :: IIimsc_ImageNumberBoundError_CA
!___________________________________________________________
!
!************************
!****  Enumerations:  ***
!************************
!***  ImageActivityFlag:
type, private :: OOOPimsc_DontUse1
  integer(kind=OOOGglob_kint) :: Enum_StepWidth ! = 1000000
  integer(kind=OOOGglob_kint) :: InitialWaiting ! = 2000000
  integer(kind=OOOGglob_kint) :: TeamManager ! = 3000000
  integer(kind=OOOGglob_kint) :: TeamMember ! = 4000000
  integer(kind=OOOGglob_kint) :: ExecutionFinished ! = 5000000
  integer(kind=OOOGglob_kint) :: InitiateTestArrayTransfer ! = 12000000
  integer(kind=OOOGglob_kint) :: WaitForTestArrayTransfer ! = 13000000
  integer(kind=OOOGglob_kint) :: ResetTheTestArray ! = 14000000
  integer(kind=OOOGglob_kint) :: LocalTestArrayResetDone ! = 15000000
  integer(kind=OOOGglob_kint) :: TestArrayRemoteTransferDone ! = 16000000
  integer(kind=OOOGglob_kint) :: Enum_MaxValue ! = 17000000
end type OOOPimsc_DontUse1
!
type (OOOPimsc_DontUse1), public, parameter :: OOOPimscEnum_ImageActivityFlag &
     = OOOPimsc_DontUse1 (1000000,2000000,3000000,4000000,5000000, &
                           12000000, 13000000, 14000000, 15000000, 16000000, 17000000)
!**************************
!***  ArrayElementSyncStat:
type, private :: OOOPimsc_DontUse2
  integer(kind=OOOGglob_kint) :: Enum_StepWidth ! = 1000000
  integer(kind=OOOGglob_kint) :: ArrayElementNotSynchronizedYet ! = 2000000
  integer(kind=OOOGglob_kint) :: ArrayElementSynchronized ! = 3000000
  integer(kind=OOOGglob_kint) :: Enum_MaxValue ! = 4000000
end type OOOPimsc_DontUse2
!
type (OOOPimsc_DontUse2), public, parameter :: OOOPimscEnum_ArrayElementSyncStat &
     = OOOPimsc_DontUse2 (1000000,2000000,3000000, 4000000)
!___________________________________________________________
!
!***************************************
!***  Type Definition: *****************
!***************************************
type, public :: OOOPimsc_adtImageStatus_CA
  private
  !*****
  integer(atomic_int_kind), dimension (1:OOOGglob_NumImages_UpperBound, 1:2) :: &
                  mA_atomic_intImageActivityFlag99 = OOOPimscEnum_ImageActivityFlag % InitialWaiting
  !*****
  integer(atomic_int_kind), dimension (1:OOOGglob_NumImages_UpperBound) :: mA_atomic_intImageSyncMemoryCount99 = 0
  !*****
!171006:
  integer(atomic_int_kind), dimension (1:OOOGglob_NumImages_UpperBound, 1:5) :: mA_atomic_intTestArray
  !*****
  type (OOOEerroc_colError) :: m_UUerrocError ! error collection
  !
end type OOOPimsc_adtImageStatus_CA
!___________________________________________________________
!
!****************************************************
!***  Corresponding Coarray Declaration:  ***********
!****************************************************
!***
type (OOOPimsc_adtImageStatus_CA), public, codimension[*], save :: OOOPimscImageStatus_CA_1
!___________________________________________________________





contains


!##################################################################################################
!##################################################################################################
!##################################################################################################


!*******************************
! access routines for scalar   *
! and static array members:    *
!*******************************
!____________________________________________________________
!
! **************************************************************
! Pack and unpack an enumeration value with an additional value.
! (Store two distinct values into a single integer to allow for
!  atomic synchronizations):
! **************************************************************
! **********
subroutine OOOPimsc_PackEnumValue (Object_CA, intEnumValue, intAdditionalValue, &
                                                intPackedEnumValue, intEnum_StepWidth)
  ! pack the both integer input arguments into a single integer scalar
  type (OOOPimsc_adtImageStatus_CA), codimension[*], intent (inout) :: Object_CA
  integer(OOOGglob_kint), intent (in) :: intEnumValue
  integer(OOOGglob_kint), intent (in) :: intAdditionalValue
  integer(OOOGglob_kint), intent (out) :: intPackedEnumValue
  integer(OOOGglob_kint), optional, intent(in) :: intEnum_StepWidth ! only for error checking
  integer(OOOGglob_kint) :: status
  !
                                                                call OOOGglob_subSetProcedures &
                                                              ("OOOPimsc_PackEnumValue")
  !
  if (present(intEnum_StepWidth)) then ! do error checking
                                                                ! check if intAdditionalValue argument is to large:
                                                                ! ToDo: check if it is negative
                                                                if (intAdditionalValue >= intEnum_StepWidth) then
                                                                  call IIimsc_ErrorHandler (Object_CA, &
                                                                       "intAdditionalValue is to large", &
                                                                    OOOGglob_error, status)
                                                                  call OOOGglob_subResetProcedures
                                                                  return
                                                                end if
                                                                !
  end if
  !
  intPackedEnumValue = intEnumValue + intAdditionalValue
  !
                                                                call OOOGglob_subResetProcedures
end subroutine OOOPimsc_PackEnumValue
!
!**********
subroutine OOOPimsc_UnpackEnumValue (intPackedEnumValue, intEnum_StepWidth, &
                             intUnpackedEnumValue, intUnpackedAdditionalValue)
  ! unpack the integer enum value into two integer scalars
  integer(OOOGglob_kint), intent (in) :: intPackedEnumValue
  integer(OOOGglob_kint), intent (in) :: intEnum_StepWidth
  integer(OOOGglob_kint), intent (out) :: intUnpackedEnumValue
  integer(OOOGglob_kint), intent (out) :: intUnpackedAdditionalValue
  !
                                                                call OOOGglob_subSetProcedures &
                                                                   ("OOOPimsc_UnpackEnumValue")
  !
  intUnpackedAdditionalValue = mod(intPackedEnumValue, intEnum_StepWidth)
  !
  intUnpackedEnumValue = intPackedEnumValue - intUnpackedAdditionalValue
  !
                                                                call OOOGglob_subResetProcedures
end subroutine OOOPimsc_UnpackEnumValue
!
!**********
!____________________________________________________________
!
! *****************************************************
! access routines and synchronization routine for the
! mA_atomic_intImageActivityFlag99 member:
! *****************************************************
!
subroutine OOOPimscSAElement_atomic_intImageActivityFlag99_CA (Object_CA, intImageActivityFlag, &
                                intImageNumber, intArrayIndex, logExecuteSyncMemory)
  ! Set an Array Element atomically
  type (OOOPimsc_adtImageStatus_CA), codimension[*], intent (inout) :: Object_CA
  integer(OOOGglob_kint), intent (in) :: intImageActivityFlag
  integer(OOOGglob_kint), intent (in) :: intImageNumber ! the (remote) image number
  integer(OOOGglob_kint), optional, intent (in) :: intArrayIndex
  integer(OOOGglob_kint) :: intArrIndex
  integer(OOOGglob_kint) :: intMaxVal
  logical(OOOGglob_klog), optional, intent (in) :: logExecuteSyncMemory
  logical(OOOGglob_klog) :: logSyncMemoryExecution
  integer(OOOGglob_kint) :: status = 0 ! error status
                                                                call OOOGglob_subSetProcedures &
                                                                 ("OOOPimscSAElement_atomic_intImageActivityFlag99_CA")
  !
  if (present(intArrayIndex)) then
                                                                ! check if intArrayIndex argument is to large:
                                                                ! ToDo: check if it is negative
                                                                intMaxVal = OOOGglob_NumImages_UpperBound
                                                                if (intArrayIndex .gt. intMaxVal) then
                                                                  call IIimsc_ErrorHandler (Object_CA, &
                                                                       "intArrayIndex is to large", &
                                                                    OOOGglob_error, status)
                                                                  call OOOGglob_subResetProcedures
                                                                  return
                                                                end if
                                                                !
    intArrIndex = intArrayIndex
  else ! default:
    intArrIndex = intImageNumber
  end if
  !
  !
  if (present(logExecuteSyncMemory)) then
    logSyncMemoryExecution = logExecuteSyncMemory
  else ! default:
    logSyncMemoryExecution = .true.
  end if
  !
  !
  if (intImageNumber == this_image()) then ! local atomic define
    ! don't execute sync memory for local atomic_define:
    call atomic_define(Object_CA % mA_atomic_intImageActivityFlag99(intArrIndex,1), intImageActivityFlag)
  !
  else ! remote atomic define
                                                                ! check if the image number is valid:
                                                                if (IIimsc_ImageNumberBoundError_CA &
                                                                (Object_CA, intImageNumber)) then
                                                                  call IIimsc_ErrorHandler (Object_CA, &
                                                                       "no valid image number", &
                                                                    OOOGglob_error, status)
                                                                  call OOOGglob_subResetProcedures
                                                                  return
                                                                end if
                                                                !
    ! execute sync memory for remote atomic_define:
    if (logSyncMemoryExecution) call OOOPimsc_subSyncMemory (Object_CA) ! execute sync memory

! the following generates 'error #8583: COARRAY argument of ATOMIC_DEFINE/ATOMIC_REF intrinsic subroutine shall be a coarray.'
! with ifort 18 beta:
    call atomic_define(Object_CA [intImageNumber] % mA_atomic_intImageActivityFlag99(intArrIndex,1), intImageActivityFlag)
! the following does not generate an error with ifort 18 beta:
! (thus, we may expect that upcomming versions of ifort will support this too)
!    call atomic_define(Object_CA % mA_atomic_intImageActivityFlag99(intArrIndex,1), intImageActivityFlag)
    !
  end if
  !
                                                                call OOOGglob_subResetProcedures
end subroutine OOOPimscSAElement_atomic_intImageActivityFlag99_CA
!
!**********
!
logical(OOOGglob_klog) function OOOPimscGAElement_check_atomic_intImageActivityFlag99_CA (Object_CA, intCheckImageActivityFlag, &
                                                          intArrayIndex, intAdditionalAtomicValue, logExecuteSyncMemory)
  ! Get (check) an Array Element atomically:
  ! in order to hide the sync memory statement herein, this Getter does not allow
  ! to access the member directly, but instead does only allow to check the atomic member
  ! for specific values (this Getter is intented for synchronizations)
  type (OOOPimsc_adtImageStatus_CA), codimension[*], intent (inout) :: Object_CA
  integer(OOOGglob_kint), intent (in) :: intCheckImageActivityFlag
  integer(OOOGglob_kint), optional, intent (in) :: intArrayIndex
  integer(OOOGglob_kint) :: intArrIndex
  integer(OOOGglob_kint) :: intMaxVal
  integer(OOOGglob_kint), optional, intent (out) :: intAdditionalAtomicValue
  integer(OOOGglob_kint) :: intImageActivityFlag
  integer(OOOGglob_kint) :: intPackedEnumValue
  integer(OOOGglob_kint) :: intEnum_StepWidth
  integer(OOOGglob_kint) :: intUnpackedEnumValue
  integer(OOOGglob_kint) :: intUnpackedAdditionalValue
  logical(OOOGglob_klog), optional, intent (in) :: logExecuteSyncMemory
  logical(OOOGglob_klog) :: logSyncMemoryExecution
  integer(OOOGglob_kint) :: status = 0 ! error status
                                                                call OOOGglob_subSetProcedures &
                                                                 ("OOOPimscGAElement_check_atomic_intImageActivityFlag99_CA")
  !
  OOOPimscGAElement_check_atomic_intImageActivityFlag99_CA = .false.
  !
  if (present(intArrayIndex)) then
                                                                ! check if intArrayIndex argument is to large:
                                                                ! ToDo: check if it is negative
                                                                intMaxVal = OOOGglob_NumImages_UpperBound
                                                                if (intArrayIndex .gt. intMaxVal) then
                                                                  call IIimsc_ErrorHandler (Object_CA, &
                                                                       "intArrayIndex is to large", &
                                                                    OOOGglob_error, status)
                                                                  call OOOGglob_subResetProcedures
                                                                  return
                                                                end if
                                                                !
    intArrIndex = intArrayIndex
  else ! default:
    intArrIndex = this_image()
  end if
  !
  if (present(logExecuteSyncMemory)) then
    logSyncMemoryExecution = logExecuteSyncMemory
  else ! default:
    logSyncMemoryExecution = .true.
  end if
  !
  ! access an array element in local PGAS memory atomically:
  call atomic_ref(intImageActivityFlag, Object_CA % mA_atomic_intImageActivityFlag99(intArrIndex,1))
  ! unpack the intImageActivityFlag value:
  intPackedEnumValue = intImageActivityFlag
  intEnum_StepWidth = OOOPimscEnum_ImageActivityFlag % Enum_StepWidth
  call OOOPimsc_UnpackEnumValue (intPackedEnumValue, intEnum_StepWidth, &
                             intUnpackedEnumValue, intUnpackedAdditionalValue)
  intImageActivityFlag = intUnpackedEnumValue
  !
  if (intCheckImageActivityFlag == intImageActivityFlag) then
    if (logSyncMemoryExecution) call OOOPimsc_subSyncMemory (Object_CA) ! execute sync memory
    OOOPimscGAElement_check_atomic_intImageActivityFlag99_CA = .true.
  end if
  !
  if (present(intAdditionalAtomicValue)) then
    intAdditionalAtomicValue = intUnpackedAdditionalValue
  end if
  !
                                                                call OOOGglob_subResetProcedures
end function OOOPimscGAElement_check_atomic_intImageActivityFlag99_CA
!
!**********
!
subroutine OOOPimscSpinWaitBulkSync_atomic_intImageActivityFlag99_CA (Object_CA, intCheckImageActivityFlag, &
                  intNumberOfImages, intA_RemoteImageNumbers, logArrayIndexIsThisImage, &
                  intA_RemoteImageAndItsAdditionalAtomicValue, logExecuteSyncMemory)
  ! This routine is for atomic bulk synchronization (among the executing image and one or more remote images)
  ! using a spin-wait loop synchronizaton. Thus, the procedure implements a customized synchronization
  ! routine using atomic subroutines and the sync memory statement. Ordered execution segments among the involved images
  ! are not required.
  type (OOOPimsc_adtImageStatus_CA), codimension[*], intent (inout) :: Object_CA
  integer(OOOGglob_kint), intent (in) :: intCheckImageActivityFlag
  integer(OOOGglob_kint), intent (in) :: intNumberOfImages ! these are the number of involved remote images
  integer(OOOGglob_kint), dimension (1:intNumberOfImages), intent (in) :: intA_RemoteImageNumbers
  logical(OOOGglob_klog), optional, intent (in) :: logArrayIndexIsThisImage
  logical(OOOGglob_klog) :: logArrIndexIsThisImage
  integer(OOOGglob_kint) :: intArrIndex
  integer(OOOGglob_kint), optional, dimension (1:intNumberOfImages, 1:2), intent (out) :: &
                                                       intA_RemoteImageAndItsAdditionalAtomicValue
  integer(OOOGglob_kint) :: intCount
  integer(OOOGglob_kint) :: intImageNumber
  logical(OOOGglob_klog), dimension (1:intNumberOfImages) :: logA_CheckImageStates
  integer(OOOGglob_kint) :: intAtomicValue = 0
  logical(OOOGglob_klog), optional, intent (in) :: logExecuteSyncMemory
  logical(OOOGglob_klog) :: logSyncMemoryExecution
  integer(OOOGglob_kint) :: intMaxVal
  integer(OOOGglob_kint) :: status = 0 ! error status
  !
                                                                call OOOGglob_subSetProcedures &
                                            ("OOOPimscSpinWaitBulkSync_atomic_intImageActivityFlag99_CA")
  !
  !**********************************************************************
  !****
  if (present(logArrayIndexIsThisImage)) then
    logArrIndexIsThisImage = logArrayIndexIsThisImage
  else ! default:
    logArrIndexIsThisImage = .false.
  end if
  !****
  if (present(logExecuteSyncMemory)) then
    logSyncMemoryExecution = logExecuteSyncMemory
  else ! default:
    logSyncMemoryExecution = .true.
  end if
  !****
  ! initialize the array elements with .false.:
  logA_CheckImageStates = .false.
  !
  !**********************************************************************
  ! wait until all the involved remote image(s) do signal that they are in state intCheckImageActivityFlag
  ! spin-wait loop synchronization:
  do
    do intCount = 1, intNumberOfImages
      !
      intImageNumber = intA_RemoteImageNumbers(intCount)
      intArrIndex = intImageNumber ! but:
        if (logArrIndexIsThisImage) intArrIndex = this_image()
      if (intImageNumber .ne. this_image()) then ! (synchronization is only required between distinct images)
        if (.not. logA_CheckImageStates(intCount)) then ! check is only required if the remote image is not already
                                                        ! in state intCheckImageActivityFlag:
          !
          if (OOOPimscGAElement_check_atomic_intImageActivityFlag99_CA (OOOPimscImageStatus_CA_1, &
                           intCheckImageActivityFlag, intArrayIndex = intArrIndex, &
                           intAdditionalAtomicValue = intAtomicValue, logExecuteSyncMemory = .false.)) then
            logA_CheckImageStates(intCount) = .true. ! the remote image is in state intCheckImageActivityFlag
            !
            if (present(intA_RemoteImageAndItsAdditionalAtomicValue)) then
            ! save the remote image number together with its sent AdditionalAtomicValue:
              intA_RemoteImageAndItsAdditionalAtomicValue(intCount,1) = intImageNumber
              intA_RemoteImageAndItsAdditionalAtomicValue(intCount,2) = intAtomicValue
            end if
          end if
        end if
      else ! (intImageNumber .eq. this_image())
        ! raise an error:
                                                                call IIimsc_ErrorHandler (Object_CA, &
                                                            "the executing image can't synchronize with itself yet", &
                                                                  OOOGglob_error, status)
                                                                !
        logA_CheckImageStates(intCount) = .true. ! otherwise the outer do loop would turn into an endless loop
      end if
    end do
    !
    if (all(logA_CheckImageStates)) then ! all involved remote images are in state intCheckImageActivityFlag
      if (logSyncMemoryExecution) call OOOPimsc_subSyncMemory (Object_CA) ! execute sync memory
      exit ! exit the do loop if all involved remote images are in state
                                         ! intCheckImageActivityFlag
    end if
  end do
  !
  !**********************************************************************
  !
                                                                call OOOGglob_subResetProcedures
  !
end subroutine OOOPimscSpinWaitBulkSync_atomic_intImageActivityFlag99_CA
!
!____________________________________________________________
!
! *****************************************************
! access routines and synchronization routine for the
! mA_atomic_intTestArray member:
! *****************************************************
!
subroutine OOOPimscSAElement_atomic_intTestArray_CA (Object_CA, intArrayElementSyncStat, &
                                intImageNumber, intArrayIndex, logExecuteSyncMemory)
  ! Set an Array Element atomically
  type (OOOPimsc_adtImageStatus_CA), codimension[*], intent (inout) :: Object_CA
  integer(OOOGglob_kint), intent (in) :: intArrayElementSyncStat
  integer(OOOGglob_kint), intent (in) :: intImageNumber ! the (remote) image number
  integer(OOOGglob_kint), intent (in) :: intArrayIndex
  integer(OOOGglob_kint), dimension(1) :: intA_MaxVal
  logical(OOOGglob_klog), optional, intent (in) :: logExecuteSyncMemory
  logical(OOOGglob_klog) :: logSyncMemoryExecution
  integer(OOOGglob_kint) :: status = 0 ! error status
                                                                call OOOGglob_subSetProcedures &
                                                                 ("OOOPimscSAElement_atomic_intTestArray_CA")
  !
                                                                ! check if intArrayIndex argument is to large:
                                                                ! ToDo: check if it is negative
                                                                intA_MaxVal(1) = ubound(Object_CA % &
                                                                              mA_atomic_intTestArray,1)
                                                                if (intArrayIndex .gt. intA_MaxVal(1)) then
                                                                  call IIimsc_ErrorHandler (Object_CA, &
                                                                       "intArrayIndex is to large", &
                                                                    OOOGglob_error, status)
                                                                  call OOOGglob_subResetProcedures
                                                                  return
                                                                end if
                                                                !
  !
  if (present(logExecuteSyncMemory)) then
    logSyncMemoryExecution = logExecuteSyncMemory
  else ! default:
    logSyncMemoryExecution = .true.
  end if
  !
  !
  if (intImageNumber == this_image()) then ! local atomic define
    ! don't execute sync memory for local atomic_define:
!171006:
    call atomic_define(Object_CA % mA_atomic_intTestArray(this_image(),intArrayIndex), intArrayElementSyncStat)
  !
  else ! remote atomic define
                                                                ! check if the image number is valid:
                                                                if (IIimsc_ImageNumberBoundError_CA &
                                                                (Object_CA, intImageNumber)) then
                                                                  call IIimsc_ErrorHandler (Object_CA, &
                                                                       "no valid image number", &
                                                                    OOOGglob_error, status)
                                                                  call OOOGglob_subResetProcedures
                                                                  return
                                                                end if
                                                                !
    ! execute sync memory for remote atomic_define:
    if (logSyncMemoryExecution) call OOOPimsc_subSyncMemory (Object_CA) ! execute sync memory

! the following generates 'error #8583: COARRAY argument of ATOMIC_DEFINE/ATOMIC_REF intrinsic subroutine shall be a coarray.'
! with ifort 18 beta:
!171006:
    call atomic_define(Object_CA [intImageNumber] % mA_atomic_intTestArray(this_image(),intArrayIndex), intArrayElementSyncStat)
! the following does not generate an error with ifort 18 beta:
! (thus, we may expect that upcomming versions of ifort will support this too)
!    call atomic_define(Object_CA % mA_atomic_intTestArray(intArrayIndex), intArrayElementSyncStat)

    !
  end if
  !
                                                                call OOOGglob_subResetProcedures
end subroutine OOOPimscSAElement_atomic_intTestArray_CA
!
!**********
logical(OOOGglob_klog) function OOOPimscGAElement_check_atomic_intTestArray_CA (Object_CA, intCheckArrayElementSyncStat, &
                                        intArrayIndex, intRemoteImageNumber, intAdditionalAtomicValue, logExecuteSyncMemory)
  ! Get (check) an Array Element atomically:
  ! in order to hide the sync memory statement herein, this Getter does not allow
  ! to access the member directly, but instead does only allow to check the atomic member
  ! for specific values (this Getter is intented for synchronizations)
  type (OOOPimsc_adtImageStatus_CA), codimension[*], intent (inout) :: Object_CA
  integer(OOOGglob_kint), intent (in) :: intCheckArrayElementSyncStat
  integer(OOOGglob_kint), intent (in) :: intArrayIndex
!171006:
  integer(OOOGglob_kint), intent (in) :: intRemoteImageNumber
  integer(OOOGglob_kint), dimension(1) :: intA_MaxVal
  integer(OOOGglob_kint), optional, intent (out) :: intAdditionalAtomicValue
  integer(OOOGglob_kint) :: intArrayElementSyncStat
  integer(OOOGglob_kint) :: intPackedEnumValue
  integer(OOOGglob_kint) :: intEnum_StepWidth
  integer(OOOGglob_kint) :: intUnpackedEnumValue
  integer(OOOGglob_kint) :: intUnpackedAdditionalValue
  logical(OOOGglob_klog), optional, intent (in) :: logExecuteSyncMemory
  logical(OOOGglob_klog) :: logSyncMemoryExecution
  integer(OOOGglob_kint) :: status = 0 ! error status
                                                                call OOOGglob_subSetProcedures &
                                                                 ("OOOPimscGAElement_check_atomic_intTestArray_CA")
  !
  OOOPimscGAElement_check_atomic_intTestArray_CA = .false.
  !
                                                                ! check if intArrayIndex argument is to large:
                                                                ! ToDo: check if it is negative
                                                                intA_MaxVal(1) = ubound(Object_CA % &
                                                                              mA_atomic_intTestArray,1)
                                                                if (intArrayIndex .gt. intA_MaxVal(1)) then
                                                                  call IIimsc_ErrorHandler (Object_CA, &
                                                                       "intArrayIndex is to large", &
                                                                    OOOGglob_error, status)
                                                                  call OOOGglob_subResetProcedures
                                                                  return
                                                                end if
                                                                !
  !
  if (present(logExecuteSyncMemory)) then
    logSyncMemoryExecution = logExecuteSyncMemory
  else ! default:
    logSyncMemoryExecution = .true.
  end if
  !
  ! access an array element in local PGAS memory atomically:
!171006:
  call atomic_ref(intArrayElementSyncStat, Object_CA % mA_atomic_intTestArray(intRemoteImageNumber,intArrayIndex))
  ! unpack the intArrayElementSyncStat value:
  intPackedEnumValue = intArrayElementSyncStat
  intEnum_StepWidth = OOOPimscEnum_ArrayElementSyncStat % Enum_StepWidth
  call OOOPimsc_UnpackEnumValue (intPackedEnumValue, intEnum_StepWidth, &
                             intUnpackedEnumValue, intUnpackedAdditionalValue)
  intArrayElementSyncStat = intUnpackedEnumValue
  !
  if (intCheckArrayElementSyncStat == intArrayElementSyncStat) then
    if (logSyncMemoryExecution) call OOOPimsc_subSyncMemory (Object_CA) ! execute sync memory
    OOOPimscGAElement_check_atomic_intTestArray_CA = .true.
  end if
  !
  if (present(intAdditionalAtomicValue)) then
    intAdditionalAtomicValue = intUnpackedAdditionalValue
  end if
  !
                                                                call OOOGglob_subResetProcedures
end function OOOPimscGAElement_check_atomic_intTestArray_CA
!
!**********
subroutine OOOPimscSpinWaitArrayRang1Sync_atomic_intTestArray_CA (Object_CA, intCheckArrayElementSyncStat, &
                  intRemoteImageNumber, intArrayUpperBound, intA_ArrayElementSyncStatAndItsAdditionalAtomicValue, &
                  logExecuteSyncMemory)
  ! This routine is for atomic array synchronization (among the executing image and a single remote image)
  ! using a spin-wait loop synchronizaton. The routine synchronizes the array elements each resp.
  ! Thus, the procedure implements a customized synchronization routine using atomic subroutines and
  ! the sync memory statement. Ordered execution segments between the involved images
  ! are not required.
  type (OOOPimsc_adtImageStatus_CA), codimension[*], intent (inout) :: Object_CA
  integer(OOOGglob_kint), intent (in) :: intCheckArrayElementSyncStat
  integer(OOOGglob_kint), intent (in) :: intRemoteImageNumber
  integer(OOOGglob_kint) :: intArrayIndex
  integer(OOOGglob_kint), intent (in) :: intArrayUpperBound
  integer(OOOGglob_kint), dimension(1) :: intA_MaxVal
  integer(OOOGglob_kint), optional, dimension (1:intArrayUpperBound, 1:2), intent (out) :: &
                                                    intA_ArrayElementSyncStatAndItsAdditionalAtomicValue


  integer(OOOGglob_kint) :: intCount
  logical(OOOGglob_klog), dimension (1:intArrayUpperBound) :: logA_CheckArrayElementStates
  integer(OOOGglob_kint) :: intAtomicValue = 0
  logical(OOOGglob_klog), optional, intent (in) :: logExecuteSyncMemory
  logical(OOOGglob_klog) :: logSyncMemoryExecution
  integer(OOOGglob_kint) :: status = 0 ! error status
  !
                                                                call OOOGglob_subSetProcedures &
                                            ("OOOPimscSpinWaitArrayRang1Sync_atomic_intTestArray_CA")
                                                                !
                                                                ! check if the intArrayUpperBound argument is to large:
                                                                ! ToDo: check if it is negative
                                                                intA_MaxVal(1) = ubound(Object_CA % &
                                                                              mA_atomic_intTestArray,1)
                                                                if (intArrayUpperBound .gt. intA_MaxVal(1)) then
                                                                  call IIimsc_ErrorHandler (Object_CA, &
                                                                       "intArrayUpperBound is to large", &
                                                                    OOOGglob_error, status)
                                                                  call OOOGglob_subResetProcedures
                                                                  return
                                                                end if
                                                                !
  !
  !**********************************************************************
  !****
  if (present(logExecuteSyncMemory)) then
    logSyncMemoryExecution = logExecuteSyncMemory
  else ! default:
    logSyncMemoryExecution = .true.
  end if
  !****
  ! initialize the array elements with .false.:
  logA_CheckArrayElementStates = .false.
  !
  !**********************************************************************
  ! wait until all the array elements are in state intCheckArrayElementSyncStat
  ! spin-wait loop synchronization:
  do
    do intCount = 1, intArrayUpperBound
      intArrayIndex = intCount
      if (intRemoteImageNumber .ne. this_image()) then ! (synchronization is only required between distinct images)
        if (.not. logA_CheckArrayElementStates(intCount)) then ! check is only required if the array element is not already
                                                               ! in state intCheckArrayElementSyncStat:
          !
!171006:
          if (OOOPimscGAElement_check_atomic_intTestArray_CA (OOOPimscImageStatus_CA_1, &
                           intCheckArrayElementSyncStat, intArrayIndex, intRemoteImageNumber, &
                           intAdditionalAtomicValue = intAtomicValue, logExecuteSyncMemory = .false.)) then
            logA_CheckArrayElementStates(intCount) = .true. ! the array element is in state intCheckArrayElementSyncStat
            !
            if (present(intA_ArrayElementSyncStatAndItsAdditionalAtomicValue)) then
            ! save the array element synchronization status together with its sent AdditionalAtomicValue:
              intA_ArrayElementSyncStatAndItsAdditionalAtomicValue(intCount,1) = intCheckArrayElementSyncStat
              intA_ArrayElementSyncStatAndItsAdditionalAtomicValue(intCount,2) = intAtomicValue
            end if
          end if
        end if
      else ! (intRemoteImageNumber .eq. this_image())
      ! (be aware: this is to already prepare this routine for array synchronizations
      !  with several remote images involved)
        ! raise an error:
                                                                call IIimsc_ErrorHandler (Object_CA, &
                                                            "the executing image can't synchronize with itself yet", &
                                                                  OOOGglob_error, status)
                                                                !
        logA_CheckArrayElementStates(intCount) = .true. ! otherwise the outer do loop would turn into an endless loop
      end if
    end do
    !
    if (all(logA_CheckArrayElementStates)) then ! all array elements are in state intCheckArrayElementSyncStat
      if (logSyncMemoryExecution) call OOOPimsc_subSyncMemory (Object_CA) ! execute sync memory
      exit ! exit the do loop if all array elements are in state
                                         ! intCheckArrayElementSyncStat
    end if
  end do
  !
  !**********************************************************************
  !
                                                                call OOOGglob_subResetProcedures
  !
end subroutine OOOPimscSpinWaitArrayRang1Sync_atomic_intTestArray_CA
!
!__________________________________________________________
!
! *************************************************************************
! Encapsulate access to the SYNC MEMORY statement herein,
! counting/tracking of the execution segments on each image locally,
! and access routines for the mA_atomic_intImageSyncMemoryCount99 member:
! *************************************************************************
!
!**********
subroutine OOOPimsc_subSyncMemory (Object_CA)
  type (OOOPimsc_adtImageStatus_CA), codimension[*], intent (inout) :: Object_CA
  ! encapsulates access to SYNC MEMORY
                                                                call OOOGglob_subSetProcedures &
                                                                 ("OOOPimsc_subSyncMemory")
  sync memory
  ! increment the ImageSyncMemoryCount to track the execution segment order
  ! on the executing image:
  call OOOPimscSAElement_atomic_incr_intImageSyncMemoryCount99_CA (Object_CA)
  !
                                                                call OOOGglob_subResetProcedures
end subroutine OOOPimsc_subSyncMemory
!
!**********
! private:
subroutine OOOPimscSAElement_atomic_incr_intImageSyncMemoryCount99_CA (Object_CA)
  ! increment (by 1) the ImageSyncMemoryCount member atomically on the executing image
  type (OOOPimsc_adtImageStatus_CA), codimension[*], intent (inout) :: Object_CA
  integer(OOOGglob_kint) :: intTemp
  integer(OOOGglob_kint) :: status = 0 ! error status
!integer(OOOGglob_kint) :: test
  !
                                                                call OOOGglob_subSetProcedures &
                                                            ("OOOPimscSAElement_atomic_incr_intImageSyncMemoryCount99_CA")
  !
  ! increment (by 1) the ImageSyncMemoryCount member atomically on the executing image only:
  ! every image uses its own array index (this_image())
  !
  ! Fortran 2015 syntax:
  !call atomic_add(Object_CA % mA_atomic_intImageSyncMemoryCount99(this_image()), 1)
  !
  ! Fortran 2008 syntax:
  call atomic_ref(intTemp, Object_CA % mA_atomic_intImageSyncMemoryCount99 (this_image()))
  intTemp = intTemp + 1
  ! don't execute sync memory for local atomic_define:
  call atomic_define(Object_CA % mA_atomic_intImageSyncMemoryCount99 (this_image()), intTemp)
!
! test:
!call atomic_ref(test, Object_CA % mA_atomic_intImageSyncMemoryCount99 (this_image()))
!write(*,*) 'entering execution segment', test, 'on image', this_image()
  !
                                                                call OOOGglob_subResetProcedures
end subroutine OOOPimscSAElement_atomic_incr_intImageSyncMemoryCount99_CA
!**********
! private:
subroutine OOOPimscGAElement_atomic_intImageSyncMemoryCount99_CA (Object_CA, intSyncMemoryCount, intArrayIndex)
  ! get only one array element on the executing image
  type (OOOPimsc_adtImageStatus_CA), codimension[*], intent (inout) :: Object_CA
  integer(OOOGglob_kint), intent (out) :: intSyncMemoryCount
  integer(OOOGglob_kint), optional, intent (in) :: intArrayIndex
  integer(OOOGglob_kint) :: intArrIndex
  integer(OOOGglob_kint) :: intMaxVal
  integer(OOOGglob_kint) :: status = 0 ! error status
  !
                                                                call OOOGglob_subSetProcedures &
                                                                  ("OOOPimscGAElement_atomic_intImageSyncMemoryCount99_CA")
  if (present(intArrayIndex)) then
                                                                ! check if intArrayIndex argument is to large:
                                                                ! ToDo: check if it is negative
                                                                intMaxVal = OOOGglob_NumImages_UpperBound
                                                                if (intArrayIndex .gt. intMaxVal) then
                                                                  call IIimsc_ErrorHandler (Object_CA, &
                                                                       "intArrayIndex is to large", &
                                                                    OOOGglob_error, status)
                                                                  call OOOGglob_subResetProcedures
                                                                  return
                                                                end if
                                                                !
    intArrIndex = intArrayIndex
  else ! default:
    intArrIndex = this_image() ! ToDo: this subroutine should only be used with this default
  end if
  ! get the array element:
  call atomic_ref(intSyncMemoryCount, Object_CA % mA_atomic_intImageSyncMemoryCount99 (intArrIndex))
  ! no SYNC MEMORY statement here, because this call to atomic_ref is not used for synchronization and
  ! thus, this is not an atomic checker routine
  !
                                                                call OOOGglob_subResetProcedures
end subroutine OOOPimscGAElement_atomic_intImageSyncMemoryCount99_CA
!*************
!___________________________________________________________
!
!##########################################################################################
! The parallel logic codes:  ##############################################################
!##########################################################################################
!
! executed on image 1:
!___________________________________________________________
!
! public
subroutine OOOPimsc_SynchronizeAndDistributeTheTestArray_CA (Object_CA, intNumberOfImages, intA_RemoteImageNumbers, &
                                                             intA_TestArrayForRemoteTransfer)
  ! This routine is to synchronize and distribute the TestArray (using atomic subroutines)
  ! to the involved remote images (2,3,4).
  ! To do so, this routine gets executed on a separate coarray image
  ! (on image 1 with this example)
  !
  type (OOOPimsc_adtImageStatus_CA), codimension[*], intent (inout) :: Object_CA
  integer(OOOGglob_kint), intent (in) :: intNumberOfImages ! these are the number of involved remote images
  integer(OOOGglob_kint), dimension (intNumberOfImages), intent (in) :: intA_RemoteImageNumbers
  integer(OOOGglob_kint), dimension (1:5), intent (in) :: intA_TestArrayForRemoteTransfer
  integer(OOOGglob_kint) :: intCount
  integer(OOOGglob_kint) :: intCount2
  integer(OOOGglob_kint) :: intImageNumber
  integer(OOOGglob_kint) :: intImageActivityFlag
  integer(OOOGglob_kint) :: intPackedEnumValue
  integer(OOOGglob_kint) :: intArrayElementSyncStat
  integer(OOOGglob_kint), dimension (1:5) :: intA_TestArray ! will contain the packed enum values
                                                            ! for remote transfer
  integer(OOOGglob_kint) :: intEnumStepWidth
  !
                                                                call OOOGglob_subSetProcedures &
                                                            ("OOOPimsc_SynchronizeAndDistributeTheTestArray_CA")
  !
  !************************************************
  ! (1) wait until all the involved remote image(s) do signal that they are in state WaitForTestArrayTransfer:
  ! (counterpart routine is step 2 in OOOPimsc_InitiateAndWaitForTestArrayTransfer_CA)
  intImageActivityFlag = OOOPimscEnum_ImageActivityFlag % WaitForTestArrayTransfer
  ! spin-wait loop synchronization:
  call OOOPimscSpinWaitBulkSync_atomic_intImageActivityFlag99_CA (Object_CA, intImageActivityFlag, &
                         intNumberOfImages, intA_RemoteImageNumbers)
  !*************************************************
  ! (2) pack and fill the TestArray locally with values from intA_TestArrayForRemoteTransfer:
  ! - Firstly, set this to allow for later synchronization of the array elements on the remote images:
  intArrayElementSyncStat = OOOPimscEnum_ArrayElementSyncStat % ArrayElementSynchronized
  ! - pack the Enum value with input values from intA_TestArrayForRemoteTransfer and fill the
  !   intA_TestArray elements with these values:
  intEnumStepWidth = OOOPimscEnum_ArrayElementSyncStat % Enum_StepWidth ! only for error checking
  do intCount = 1, 5
    call OOOPimsc_PackEnumValue (Object_CA, intArrayElementSyncStat, &
                        intA_TestArrayForRemoteTransfer(intCount), intPackedEnumValue, intEnumStepWidth)
    intA_TestArray(intCount) = intPackedEnumValue
  end do
  !
  !**********************************************************************
  ! (3) distribute the TestArray to the TestArray coarray components of the involved remote image(s):
  ! (counterpart synchronization routine is step 3 in OOOPimsc_InitiateAndWaitForTestArrayTransfer_CA)
  call OOOPimsc_subSyncMemory (Object_CA) ! execute sync memory
  do intCount = 1, intNumberOfImages ! distribute the array to each involved remote image
     intImageNumber = intA_RemoteImageNumbers(intCount)
    do intCount2 = 1, ubound(intA_TestArray, 1) ! distribute the array elements each individually:
      call OOOPimscSAElement_atomic_intTestArray_CA (Object_CA, intA_TestArray(intCount2), &
                            intImageNumber, intArrayIndex = intCount2, logExecuteSyncMemory = .false.)
    end do
  end do
  !
  !************************************************
  ! (4) wait until all the involved remote image(s) do signal that they are in state TestArrayRemoteTransferDone:
  ! (counterpart routine is step 4 in OOOPimsc_InitiateAndWaitForTestArrayTransfer_CA)
  intImageActivityFlag = OOOPimscEnum_ImageActivityFlag % TestArrayRemoteTransferDone
  ! - spin-wait loop synchronization:
  call OOOPimscSpinWaitBulkSync_atomic_intImageActivityFlag99_CA (Object_CA, intImageActivityFlag, &
                         intNumberOfImages, intA_RemoteImageNumbers)
  !
  !**********************************************************************
  ! (5) finish execution on the executing image (not really required for this example):
!  call OOOPimscSAElement_atomic_intImageActivityFlag99_CA (OOOPimscImageStatus_CA_1, OOOPimscEnum_ImageActivityFlag % &
!                                    ExecutionFinished, this_image(), logExecuteSyncMemory = .false.)
  !

                                                                call OOOGglob_subResetProcedures
end subroutine OOOPimsc_SynchronizeAndDistributeTheTestArray_CA
!______________________________________________________________
!
! executed on images 2,3,4:
!______________________________________________________________
!
! public
subroutine OOOPimsc_InitiateAndWaitForTestArrayTransfer_CA (Object_CA, intSetFromImageNumber, intA_TestArrayForRemoteTransfer)
  ! This routine is to synchronize and receive the TestArray (using atomic subroutines indirectly)
  ! on the involved images (2,3,4):
  ! (the involved images (not image 1) will execute this rotuine)
  type (OOOPimsc_adtImageStatus_CA), codimension[*], intent (inout) :: Object_CA
  integer(OOOGglob_kint), intent (in) :: intSetFromImageNumber ! this is the remote image number (image 1)
                                                               ! which initiated the synchronization
  integer(OOOGglob_kint), dimension (1:5), intent (out) :: intA_TestArrayForRemoteTransfer
  integer(OOOGglob_kint) :: intImageActivityFlag
  integer(OOOGglob_kint) :: intArrayElementSyncStat
  integer(OOOGglob_kint) :: intArrayUpperBound
  integer(OOOGglob_kint), dimension (1:5, 1:2) :: intA_ArrayElementSyncStatAndItsAdditionalAtomicValue
  integer(OOOGglob_kint) :: intPackedEnumValue
  integer(OOOGglob_kint) :: intRemoteImageNumber
  integer(OOOGglob_kint) :: intCount
  integer(OOOGglob_kint) :: intEnumStepWidth
  !
                                                                call OOOGglob_subSetProcedures &
                                                            ("OOOPimsc_InitiateAndWaitForTestArrayTransfer_CA")
  !
  !**********************************************************************
  ! (1) reset the TestArray component locally on this image (to allow for
  ! synchronization of the array elements later on):
  intArrayElementSyncStat = OOOPimscEnum_ArrayElementSyncStat % ArrayElementNotSynchronizedYet
  ! - pack the Enum value together with the value 0 for the TestArray elements:
  intEnumStepWidth = OOOPimscEnum_ArrayElementSyncStat % Enum_StepWidth ! only for error checking
  call OOOPimsc_PackEnumValue (Object_CA, intArrayElementSyncStat, &
                                         0, intPackedEnumValue, intEnumStepWidth)
  ! - fill the local TestArray component atomicly:
  do intCount = 1, ubound(intA_TestArrayForRemoteTransfer, 1)
    call OOOPimscSAElement_atomic_intTestArray_CA (Object_CA, intPackedEnumValue, &
                          this_image(), intArrayIndex = intCount, logExecuteSyncMemory = .false.)
                        ! here, we do not execute sync memory for local atomic_define yet
  end do
  !
  ! *********************************************************************
  ! (2) set this image to state 'WaitForTestArrayTransfer' and signal this to the remote image 1:
  ! (conterpart synchronization routine is step 1 in OOOPimsc_SynchronizeAndDistributeTheTestArray_CA)
  intRemoteImageNumber = intSetFromImageNumber
  intImageActivityFlag = OOOPimscEnum_ImageActivityFlag % WaitForTestArrayTransfer
  !
  ! - pack the ImageActivityFlag together with this_image():
  intEnumStepWidth = OOOPimscEnum_ImageActivityFlag % Enum_StepWidth
  call OOOPimsc_PackEnumValue (Object_CA, intImageActivityFlag, this_image(), intPackedEnumValue, &
                                        intEnumStepWidth)
  !
  ! - signal to the remote image (image 1) that this image is now in state 'WaitForTestArrayTransfer':
  call OOOPimscSAElement_atomic_intImageActivityFlag99_CA (Object_CA, intPackedEnumValue, &
                         intRemoteImageNumber, intArrayIndex = this_image(), logExecuteSyncMemory = .true.)
  !
  !*************************************************************************
  ! (3) wait until all the array elements of the TestArray coarray component are in state ArrayElementSynchronized
  ! (counterpart routine is step 3 in OOOPimsc_SynchronizeAndDistributeTheTestArray_CA)
  intArrayElementSyncStat = OOOPimscEnum_ArrayElementSyncStat % ArrayElementSynchronized
  intArrayUpperBound = ubound(intA_ArrayElementSyncStatAndItsAdditionalAtomicValue, 1)
  ! - spin-wait loop synchronization for all the distinct array elements:
  intRemoteImageNumber = intSetFromImageNumber
  call OOOPimscSpinWaitArrayRang1Sync_atomic_intTestArray_CA (Object_CA, intArrayElementSyncStat, &
                  intRemoteImageNumber, intArrayUpperBound, &
                  intA_ArrayElementSyncStatAndItsAdditionalAtomicValue, &
                  logExecuteSyncMemory = .true.)
  !
  intA_TestArrayForRemoteTransfer(:) = intA_ArrayElementSyncStatAndItsAdditionalAtomicValue(:,2) ! procedure output argument
  !
  !*************************************************************************
  ! (4) signal to the remote image (image 1) that this image is now in state 'TestArrayRemoteTransferDone'
  ! (conterpart synchronization routine is step 4 in OOOPimsc_SynchronizeAndDistributeTheTestArray_CA)
  intImageActivityFlag = OOOPimscEnum_ImageActivityFlag % TestArrayRemoteTransferDone
  call OOOPimscSAElement_atomic_intImageActivityFlag99_CA (Object_CA, intImageActivityFlag, &
                         intRemoteImageNumber, intArrayIndex = this_image(), logExecuteSyncMemory = .true.)
  !*************************************************************************
  ! (5) finish execution on the executing image (not really required for this example):
!  call OOOPimscSAElement_atomic_intImageActivityFlag99_CA (OOOPimscImageStatus_CA_1, OOOPimscEnum_ImageActivityFlag % &
!                                    ExecutionFinished, this_image(), logExecuteSyncMemory = .false.)
  !*************************************************************************
  !
                                                                call OOOGglob_subResetProcedures
end subroutine OOOPimsc_InitiateAndWaitForTestArrayTransfer_CA
!__________________________________________________________
!


!##################################################################################################
!##################################################################################################
!##################################################################################################


!**************************
! coarray type management: *
!**************************
!___________________________________________________________
!
!
subroutine OOOPimsc_StructureConstructor_CA (Object_CA)
  type (OOOPimsc_adtImageStatus_CA), codimension[*], intent (inout) :: Object_CA
  !
                                                                call OOOGglob_subSetProcedures ("OOOPimsc_StructureConstructor_CA")

  !
                                                                call OOOGglob_subResetProcedures
end subroutine OOOPimsc_StructureConstructor_CA
!___________________________________________________________


!##################################################################################################
!##################################################################################################
!##################################################################################################

!**********************************************************
!*************  Error Handling:   *************************
!**********************************************************
!__________________________________________________________
!
!Private
subroutine IIimsc_ErrorHandler (Object, chrErrorDescription, &
  intErrorType, intErrorNumber)
  ! ErrorHandler for the ADT and CA routines
  type(OOOPimsc_adtImageStatus_CA), intent(inout) :: Object
  character(kind=1, len=*), intent(in) :: chrErrorDescription
  integer(OOOGglob_kint), intent(in) :: intErrorType ! 1=warning, 2=Severe System error
  integer(OOOGglob_kint), intent(in) :: intErrorNumber ! Run Time error Number (e.g. status)
  call OOOEerroc_AddObject (Object % m_UUerrocError, chrErrorDescription, &
    intErrorType)
end subroutine IIimsc_ErrorHandler
!__________________________________________________________
!
!Private
logical(OOOGglob_klog) function IIimsc_ImageNumberBoundError_CA (Object_CA, intImageNumber)
  ! error handling routine
  ! checks if the image number does exist
  type(OOOPimsc_adtImageStatus_CA), codimension[*], intent(inout) :: Object_CA
  integer(OOOGglob_kint), intent(in) :: intImageNumber
  !
  IIimsc_ImageNumberBoundError_CA = .false.
  !
  if (intImageNumber .lt. 1) then ! image number is too small
    IIimsc_ImageNumberBoundError_CA = .true.
  end if
  !
  if (intImageNumber .gt. num_images()) then ! image number is too large
    IIimsc_ImageNumberBoundError_CA = .true.
  end if
  !
end function IIimsc_ImageNumberBoundError_CA
!__________________________________________________________

end module OOOPimsc_admImageStatus_CA
