! https://github.com/MichaelSiehl/Atomic_Subroutines--Part_3-a--How_To_Process_Array_Data

!program Main
!  !
!  use Main_Sub
!  !
!  call Entry_Main_Sub ()
!  !
!end program Main


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
