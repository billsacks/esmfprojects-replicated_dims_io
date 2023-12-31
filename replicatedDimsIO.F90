module test_replicated_dims_io
  use ESMF
  implicit none
  private

  public :: io_with_replicated_dims

contains
  subroutine io_with_replicated_dims(decompDim1, decompDim2, fname, delayout, skipRead)
    integer, intent(in) :: decompDim1
    integer, intent(in) :: decompDim2
    character(len=*), intent(in) :: fname
    type(ESMF_DELayout), intent(in), optional :: delayout
    logical, intent(in), optional :: skipRead

    integer :: rc
    type(ESMF_DistGrid) :: distgrid
    type(ESMF_ArraySpec) :: arraySpec
    type(ESMF_Array) :: array, arrayRead
    real(ESMF_KIND_R8), pointer :: arrayData(:,:), arrayReadData(:,:)
    logical :: allEqual
    logical :: l_skipRead

    if (present(skipRead)) then
       l_skipRead = skipRead
    else
       l_skipRead = .false.
    end if

    ! Create a 4-d distgrid, with 8 DEs
    distgrid = ESMF_DistGridCreate( &
         minIndex = [1,11,21,31], &
         maxIndex = [8,18,26,37], &
         regDecomp = [decompDim1,decompDim2,1,1], &
         delayout = delayout, &
         rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, file=__FILE__)) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    ! Create an array spec for a 2-d array
    call ESMF_ArraySpecSet(arraySpec, typekind=ESMF_TYPEKIND_R8, rank=2, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, file=__FILE__)) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    ! Create a 2-d array, with 2 replicated dims
    array = ESMF_ArrayCreate(distgrid, arraySpec, name="arrayWithReplicatedDims", &
         ! Dimensions 3 and 4 of the DistGrid will be replicated dimensions
         distgridToArrayMap = [1,2,0,0], &
         rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, file=__FILE__)) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    arrayRead = ESMF_ArrayCreate(distgrid, arraySpec, name="arrayWithReplicatedDims", &
         ! Dimensions 3 and 4 of the DistGrid will be replicated dimensions
         distgridToArrayMap = [1,2,0,0], &
         rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, file=__FILE__)) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    ! Fill the array
    call ESMF_ArrayGet(array, farrayPtr=arrayData, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, file=__FILE__)) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    call fillArray(arrayData, 1)

    ! Write the array
    call ESMF_ArrayWrite(array, fileName=fname, overwrite=.true., rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, file=__FILE__)) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    if (.not. l_skipRead) then
       ! Read the array
       call ESMF_ArrayRead(arrayRead, fileName=fname, rc=rc)
       if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) call ESMF_Finalize(endflag=ESMF_END_ABORT)

       ! Confirm that read-in field matches original
       call ESMF_ArrayGet(arrayRead, farrayPtr=arrayReadData, rc=rc)
       if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) call ESMF_Finalize(endflag=ESMF_END_ABORT)
       allEqual = all(arrayReadData == arrayData)
       if (.not. allEqual) then
          if (ESMF_LogFoundError(ESMF_RC_OBJ_BAD, &
               msg="Read-in data differ from original", &
               line=__LINE__, file=__FILE__)) call ESMF_Finalize(endflag=ESMF_END_ABORT)
       end if
    end if

  end subroutine io_with_replicated_dims

  subroutine fillArray(array, multiplier)
    ! Fill the given 2-d array based on indices times a multiplier
    real(ESMF_KIND_R8), intent(out) :: array(:,:)
    integer, intent(in) :: multiplier

    type(ESMF_VM) :: vm
    integer :: localPet

    integer :: rc
    integer :: i, j

    call ESMF_VMGetGlobal(vm, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, file=__FILE__)) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    call ESMF_VMGet(vm, localPet=localPet, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, file=__FILE__)) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    do j = 1, size(array, 2)
       do i = 1, size(array, 1)
          array(i,j) = (localPet+1) * multiplier * ((i-1)*size(array,2) + (j-1))
       end do
    end do
  end subroutine fillArray

end module test_replicated_dims_io

program replicated_dims_io
  use ESMF
  use test_replicated_dims_io
  implicit none

  type(ESMF_DELayout) :: delayout
  integer :: rc

  call ESMF_Initialize(logkindflag=ESMF_LOGKIND_MULTI, defaultCalkind=ESMF_CALKIND_GREGORIAN, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
       line=__LINE__, file=__FILE__)) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  ! Simple case
  call io_with_replicated_dims( &
       decompDim1 = 2, &
       decompDim2 = 4, &
       fname = 'test.nc')

  ! 2 DEs per PET
  delayout = ESMF_DELayoutCreate(petMap=[0,0,1,1,2,2,3,3,4,4,5,5,6,6,7,7])
  call io_with_replicated_dims( &
       decompDim1 = 4, &
       decompDim2 = 4, &
       fname = 'test_multiple_des_per_pet.nc', &
       delayout = delayout, &
       skipRead = .true.)

  call ESMF_Finalize()

end program replicated_dims_io
