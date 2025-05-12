module mod_rw_hdf5

   USE HDF5 ! This module contains all necessary modules

   IMPLICIT NONE
   private
   public :: arrayR_hdf5, arrayI_hdf5, hdf5_array1R, hdf5_array3R, hdf5_array2R, hdf5_1I, hdf5_1R, hdf5_array1I
   contains

   ! subroutine array1R_hdf5(filenameout, dsetname1, dset_data, error)
   !    use HDF5
   !    implicit none
   !    CHARACTER(len=64), intent(in) :: filenameout
   !    CHARACTER(len=64), intent(in) :: dsetname1

   !    INTEGER(HID_T) :: file_id       ! File identifier
   !    INTEGER(HID_T) :: dset_id       ! Dataset identifier
   !    INTEGER(HID_T) :: space_id       ! Dataspace identifier
   !    INTEGER(HID_T) :: dtype_id       ! Dataspace identifier

   !    INTEGER     ::   error ! Error flag
   !    INTEGER  :: dname_exist
   !    logical :: file_exists

   !    REAL(KIND = 8), DIMENSION(:) :: dset_data
   !    INTEGER(HSIZE_T), DIMENSION(:), ALLOCATABLE :: data_dims

   !    data_dims = shape(dset_data)   

   !    ! Initialize FORTRAN interface.

   !    CALL h5open_f(error)

   !    ! Open an existing file.
   !    INQUIRE(File=filenameout,EXIST=file_exists)
   !    if(file_exists) then
   !       CALL h5fopen_f(filenameout, H5F_ACC_RDWR_F, file_id, error)

   !       CALL h5dopen_f(file_id, dsetname1, dset_id, dname_exist)

   !       if(dname_exist.ne.0) then
   !          CALL h5screate_simple_f(size(data_dims),data_dims,space_id,error)
   !          CALL h5dcreate_f(file_id,dsetname1,H5T_NATIVE_DOUBLE,space_id,dset_id,error)
   !       endif 
   !    else
   !       CALL h5fcreate_f(filenameout, H5F_ACC_TRUNC_F, file_id, error)
   !       CALL h5screate_simple_f(size(data_dims),data_dims,space_id,error)
   !       CALL h5dcreate_f(file_id,dsetname1,H5T_NATIVE_DOUBLE,space_id,dset_id,error)
   !    endif

   !    !Get dataspace ID
   !    CALL h5dwrite_f(dset_id, H5T_NATIVE_DOUBLE,dset_data,data_dims,error)

   !    CALL h5dclose_f(dset_id,error)

   !    if (dname_exist.ne.0) then
   !       CALL h5sclose_f(space_id,error)
   !    endif

   !    CALL h5fclose_f(file_id,error)

   !    CALL h5close_f(error)

   ! end subroutine array1R_hdf5

   subroutine arrayR_hdf5(filenameout, dsetname1, dset_data, error)
      use HDF5
      implicit none
      CHARACTER(len=*), intent(in) :: filenameout
      CHARACTER(len=*), intent(in) :: dsetname1

      INTEGER(HID_T) :: file_id       ! File identifier
      INTEGER(HID_T) :: dset_id       ! Dataset identifier
      INTEGER(HID_T) :: space_id       ! Dataspace identifier
      INTEGER(HID_T) :: dtype_id       ! Dataspace identifier

      INTEGER     ::   error ! Error flag
      INTEGER  :: dname_exist
      logical :: file_exists

      REAL(KIND = 8), DIMENSION(..) :: dset_data
      INTEGER(HSIZE_T), DIMENSION(:), ALLOCATABLE :: data_dims

      data_dims = shape(dset_data)   

      ! Initialize FORTRAN interface.

      CALL h5open_f(error)

      ! Open an existing file.
      INQUIRE(File=filenameout,EXIST=file_exists)
      if(file_exists) then
         CALL h5fopen_f(filenameout, H5F_ACC_RDWR_F, file_id, error)

         CALL h5dopen_f(file_id, dsetname1, dset_id, dname_exist)

         if(dname_exist.ne.0) then
            CALL h5screate_simple_f(size(data_dims),data_dims,space_id,error)
            CALL h5dcreate_f(file_id,dsetname1,H5T_NATIVE_DOUBLE,space_id,dset_id,error)
         endif 
      else
         CALL h5fcreate_f(filenameout, H5F_ACC_TRUNC_F, file_id, error)
         CALL h5screate_simple_f(size(data_dims),data_dims,space_id,error)
         CALL h5dcreate_f(file_id,dsetname1,H5T_NATIVE_DOUBLE,space_id,dset_id,error)
      endif

      !Get dataspace ID
      SELECT RANK(dset_data)
         RANK(1)
            CALL h5dwrite_f(dset_id, H5T_NATIVE_DOUBLE,dset_data(:),data_dims,error)
         RANK(2)
            CALL h5dwrite_f(dset_id, H5T_NATIVE_DOUBLE,dset_data(:,:),data_dims,error)            
         RANK(3)
            CALL h5dwrite_f(dset_id, H5T_NATIVE_DOUBLE,dset_data(:,:,:),data_dims,error)      
      END SELECT
      CALL h5dclose_f(dset_id,error)

      if (dname_exist.ne.0) then
         CALL h5sclose_f(space_id,error)
      endif

      CALL h5fclose_f(file_id,error)

      CALL h5close_f(error)

   end subroutine arrayR_hdf5

   subroutine arrayI_hdf5(filenameout, dsetname1, dset_data, error)
      use HDF5
      implicit none
      CHARACTER(len=*), intent(in) :: filenameout
      CHARACTER(len=*), intent(in) :: dsetname1

      INTEGER(HID_T) :: file_id       ! File identifier
      INTEGER(HID_T) :: dset_id       ! Dataset identifier
      INTEGER(HID_T) :: space_id       ! Dataspace identifier
      INTEGER(HID_T) :: dtype_id       ! Dataspace identifier

      INTEGER     ::   error ! Error flag
      INTEGER  :: dname_exist
      logical :: file_exists

      INTEGER, DIMENSION(..) :: dset_data
      INTEGER(HSIZE_T), DIMENSION(:), ALLOCATABLE :: data_dims

      data_dims = shape(dset_data)   

      ! Initialize FORTRAN interface.

      CALL h5open_f(error)

      ! Open an existing file.
      INQUIRE(File=filenameout,EXIST=file_exists)
      if(file_exists) then
         CALL h5fopen_f(filenameout, H5F_ACC_RDWR_F, file_id, error)

         CALL h5dopen_f(file_id, dsetname1, dset_id, dname_exist)

         if(dname_exist.ne.0) then
            CALL h5screate_simple_f(size(data_dims),data_dims,space_id,error)
            CALL h5dcreate_f(file_id,dsetname1,H5T_NATIVE_INTEGER,space_id,dset_id,error)
         endif 
      else
         CALL h5fcreate_f(filenameout, H5F_ACC_TRUNC_F, file_id, error)
         CALL h5screate_simple_f(size(data_dims),data_dims,space_id,error)
         CALL h5dcreate_f(file_id,dsetname1,H5T_NATIVE_INTEGER,space_id,dset_id,error)
      endif

      !Get dataspace ID
      SELECT RANK(dset_data)
         RANK(1)
            CALL h5dwrite_f(dset_id, H5T_NATIVE_INTEGER,dset_data(:),data_dims,error)
         RANK(2)
            CALL h5dwrite_f(dset_id, H5T_NATIVE_INTEGER,dset_data(:,:),data_dims,error)            
         RANK(3)
            CALL h5dwrite_f(dset_id, H5T_NATIVE_INTEGER,dset_data(:,:,:),data_dims,error)      
      END SELECT
      CALL h5dclose_f(dset_id,error)

      if (dname_exist.ne.0) then
         CALL h5sclose_f(space_id,error)
      endif

      CALL h5fclose_f(file_id,error)

      CALL h5close_f(error)

   end subroutine arrayI_hdf5

   subroutine hdf5_1I(filename, dsetname, var, error)
      use HDF5
      implicit none
      CHARACTER(len=*), intent(in) :: filename
      CHARACTER(len=*), intent(in) :: dsetname
      INTEGER, intent(INOUT) :: var       !variable out

      INTEGER(HID_T) :: file_id       ! File identifier
      INTEGER(HID_T) :: dset_id       ! Dataset identifier
      INTEGER(HID_T) :: space_id       ! Dataspace identifier
      INTEGER(HID_T) :: dtype_id       ! Dataspace identifier

      INTEGER     ::   error ! Error flag

      INTEGER, DIMENSION(:),ALLOCATABLE :: dset_data
      INTEGER(HSIZE_T), DIMENSION(1) :: data_dims

      ! Initialize FORTRAN interface.

      CALL h5open_f(error)
      CALL h5fopen_f(filename, H5F_ACC_RDONLY_F, file_id, error)
      CALL h5dopen_f(file_id, dsetname, dset_id, error)

      if(error == 0) then
         CALL h5dget_space_f(dset_id,space_id,error)
         CALL h5sget_simple_extent_dims_f(space_id, data_dims, data_dims, error)

         ALLOCATE(dset_data(data_dims(1)))

         CALL h5dread_f(dset_id, H5T_NATIVE_INTEGER, dset_data, data_dims, error)

      endif

      CALL h5dclose_f(dset_id,error)

      CALL h5sclose_f(space_id,error)

      CALL h5fclose_f(file_id,error)

      CALL h5close_f(error)

      var = dset_data(1)

   end subroutine hdf5_1I

   subroutine hdf5_1R(filename, dsetname, var, error)
      use HDF5
      implicit none
      CHARACTER(len=*), intent(in) :: filename
      CHARACTER(len=*), intent(in) :: dsetname
      REAL(KIND = 8), intent(INOUT) :: var       !variable out

      INTEGER(HID_T) :: file_id       ! File identifier
      INTEGER(HID_T) :: dset_id       ! Dataset identifier
      INTEGER(HID_T) :: space_id       ! Dataspace identifier
      INTEGER(HID_T) :: dtype_id       ! Dataspace identifier

      INTEGER     ::   error ! Error flag

      REAL(KIND = 8), DIMENSION(:),ALLOCATABLE :: dset_data
      INTEGER(HSIZE_T), DIMENSION(1) :: data_dims

      ! Initialize FORTRAN interface.

      CALL h5open_f(error)
      CALL h5fopen_f(filename, H5F_ACC_RDONLY_F, file_id, error)
      CALL h5dopen_f(file_id, dsetname, dset_id, error)

      if(error == 0) then
         CALL h5dget_space_f(dset_id,space_id,error)
         CALL h5sget_simple_extent_dims_f(space_id, data_dims, data_dims, error)

         ALLOCATE(dset_data(data_dims(1)))

         CALL h5dread_f(dset_id, H5T_NATIVE_DOUBLE, dset_data, data_dims, error)

      endif

      CALL h5dclose_f(dset_id,error)

      CALL h5sclose_f(space_id,error)

      CALL h5fclose_f(file_id,error)

      CALL h5close_f(error)

      var = dset_data(1)

   end subroutine hdf5_1R

   subroutine hdf5_array1R(filename, dsetname, dset_data, error)
      use HDF5
      implicit none
      CHARACTER(len=*), intent(in) :: filename
      CHARACTER(len=*), intent(in) :: dsetname

      INTEGER(HID_T) :: file_id       ! File identifier
      INTEGER(HID_T) :: dset_id       ! Dataset identifier
      INTEGER(HID_T) :: space_id       ! Dataspace identifier
      INTEGER(HID_T) :: dtype_id       ! Dataspace identifier

      INTEGER     ::   error ! Error flag

      REAL(KIND = 8), DIMENSION(:),ALLOCATABLE :: dset_data
      INTEGER(HSIZE_T), DIMENSION(1) :: data_dims

      ! Initialize FORTRAN interface.

      CALL h5open_f(error)
      CALL h5fopen_f(filename, H5F_ACC_RDONLY_F, file_id, error)
      CALL h5dopen_f(file_id, dsetname, dset_id, error)

      if(error == 0) then
         CALL h5dget_space_f(dset_id,space_id,error)
         CALL h5sget_simple_extent_dims_f(space_id, data_dims, data_dims, error)

         ALLOCATE(dset_data(data_dims(1)))

         CALL h5dread_f(dset_id, H5T_NATIVE_DOUBLE, dset_data, data_dims, error)

      endif

      CALL h5dclose_f(dset_id,error)

      CALL h5sclose_f(space_id,error)

      CALL h5fclose_f(file_id,error)

      CALL h5close_f(error)

   end subroutine hdf5_array1R

   subroutine hdf5_array3R(filename, dsetname, dset_data, error)
      use HDF5
      implicit none
      CHARACTER(len=*), intent(in) :: filename
      CHARACTER(len=*), intent(in) :: dsetname

      INTEGER(HID_T) :: file_id       ! File identifier
      INTEGER(HID_T) :: dset_id       ! Dataset identifier
      INTEGER(HID_T) :: space_id       ! Dataspace identifier
      INTEGER(HID_T) :: dtype_id       ! Dataspace identifier

      INTEGER     ::   error ! Error flag

      REAL(KIND = 8), DIMENSION(:,:,:),ALLOCATABLE :: dset_data
      INTEGER(HSIZE_T), DIMENSION(3) :: data_dims

      ! Initialize FORTRAN interface.

      CALL h5open_f(error)
      CALL h5fopen_f(filename, H5F_ACC_RDONLY_F, file_id, error)
      CALL h5dopen_f(file_id, dsetname, dset_id, error)

      if(error == 0) then
         CALL h5dget_space_f(dset_id,space_id,error)
         CALL h5sget_simple_extent_dims_f(space_id, data_dims, data_dims, error)

         ALLOCATE(dset_data(data_dims(1),data_dims(2),data_dims(3)))

         CALL h5dread_f(dset_id, H5T_NATIVE_DOUBLE, dset_data, data_dims, error)

      endif

      CALL h5dclose_f(dset_id,error)

      CALL h5sclose_f(space_id,error)

      CALL h5fclose_f(file_id,error)

      CALL h5close_f(error)

   end subroutine hdf5_array3R

   subroutine hdf5_array2R(filename, dsetname, dset_data, error)
      use HDF5
      implicit none
      CHARACTER(len=*), intent(in) :: filename
      CHARACTER(len=*), intent(in) :: dsetname

      INTEGER(HID_T) :: file_id       ! File identifier
      INTEGER(HID_T) :: dset_id       ! Dataset identifier
      INTEGER(HID_T) :: space_id       ! Dataspace identifier
      INTEGER(HID_T) :: dtype_id       ! Dataspace identifier

      INTEGER     ::   error ! Error flag

      REAL(KIND = 8), DIMENSION(:,:),ALLOCATABLE :: dset_data
      INTEGER(HSIZE_T), DIMENSION(2) :: data_dims

      ! Initialize FORTRAN interface.

      CALL h5open_f(error)
      CALL h5fopen_f(filename, H5F_ACC_RDONLY_F, file_id, error)
      CALL h5dopen_f(file_id, dsetname, dset_id, error)

      if(error == 0) then
         CALL h5dget_space_f(dset_id,space_id,error)
         CALL h5sget_simple_extent_dims_f(space_id, data_dims, data_dims, error)

         ALLOCATE(dset_data(data_dims(1),data_dims(2)))

         CALL h5dread_f(dset_id, H5T_NATIVE_DOUBLE, dset_data, data_dims, error)

      endif

      CALL h5dclose_f(dset_id,error)

      CALL h5sclose_f(space_id,error)

      CALL h5fclose_f(file_id,error)

      CALL h5close_f(error)

   end subroutine hdf5_array2R   

   subroutine hdf5_array1I(filename, dsetname, dset_data, error)
      use HDF5
      implicit none
      CHARACTER(len=*), intent(in) :: filename
      CHARACTER(len=*), intent(in) :: dsetname

      INTEGER(HID_T) :: file_id       ! File identifier
      INTEGER(HID_T) :: dset_id       ! Dataset identifier
      INTEGER(HID_T) :: space_id       ! Dataspace identifier
      INTEGER(HID_T) :: dtype_id       ! Dataspace identifier

      INTEGER     ::   error ! Error flag

      INTEGER, DIMENSION(:),ALLOCATABLE :: dset_data
      INTEGER(HSIZE_T), DIMENSION(1) :: data_dims

      ! Initialize FORTRAN interface.

      CALL h5open_f(error)
      CALL h5fopen_f(filename, H5F_ACC_RDONLY_F, file_id, error)
      CALL h5dopen_f(file_id, dsetname, dset_id, error)

      if(error == 0) then
         CALL h5dget_space_f(dset_id,space_id,error)
         CALL h5sget_simple_extent_dims_f(space_id, data_dims, data_dims, error)

         ALLOCATE(dset_data(data_dims(1)))

         CALL h5dread_f(dset_id, H5T_NATIVE_INTEGER, dset_data, data_dims, error)

      endif

      CALL h5dclose_f(dset_id,error)

      CALL h5sclose_f(space_id,error)

      CALL h5fclose_f(file_id,error)

      CALL h5close_f(error)

   end subroutine hdf5_array1I

end module mod_rw_hdf5