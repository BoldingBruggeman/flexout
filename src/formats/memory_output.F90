module memory_output
   use field_manager
   use output_manager_core

   implicit none

   public type_memory_file

   private

   type,extends(type_file) :: type_memory_file
      integer,  allocatable :: offsets(:)
      real(rk), allocatable :: data(:)
   contains
      procedure :: initialize
      procedure :: save
      procedure :: restore
      procedure :: write_metadata
   end type

contains

   subroutine initialize(self)
      class (type_memory_file), intent(inout) :: self

      class (type_base_output_field), pointer :: output_field
      type (type_dimension_pointer), allocatable :: dimensions(:)
      integer ::  n, ntot, ivar, idim, nvar

      nvar = 0
      output_field => self%first_field
      do while (associated(output_field))
         nvar = nvar + 1
         output_field => output_field%next
      end do
      allocate(self%offsets(nvar))

      ivar = 1
      ntot = 0
      output_field => self%first_field
      do while (associated(output_field))
         self%offsets(ivar) = ntot + 1
         call output_field%get_metadata(dimensions=dimensions)
         n = 1
         do idim = 1, size(dimensions)
            if (dimensions(idim)%p%id /= id_dim_time) n = n * dimensions(idim)%p%length
         end do
         ntot = ntot + n
         ivar = ivar + 1
         output_field => output_field%next
      end do
      allocate(self%data(ntot))
   end subroutine initialize

   subroutine save(self, julianday, secondsofday, microseconds)
      class (type_memory_file), intent(inout) :: self
      integer,                  intent(in)    :: julianday, secondsofday, microseconds

      class (type_base_output_field), pointer :: output_field
      integer                                 :: ivar, offset, length, j, k

      ivar = 1
      output_field => self%first_field
      do while (associated(output_field))
         if (associated(output_field%data%p3d)) then
            offset = self%offsets(ivar)
            length = size(output_field%data%p3d, 1)
            do k = 1, size(output_field%data%p3d, 3)
               do j = 1, size(output_field%data%p3d, 2)
                  self%data(offset:offset + length - 1) = output_field%data%p3d(:, j, k)
                  offset = offset + length
               end do
            end do
         elseif (associated(output_field%data%p2d)) then
            offset = self%offsets(ivar)
            length = size(output_field%data%p2d, 1)
            do j = 1, size(output_field%data%p2d, 2)
               self%data(offset:offset + length - 1) = output_field%data%p2d(:, j)
               offset = offset + length
            end do
         elseif (associated(output_field%data%p1d)) then
            self%data(self%offsets(ivar):self%offsets(ivar) + size(output_field%data%p1d) - 1) = output_field%data%p1d
         elseif (associated(output_field%data%p0d)) then
            self%data(self%offsets(ivar)) = output_field%data%p0d
         end if
         ivar = ivar + 1
         output_field => output_field%next
      end do
   end subroutine save

   subroutine restore(self)
      class (type_memory_file), intent(inout) :: self

      class (type_base_output_field), pointer :: output_field
      integer                                 :: ivar, offset, length, j, k

      ivar = 1
      output_field => self%first_field
      do while (associated(output_field))
         if (associated(output_field%data%p3d)) then
            offset = self%offsets(ivar)
            length = size(output_field%data%p3d, 1)
            do k = 1, size(output_field%data%p3d, 3)
               do j = 1, size(output_field%data%p3d, 2)
                  output_field%data%p3d(:, j, k) = self%data(offset:offset + length - 1)
                  offset = offset + length
               end do
            end do
         elseif (associated(output_field%data%p2d)) then
            offset = self%offsets(ivar)
            length = size(output_field%data%p2d, 1)
            do j = 1, size(output_field%data%p2d, 2)
               output_field%data%p2d(:, j) = self%data(offset:offset + length - 1)
               offset = offset + length
            end do
         elseif (associated(output_field%data%p1d)) then
            output_field%data%p1d(:) = self%data(self%offsets(ivar):self%offsets(ivar) + size(output_field%data%p1d) - 1)
         elseif (associated(output_field%data%p0d)) then
            output_field%data%p0d = self%data(self%offsets(ivar))
         end if
         ivar = ivar + 1
         output_field => output_field%next
      end do
   end subroutine restore

   subroutine write_metadata(self, unit)
      class (type_memory_file), intent(in) :: self
      integer,                  intent(in) :: unit

      class (type_base_output_field), pointer    :: output_field
      character(len=:), allocatable              :: long_name, units, path
      type (type_dimension_pointer), allocatable :: dimensions(:)
      integer                                    :: ntot, n, idim
      character, parameter                       :: separator = char(9)

      ntot = 0
      output_field => self%first_field
      do while (associated(output_field))
         call output_field%get_metadata(long_name=long_name, units=units, dimensions=dimensions)
         write (unit, '(a,a,a,a,a,a)', advance='NO') trim(output_field%output_name), separator, trim(long_name), separator, trim(units), separator
         if (associated(output_field%category))  write (unit, '(a)', advance='NO') trim(output_field%category%get_path())
         write (unit, '(a)', advance='NO') separator
         n = 1
         do idim = 1, size(dimensions)
            if (idim > 1) write (unit, '(a)', advance='NO') ','
            write (unit, '(a,"=",i0)', advance='NO') trim(dimensions(idim)%p%name), dimensions(idim)%p%length
            if (dimensions(idim)%p%id /= id_dim_time) n = n * dimensions(idim)%p%length
         end do
         write (unit, '(a,i0,a,i0)') separator, ntot + 1, separator, n
         ntot = ntot + n
         output_field => output_field%next
      end do
   end subroutine write_metadata

end module memory_output
