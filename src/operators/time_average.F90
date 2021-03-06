module output_operators_time_average

   use output_manager_core
   use field_manager
   use output_operators_base

   implicit none

   private

   public type_time_average_operator

   type, extends(type_base_operator) :: type_time_average_operator
      integer :: method = time_method_mean
   contains
      procedure :: apply
   end type

   type, extends(type_universal_operator_result) :: type_result
      integer :: method = time_method_mean
      integer :: n = 0
   contains
      procedure :: flag_as_required
      procedure :: new_data
      procedure :: before_save
      procedure :: get_metadata
   end type
   
contains

   function apply(self, source) result(output_field)
      class (type_time_average_operator), intent(inout), target :: self
      class (type_base_output_field), target                    :: source
      class (type_base_output_field), pointer                   :: output_field

      real(rk)                                   :: fill_value
      type (type_dimension_pointer), allocatable :: dimensions(:)
      integer                                    :: itimedim
      class (type_result), pointer               :: result
      integer, allocatable                       :: extents(:)

      call source%get_metadata(dimensions=dimensions, fill_value=fill_value)
      do itimedim=1,size(dimensions)
         if (dimensions(itimedim)%p%id == id_dim_time) exit
      end do
      if (itimedim > size(dimensions)) then
         output_field => source
         return
      end if

      allocate(result)
      result%operator => self
      result%source => source
      result%output_name = 'time_average('//trim(result%source%output_name)//')'
      output_field => result
      result%method = self%method

      call source%data%get_extents(extents)
      call result%allocate(extents)
      if (self%method == time_method_mean) call result%fill(fill_value)
   end function

   recursive subroutine flag_as_required(self, required)
      class (type_result), intent(inout) :: self
      logical,             intent(in)    :: required

      call self%source%flag_as_required(.true.)
   end subroutine

   recursive subroutine new_data(self)
      class (type_result), intent(inout) :: self

      integer :: i, j, k

      call self%source%before_save()
      if (self%n == 0) call self%fill(0.0_rk)
      select case (self%rank)
      case (0)
         self%result_0d = self%result_0d + self%source_data%p0d
      case (1)
         do concurrent (i=1:size(self%result_1d))
            self%result_1d(i) = self%result_1d(i) + self%source_data%p1d(i)
         end do
      case (2)
         do concurrent (i=1:size(self%result_2d, 1), j=1:size(self%result_2d, 2))
            self%result_2d(i,j) = self%result_2d(i,j) + self%source_data%p2d(i,j)
         end do
      case (3)
         do concurrent (i=1:size(self%result_3d, 1), j=1:size(self%result_3d, 2), k=1:size(self%result_3d, 3))
            self%result_3d(i,j,k) = self%result_3d(i,j,k) + self%source_data%p3d(i,j,k)
         end do
      end select
      self%n = self%n + 1
   end subroutine

   recursive subroutine before_save(self)
      class (type_result), intent(inout) :: self

      if (self%method == time_method_mean) then
         select case (self%rank)
         case (0)
            self%result_0d = self%result_0d / self%n
         case (1)
            self%result_1d(:) = self%result_1d / self%n
         case (2)
            self%result_2d(:,:) = self%result_2d / self%n
         case (3)
            self%result_3d(:,:,:) = self%result_3d / self%n
         end select
      end if
      self%n = 0
   end subroutine

   recursive subroutine get_metadata(self, long_name, units, dimensions, minimum, maximum, fill_value, standard_name, path, attributes)
      class (type_result), intent(in) :: self
      character(len=:), allocatable, intent(out), optional :: long_name, units, standard_name, path
      type (type_dimension_pointer), allocatable, intent(out), optional :: dimensions(:)
      real(rk), intent(out), optional :: minimum, maximum, fill_value
      type (type_attributes), intent(out), optional :: attributes

      ! Workaround for gfortran BUG 88511 - passing optional allocatable deferred length character arguments to the next routine causes memory corruption
      character(len=:), allocatable :: long_name2, units2, standard_name2, path2

      call self%type_universal_operator_result%get_metadata(long_name2, units2, dimensions, minimum, maximum, fill_value, standard_name2, path2, attributes)

      ! Workaround for gfortran
      if (present(long_name) .and. allocated(long_name2)) long_name = long_name2
      if (present(units) .and. allocated(units2)) units = units2
      if (present(standard_name) .and. allocated(standard_name2)) standard_name = standard_name2
      if (present(path) .and. allocated(path2)) path = path2

      if (present(attributes)) then
         select case (self%method)
         case (time_method_mean)
            call attributes%set('cell_methods', 'time: mean')
         case default
            call attributes%set('cell_methods', 'time: sum')
         end select
      end if
   end subroutine

end module