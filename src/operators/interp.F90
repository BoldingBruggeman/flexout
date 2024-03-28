module output_operators_interp

   use output_manager_core
   use field_manager
   use yaml_settings
   use output_operators_base

   implicit none

   private

   public type_interp_operator

   type, extends(type_base_operator) :: type_interp_operator
      character(len=string_length) :: dimension
      character(len=string_length) :: target_dimension_name
      character(len=string_length) :: target_long_name
      character(len=string_length) :: target_standard_name
      real(rk), allocatable        :: target_coordinates(:)
      type (type_field), pointer   :: source_field => null()
      type (type_field), pointer   :: offset_field => null()
      integer                      :: out_of_bounds_treatment = 1
      real(rk)                     :: offset_scale = 1._rk
      type (type_dimension), pointer :: target_dimension => null()
   contains
      procedure :: configure
      procedure :: apply
   end type

   type, extends(type_operator_result) :: type_result
      integer                      :: idim = -1
      integer                      :: idatadim = -1
      class (type_base_output_field), pointer :: source_coordinate => null()
      class (type_base_output_field), pointer :: offset => null()
      integer                      :: out_of_bounds_treatment = 1
      real(rk)                     :: out_of_bounds_value
      real(rk), allocatable        :: target_coordinates(:)
      real(rk)                     :: offset_scale = 1._rk
   contains
      procedure :: flag_as_required
      procedure :: before_save
      procedure :: get_field
   end type

   type, extends(type_list_populator) :: type_coordinate_list_populator
      class (type_interp_operator), pointer :: operator => null()
   contains
      procedure :: set_length => coordinate_list_set_length
      procedure :: create     => coordinate_list_create_element
   end type

contains

   subroutine configure(self, settings, field_manager)
      class (type_interp_operator), target, intent(inout) :: self
      class (type_settings),                intent(inout) :: settings
      type (type_field_manager),            intent(inout) :: field_manager

      type (type_dimension), pointer        :: dim
      character(len=string_length)          :: variable_name
      type (type_coordinate_list_populator) :: populator

      call settings%get(self%dimension, 'dimension', 'dimension to interpolate')

      ! Verify target dimension has been registered with field manager
      dim => field_manager%first_dimension
      do while (associated(dim))
         if (dim%name==self%dimension) exit
         dim => dim%next
      end do
      if (.not. associated(dim)) call host%fatal_error('type_interp_operator%initialize', &
         'Dimension "'//trim(self%dimension)//'" has not been registered with the field manager.')

      call settings%get(self%out_of_bounds_treatment, 'out_of_bounds_treatment', 'out-of-bounds treatment', options=(/option(1, 'mask'), option(2, 'nearest'), option(3, 'extrapolate')/), default=1)
      variable_name = settings%get_string('offset', 'variable to use as offset', default='')
      if (variable_name /= '') then
         if (variable_name(1:1) == '-') then
            self%offset_scale = -1._rk
            variable_name = variable_name(2:)
         end if
         self%offset_field => field_manager%select_for_output(trim(variable_name))
      end if
      variable_name = settings%get_string('source_coordinate', 'variable with source coordinates', default='')
      if (variable_name /= '') self%source_field => field_manager%select_for_output(trim(variable_name))

      populator%operator => self
      call settings%get_list('coordinates', populator)
      call settings%get(self%target_dimension_name, 'target_dimension', 'name for new interpolated dimension', default=trim(self%dimension))
      call settings%get(self%target_long_name, 'target_long_name', 'long name for new coordinate variable', default='')
      call settings%get(self%target_standard_name, 'target_standard_name', 'standard name for new coordinate variable', default='')
   end subroutine

   recursive subroutine coordinate_list_set_length(self, n)
      class (type_coordinate_list_populator), intent(inout) :: self
      integer,                                intent(in)    :: n
      allocate(self%operator%target_coordinates(n))
   end subroutine

   recursive subroutine coordinate_list_create_element(self, index, item)
      class (type_coordinate_list_populator), intent(inout) :: self
      integer,                                intent(in)    :: index
      type (type_list_item),                  intent(inout) :: item

      character(len=4) :: strindex
      class (type_real_setting), pointer :: real_setting

      write (strindex, '(i0)') index
      real_setting => type_real_setting_create(item, 'value '//trim(strindex), '', target=self%operator%target_coordinates(index))
      if (index > 1) then
         if (self%operator%target_coordinates(index) < self%operator%target_coordinates(index - 1)) call host%fatal_error('type_interp_operator%configure', trim(item%value%parent%get_path())//' should be monotonically increasing.')
      end if
   end subroutine

   function apply(self, source) result(output_field)
      class (type_interp_operator), intent(inout), target :: self
      class (type_base_output_field), target              :: source
      class (type_base_output_field), pointer             :: output_field

      type (type_dimension_pointer), allocatable :: dimensions(:)
      real(rk) :: out_of_bounds_value
      integer :: idim, i
      class (type_result), pointer :: result
      character(len=:), allocatable :: long_name, units, standard_name, long_name2
      integer, allocatable :: extents(:)

      call source%get_metadata(dimensions=dimensions, fill_value=out_of_bounds_value)
      do idim = 1, size(dimensions)
         if (dimensions(idim)%p%name == self%dimension) exit
      end do
      if (idim > size(dimensions)) then
         output_field => source
         return
      end if
      if (dimensions(idim)%p%length == 1) call host%fatal_error('type_interp_operator%initialize', &
         'Cannot use interp on dimension ' // trim(self%dimension) // ' because it has length 1.')
      if (self%out_of_bounds_treatment == 1 .and. out_of_bounds_value == default_fill_value) &
         call host%fatal_error('type_interp_operator%initialize', 'Cannot use out_of_bounds_value=1 because ' // trim(source%output_name) // ' does not have fill_value set.')

      allocate(result)
      result%operator => self
      result%source => source
      result%output_name = 'interp('//trim(result%source%output_name)//')'
      output_field => result
      result%idim = idim
      result%out_of_bounds_value = out_of_bounds_value
      result%out_of_bounds_treatment = self%out_of_bounds_treatment
      result%offset_scale = self%offset_scale
      allocate(result%target_coordinates(size(self%target_coordinates)))
      result%target_coordinates(:) = self%target_coordinates(:)

      ! Data dim is idim with singletons removed
      result%idatadim = 0
      do i = 1, result%idim
         if (dimensions(i)%p%length > 1) result%idatadim = result%idatadim + 1
      end do

      if (.not. associated(self%source_field)) then
         if (.not. associated(dimensions(idim)%p%coordinate)) call host%fatal_error('type_interp_operator%initialize', &
            'Dimension ' // trim(self%dimension) // ' does not have a default coordinate. &
            &You need to explicitly specify the source coordinate with the source_coordinate attribute to the interp operator.')
         self%source_field => dimensions(idim)%p%coordinate
      end if
      result%source_coordinate => result%source%get_field(self%source_field)
      if (associated(self%offset_field)) result%offset => result%source%get_field(self%offset_field)

      if (.not. associated(self%target_dimension)) then
         allocate(self%target_dimension)
         self%target_dimension%name = trim(self%target_dimension_name)
         self%target_dimension%length = size(self%target_coordinates)
         self%target_dimension%global_length = self%target_dimension%length
         allocate(self%target_dimension%coordinate)
         call self%target_dimension%coordinate%data%set(self%target_coordinates)
         self%target_dimension%coordinate%status = status_registered_with_data
         self%target_dimension%coordinate%name = trim(self%target_dimension_name)
         call result%source_coordinate%get_metadata(long_name=long_name, units=units, standard_name=standard_name)
         if (self%target_long_name /= '') then
            self%target_dimension%coordinate%long_name = trim(self%target_long_name)
         elseif (.not. associated(result%offset)) then
            self%target_dimension%coordinate%long_name = long_name
         else
            call result%offset%get_metadata(long_name=long_name2)
            self%target_dimension%coordinate%long_name = long_name // ' relative to ' // long_name2
         end if
         if (self%target_standard_name /= '') then
            self%target_dimension%coordinate%standard_name = trim(self%target_standard_name)
         elseif (.not. associated(result%offset)) then
            self%target_dimension%coordinate%standard_name = standard_name
         end if
         self%target_dimension%coordinate%units = units
         allocate(self%target_dimension%coordinate%dimensions(1))
         self%target_dimension%coordinate%dimensions(1)%p => self%target_dimension
      end if

      allocate(result%dimensions(size(dimensions)))
      result%dimensions(:) = dimensions
      result%dimensions(result%idim)%p => self%target_dimension

      call source%data%get_extents(extents)
      if (result%idatadim /= size(extents)) call host%fatal_error('type_interp_operator%initialize', 'interp can currently only operate on final dimension of source array.')
      extents(result%idatadim) = size(self%target_coordinates)
      call result%allocate(extents)
      call result%fill(result%out_of_bounds_value)
   end function

   recursive subroutine flag_as_required(self, required)
      class (type_result), intent(inout) :: self
      logical,             intent(in)    :: required

      call self%type_operator_result%flag_as_required(required)
      if (associated(self%source_coordinate)) call self%source_coordinate%flag_as_required(required)
      if (associated(self%offset)) call self%offset%flag_as_required(required)
   end subroutine

   recursive subroutine before_save(self)
      class (type_result), intent(inout) :: self

      integer :: i, j
      real(rk) :: offset
      real(rk), allocatable :: source_coordinate(:)

      call self%type_operator_result%before_save()
      if (self%idim == -1) return
      if (associated(self%source%data%p3d)) then
         allocate(source_coordinate(size(self%source%data%p3d, self%idatadim)))
         if (associated(self%source_coordinate%data%p1d)) source_coordinate(:) = self%source_coordinate%data%p1d
         do j=1,size(self%source%data%p3d, 2)
            do i=1, size(self%source%data%p3d, 1)
               offset = 0._rk
               if (associated(self%offset)) offset = self%offset_scale * self%offset%data%p2d(i,j)
               if (associated(self%source_coordinate%data%p3d)) source_coordinate(:) = self%source_coordinate%data%p3d(i,j,:)
               call interp(self%target_coordinates(:) + offset, source_coordinate, self%source%data%p3d(i,j,:), self%result_3d(i,j,:), self%out_of_bounds_treatment, self%out_of_bounds_value)
            end do
         end do
      elseif (associated(self%source%data%p2d)) then
         allocate(source_coordinate(size(self%source%data%p2d, self%idatadim)))
         if (associated(self%source_coordinate%data%p1d)) source_coordinate(:) = self%source_coordinate%data%p1d
         do i=1, size(self%source%data%p2d, 1)
            offset = 0._rk
            if (associated(self%offset)) offset = self%offset_scale * self%offset%data%p1d(i)
            if (associated(self%source_coordinate%data%p2d)) source_coordinate(:) = self%source_coordinate%data%p2d(i,:)
            call interp(self%target_coordinates(:) + offset, source_coordinate, self%source%data%p2d(i,:), self%result_2d(i,:), self%out_of_bounds_treatment, self%out_of_bounds_value)
         end do
      elseif (associated(self%source%data%p1d)) then
         offset = 0._rk
         if (associated(self%offset)) offset = self%offset_scale * self%offset%data%p0d
         call interp(self%target_coordinates(:) + offset, self%source_coordinate%data%p1d, self%source%data%p1d, self%result_1d, self%out_of_bounds_treatment, self%out_of_bounds_value)
      end if
   end subroutine

   recursive function get_field(self, field) result(output_field)
      class (type_result), intent(in)         :: self
      type (type_field), target               :: field
      class (type_base_output_field), pointer :: output_field
      if (associated(self%dimensions(self%idim)%p%coordinate, field)) then
         output_field => self%type_base_output_field%get_field(field)
      else
         output_field => self%type_operator_result%get_field(field)
      end if
   end function

   subroutine interp(x, xp, fp, f, out_of_bounds_treatment, out_of_bounds_value)
      real(rk), intent(in) :: x(:), xp(:), fp(:)
      real(rk), intent(out) :: f(:)
      integer,  intent(in) :: out_of_bounds_treatment
      real(rk), intent(in) :: out_of_bounds_value

      integer :: i, j, istart, istop
      real(rk) :: slope

      ! Find first non-masked source value
      istart = 0
      do i = 1, size(xp)
         if (fp(i) /= out_of_bounds_value) then
            istart = i
            exit
         end if
      end do

      ! If all source values are masked, return missing value
      if (istart == 0) then
         f(:) = out_of_bounds_value
         return
      end if

      ! Find last non-masked source value
      do istop = size(xp), istart, -1
         if (fp(istop) /= out_of_bounds_value) exit
      end do

      i = istart
      do j = 1, size(x)
         if (out_of_bounds_treatment /= 3 .and. (x(j) < xp(istart) .or. x(j) > xp(istop))) then
            if (out_of_bounds_treatment == 1) then
               ! Use missing value
               f(j) = out_of_bounds_value
            else
               ! Use nearest valid value
               if (x(j) < xp(istart)) then
                  f(j) = fp(istart)
               else
                  f(j) = fp(istop)
               end if
            end if
         else
            do while (i + 1 < istop)
               if (xp(i + 1) >= x(j)) exit
               i = i + 1
            end do
            slope = (fp(i + 1) - fp(i)) / (xp(i + 1) - xp(i))
            f(j) = fp(i) + (x(j) - xp(i)) * slope
         end if
      end do
   end subroutine

end module