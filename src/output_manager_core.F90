module output_manager_core

   use iso_fortran_env, only: error_unit, output_unit

   use field_manager
   use yaml_settings

   implicit none

   public type_output_variable_settings,type_output_item,type_output_field, type_file, write_time_string, read_time_string, host, type_host
   public type_base_output_field, type_base_operator, wrap_field

   private

   integer,parameter,public :: max_path = 256

   integer,parameter,public :: time_method_none          = 0  ! time-independent variable
   integer,parameter,public :: time_method_instantaneous = 1
   integer,parameter,public :: time_method_mean          = 2
   integer,parameter,public :: time_method_integrated    = 3

   integer,parameter,public :: time_unit_none   = 0
   integer,parameter,public :: time_unit_second = 1
   integer,parameter,public :: time_unit_hour   = 2
   integer,parameter,public :: time_unit_day    = 3
   integer,parameter,public :: time_unit_month  = 4
   integer,parameter,public :: time_unit_year   = 5
   integer,parameter,public :: time_unit_dt     = 6
   integer,parameter,public :: time_from_list   = 7

   integer,parameter,public :: rk = kind(_ONE_)

   type,abstract :: type_host
   contains
      procedure (host_julian_day),deferred :: julian_day
      procedure (host_calendar_date),deferred :: calendar_date
      procedure :: fatal_error => host_fatal_error
      procedure :: log_message => host_log_message
   end type

   abstract interface
      subroutine host_julian_day(self,yyyy,mm,dd,julian)
         import type_host
         class (type_host), intent(in) :: self
         integer, intent(in)  :: yyyy,mm,dd
         integer, intent(out) :: julian
      end subroutine
   end interface

   abstract interface
      subroutine host_calendar_date(self,julian,yyyy,mm,dd)
         import type_host
         class (type_host), intent(in) :: self
         integer, intent(in)  :: julian
         integer, intent(out) :: yyyy,mm,dd
      end subroutine
   end interface

   type type_output_variable_settings
      integer :: time_method = time_method_instantaneous
      class (type_base_operator), pointer :: final_operator => null()
   contains
      procedure :: initialize => output_variable_settings_initialize
      procedure :: finalize   => output_variable_settings_finalize
   end type

   type type_output_item
      class (type_output_variable_settings), pointer :: settings => null()
      character(len=string_length)         :: name = ''
      character(len=string_length)         :: prefix = ''
      character(len=string_length)         :: postfix = ''
      integer                              :: output_level = output_level_default
      class (type_category_node),  pointer :: category => null()
      type (type_field),           pointer :: field => null()
      type (type_output_item),     pointer :: next => null()
   end type

   type type_output_field_pointer
      class (type_base_output_field), pointer :: p => null()
   end type

   type type_base_output_field
      class (type_output_variable_settings), pointer :: settings => null()
      character(len=string_length)                   :: output_name = ''
      logical                                        :: is_coordinate = .false.
      class (type_category_node),  pointer           :: category => null()
      type (type_nd_data_pointer)                    :: data
      type (type_output_field_pointer), allocatable  :: coordinates(:)
      class (type_base_output_field),        pointer :: next => null()
   contains
      procedure :: new_data         => base_field_new_data
      procedure :: before_save      => base_field_before_save
      procedure :: flag_as_required => base_field_flag_as_required
      procedure :: get_metadata     => base_field_get_metadata
      procedure :: get_field        => base_field_get_field
      procedure :: finalize         => base_field_finalize
   end type type_base_output_field

   type, extends(type_base_output_field) :: type_output_field
      type (type_field), pointer :: source   => null()
      logical,           pointer :: used_now => null()
   contains
      procedure :: flag_as_required => field_flag_as_required
      procedure :: get_metadata     => field_get_metadata
   end type type_output_field

   type type_file
      type (type_field_manager),    pointer :: field_manager   => null()
      character(len=max_path)               :: path            = ''
      character(len=max_path)               :: postfix         = ''
      character(len=string_length)          :: title           = ''
      type (type_attributes)                :: attributes
      integer                               :: time_unit       = time_unit_none
      integer                               :: time_step       = 0
      integer                               :: first_index     = 0
      integer                               :: next_julian     = -1
      integer                               :: next_seconds    = -1
      integer                               :: first_julian    = -1
      integer                               :: first_seconds   = -1
      integer                               :: last_julian     = huge(1)
      integer                               :: last_seconds    = 0
      type (type_output_item),       pointer :: first_item     => null()
      class (type_base_output_field),pointer :: first_field    => null()
      class (type_file),             pointer :: next           => null()
   contains
      procedure :: configure
      procedure :: initialize
      procedure :: save
      procedure :: finalize
      procedure :: create_settings
      procedure :: is_dimension_used
      procedure :: append_item
   end type type_file

   type type_base_operator
      integer                             :: refcount = 1
      class (type_base_operator), pointer :: previous => null()
   contains
      procedure :: configure   => operator_configure
      procedure :: apply       => operator_apply
      procedure :: dereference => operator_dereference
      procedure :: finalize    => operator_finalize
      procedure :: apply_all   => operator_apply_all
   end type

   class (type_host),pointer,save :: host => null()

contains

   recursive subroutine base_field_flag_as_required(self, required)
      class (type_base_output_field), intent(inout) :: self
      logical, intent(in) :: required
   end subroutine

   recursive subroutine base_field_get_metadata(self, long_name, units, dimensions, minimum, maximum, fill_value, standard_name, path, attributes)
      class (type_base_output_field), intent(in) :: self
      character(len=:), allocatable, intent(out), optional :: long_name, units, standard_name, path
      type (type_dimension_pointer), allocatable, intent(out), optional :: dimensions(:)
      real(rk), intent(out), optional :: minimum, maximum, fill_value
      type (type_attributes), intent(out), optional :: attributes
      if (present(dimensions)) allocate(dimensions(0))
   end subroutine

   recursive subroutine base_field_new_data(self)
      class (type_base_output_field), intent(inout) :: self
   end subroutine

   recursive subroutine base_field_before_save(self)
      class (type_base_output_field), intent(inout) :: self
   end subroutine

   recursive function base_field_get_field(self, field) result(output_field)
      class (type_base_output_field), intent(in) :: self
      type (type_field), target                  :: field
      class (type_base_output_field), pointer    :: output_field
      output_field => wrap_field(field, .false.)
   end function

   recursive subroutine base_field_finalize(self)
      class (type_base_output_field), intent(inout) :: self
      if (associated(self%settings)) then
         call self%settings%finalize()
         deallocate(self%settings)
      end if
   end subroutine

   function wrap_field(field, allow_unregistered) result(output_field)
      type (type_field), target          :: field
      logical, intent(in)                :: allow_unregistered
      class (type_output_field), pointer :: output_field
      output_field => null()
      select case (field%status)
      case (status_not_registered)
         if (allow_unregistered) then
            call host%log_message('WARNING: output field "'//trim(field%name)//'" is skipped because it has not been registered with field manager.')
         else
            call host%fatal_error('wrap_field', 'Requested output field "'//trim(field%name)//'" has not been registered with field manager.')
         end if
      case (status_registered_no_data)
         call host%fatal_error('wrap_field', 'Data for requested field "'//trim(field%name)//'" have not been provided to field manager.')
      case default
         allocate(output_field)
         output_field%source => field
         output_field%data = output_field%source%data
         output_field%output_name = trim(field%name)
         output_field%used_now => field%used_now
      end select
   end function

   recursive subroutine field_flag_as_required(self, required)
      class (type_output_field), intent(inout) :: self
      logical, intent(in) :: required
      if (associated(self%used_now) .and. required) self%used_now = .true.
   end subroutine

   recursive subroutine field_get_metadata(self, long_name, units, dimensions, minimum, maximum, fill_value, standard_name, path, attributes)
      class (type_output_field), intent(in) :: self
      character(len=:), allocatable, intent(out), optional :: long_name, units, standard_name, path
      type (type_dimension_pointer), allocatable, intent(out), optional :: dimensions(:)
      type (type_attributes), intent(out), optional :: attributes
      real(rk), intent(out), optional :: minimum, maximum, fill_value

      if (self%source%status == status_not_registered) then
         if (present(dimensions)) allocate(dimensions(0))
         return
      end if
      if (present(long_name)) long_name = trim(self%source%long_name)
      if (present(units)) units = trim(self%source%units)
      if (present(dimensions)) then
         allocate(dimensions(size(self%source%dimensions)))
         dimensions(:) = self%source%dimensions(:)
      end if
      if (present(minimum)) minimum = self%source%minimum
      if (present(maximum)) maximum = self%source%maximum
      if (present(fill_value)) fill_value = self%source%fill_value
      if (present(standard_name) .and. self%source%standard_name /= '') standard_name = trim(self%source%standard_name)
      if (present(path) .and. associated(self%source%category)) path = trim(self%source%category%get_path())
      if (present(attributes)) call attributes%update(self%source%attributes)
   end subroutine

   subroutine configure(self, settings)
      class (type_file),     intent(inout) :: self
      class (type_settings), intent(inout) :: settings
   end subroutine

   subroutine initialize(self)
      class (type_file),intent(inout) :: self
      stop 'output_manager_core:initialize not implemented'
   end subroutine

   function create_settings(self) result(settings)
      class (type_file),intent(inout) :: self
      class (type_output_variable_settings), pointer :: settings
      allocate(settings)
   end function create_settings

   subroutine save(self,julianday,secondsofday,microseconds)
      class (type_file),intent(inout) :: self
      integer,          intent(in)    :: julianday,secondsofday,microseconds
      stop 'output_manager_core:save not implemented'
   end subroutine

   subroutine finalize(self)
      class (type_file), intent(inout) :: self

      class (type_base_output_field), pointer :: current, next

      current => self%first_field
      do while (associated(current))
         next => current%next
         call current%finalize()
         deallocate(current)
         current => next
      end do
   end subroutine

   subroutine write_time_string(jul,secs,timestr)
      integer,         intent(in)  :: jul,secs
      character(len=*),intent(out) :: timestr

      integer :: ss,min,hh,dd,mm,yy

      hh   = secs/3600
      min  = (secs-hh*3600)/60
      ss   = secs - 3600*hh - 60*min

      call host%calendar_date(jul,yy,mm,dd)

      write(timestr,'(i4.4,a1,i2.2,a1,i2.2,1x,i2.2,a1,i2.2,a1,i2.2)')  &
                           yy,'-',mm,'-',dd,hh,':',min,':',ss
   end subroutine write_time_string

   subroutine read_time_string(timestr,jul,secs,success)
      character(len=19)    :: timestr
      integer, intent(out) :: jul,secs
      logical, intent(out) :: success

      integer   :: ios
      character :: c1,c2,c3,c4
      integer   :: yy,mm,dd,hh,min,ss

      read(timestr,'(i4,a1,i2,a1,i2,1x,i2,a1,i2,a1,i2)',iostat=ios)  &
                          yy,c1,mm,c2,dd,hh,c3,min,c4,ss
      success = ios == 0
      if (ios==0) then
         call host%julian_day(yy,mm,dd,jul)
         secs = 3600*hh + 60*min + ss
      end if
   end subroutine read_time_string

   subroutine host_fatal_error(self,location,error)
      class (type_host), intent(in) :: self
      character(len=*),  intent(in) :: location,error

      write (error_unit,*) trim(location)//': '//trim(error)
      stop 1
   end subroutine

   subroutine host_log_message(self,message)
      class (type_host), intent(in) :: self
      character(len=*),  intent(in) :: message

      write (output_unit,*) trim(message)
   end subroutine

   logical function is_dimension_used(self,dim)
      class (type_file),intent(inout) :: self
      type (type_dimension), target   :: dim

      class (type_base_output_field),pointer :: output_field
      type (type_dimension_pointer), allocatable :: dimensions(:)
      integer :: i

      is_dimension_used = .true.
      output_field => self%first_field
      do while (associated(output_field))
         call output_field%get_metadata(dimensions=dimensions)
         do i=1,size(dimensions)
            if (associated(dimensions(i)%p,dim)) return
         end do
         output_field => output_field%next
      end do
      is_dimension_used = .false.
   end function is_dimension_used


   subroutine append_item(self, item)
      class (type_file),intent(inout) :: self
      type (type_output_item), target :: item

      ! Select this category for output in the field manager.
      if (.not.associated(item%settings)) item%settings => self%create_settings()
      if (.not. associated(item%field)) item%category => self%field_manager%select_category_for_output(item%name, item%output_level)

      ! Prepend to list of output categories.
      item%next => self%first_item
      self%first_item => item
   end subroutine append_item

   subroutine output_variable_settings_initialize(self, settings, parent)
      class (type_output_variable_settings), intent(inout)        :: self
      class (type_settings),                 intent(inout)        :: settings
      class (type_output_variable_settings), intent(in), optional :: parent

      integer                             :: display
      class (type_base_operator), pointer :: op

      display = display_normal
      if (present(parent)) then
         self%time_method = parent%time_method
         self%final_operator => parent%final_operator
         display = display_advanced
         op => parent%final_operator
         do while (associated(op))
            op%refcount = op%refcount + 1
            op => op%previous
         end do
      end if

      self%time_method = settings%get_integer('time_method', 'treatment of time dimension', options=(/option(time_method_mean, 'mean', 'mean'), &
         option(time_method_instantaneous, 'instantaneous', 'point'), option(time_method_integrated, 'integrated', 'integrated')/), default=self%time_method, display=display)
   end subroutine output_variable_settings_initialize

   recursive subroutine output_variable_settings_finalize(self)
      class (type_output_variable_settings), intent(inout) :: self
      if (associated(self%final_operator)) then
         if (self%final_operator%dereference()) deallocate(self%final_operator)
      end if
   end subroutine

   subroutine operator_configure(self, settings, field_manager)
      class (type_base_operator), target, intent(inout) :: self
      class (type_settings),              intent(inout) :: settings
      type (type_field_manager),          intent(inout) :: field_manager
   end subroutine

   function operator_apply(self, source) result(output_field)
      class (type_base_operator), intent(inout), target :: self
      class (type_base_output_field), target            :: source
      class (type_base_output_field), pointer           :: output_field
      output_field => source
   end function

   recursive function operator_apply_all(self, source) result(output_field)
      class (type_base_operator), intent(inout), target :: self
      class (type_base_output_field), target            :: source
      class (type_base_output_field), pointer           :: output_field
      output_field => source
      if (associated(self%previous)) output_field => self%previous%apply_all(output_field)
      if (associated(output_field)) output_field => self%apply(output_field)
   end function

   logical recursive function operator_dereference(self)
      class (type_base_operator), intent(inout) :: self
      self%refcount = self%refcount - 1
      operator_dereference = self%refcount == 0
      if (operator_dereference) call self%finalize()
   end function

   recursive subroutine operator_finalize(self)
      class (type_base_operator), intent(inout) :: self
      if (associated(self%previous)) then
         if (self%previous%dereference()) deallocate(self%previous)
      end if
   end subroutine

end module output_manager_core
