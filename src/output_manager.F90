module output_manager

   use field_manager
   use output_manager_core
   use netcdf_output
   use text_output
   use output_operators_library
   use output_operators_time_average
   use output_operators_slice

   use yaml_settings

   implicit none

   private

   public output_manager_init, output_manager_start, output_manager_prepare_save, output_manager_save, output_manager_clean, &
      output_manager_add_file, global_attributes

   class (type_file), pointer :: first_file
   logical                    :: files_initialized
   logical, save, public, target :: allow_missing_fields = .false.
   type (type_logical_pointer), allocatable :: used(:)

   interface output_manager_save
      module procedure output_manager_save1
      module procedure output_manager_save2
   end interface

   type,extends(type_dictionary_populator) :: type_file_populator
      type (type_field_manager), pointer :: fm => null()
      character(len=:), allocatable :: title
      character(len=:), allocatable :: postfix
      logical :: ignore = .false.
   contains
      procedure :: create => process_file
   end type

   type,extends(type_list_populator) :: type_operator_populator
      type (type_field_manager), pointer             :: field_manager     => null()
      class (type_output_variable_settings), pointer :: variable_settings => null()
   contains
      procedure :: create => create_operator_settings
   end type

   type,extends(type_list_populator) :: type_group_populator
      class (type_file), pointer :: file
      class (type_output_variable_settings), pointer :: variable_settings => null()
   contains
      procedure :: create => create_group_settings
   end type

   type,extends(type_list_populator) :: type_variable_populator
      class (type_file), pointer :: file => null()
      class (type_output_variable_settings), pointer :: variable_settings => null()
   contains
      procedure :: create => create_variable_settings
   end type

   type (type_attributes) :: global_attributes

contains

   subroutine output_manager_init(field_manager, title, postfix, settings)
      type (type_field_manager), target :: field_manager
      character(len=*),           intent(in) :: title
      character(len=*), optional, intent(in) :: postfix
      class (type_settings), pointer, optional :: settings

      if (.not.associated(host)) then
         write (*,*) 'output_manager_init: the host of an output manager must set the host pointer before calling output_manager_init'
         stop 1
      end if
      nullify(first_file)
      files_initialized = .false.
      call configure_from_yaml(field_manager, title, postfix, settings)
   end subroutine

   subroutine output_manager_clean()
      class (type_file), pointer :: file, next

      file => first_file
      do while (associated(file))
         next => file%next
         call file%finalize()
         deallocate(file)
         file => next
      end do
      first_file => null()
      if (allocated(used)) deallocate(used)
   end subroutine

   subroutine populate(file)
      class (type_file), intent(inout) :: file

      type (type_output_item),               pointer :: item, next_item
      type (type_field_set)                          :: set
      class (type_field_set_member),         pointer :: member, next_member
      class (type_output_variable_settings), pointer :: output_settings
      type (type_settings)                           :: settings
      class (type_base_output_field),        pointer :: output_field, coordinate_field
      type (type_dimension_pointer), allocatable     :: dimensions(:)
      integer                                        :: i

      ! First add fields selected by name
      ! (they take priority over fields included with wildcard expressions)
      item => file%first_item
      do while (associated(item))
         if (associated(item%field)) call create_field(item%settings, item%field, trim(item%name), .false.)
         item => item%next
      end do

      item => file%first_item
      do while (associated(item))
         if (associated(item%category)) then
            call host%log_message('Processing output category /'//trim(item%name)//':')
            if (item%category%has_fields() .or. allow_missing_fields) then
               ! Gather all variables in this category in a set
               call item%category%get_all_fields(set, item%output_level)

               ! Loop over all variables in the set and add them to the file
               member => set%first
               if (.not. associated(member)) call host%log_message('WARNING: output category "'//trim(item%name)//'" does not contain any variables with requested output level.')
               do while (associated(member))
                  call host%log_message('  - '//trim(member%field%name))

                  ! Create a separate object with variable settings, which gets initialized from the category settings.
                  ! The initialization routine is provide with a dummy (empty) configuration node, which gets cleaned up right after.
                  output_settings => file%create_settings()
                  call output_settings%initialize(settings, item%settings)
                  call settings%finalize()

                  ! Add the variable
                  call create_field(output_settings, member%field, trim(item%prefix) // trim(member%field%name) // trim(item%postfix), .true., item%category)

                  ! Move to next set member and deallocate the current member (we are cleaning up the set as we go along)
                  next_member => member%next
                  deallocate(member)
                  member => next_member
               end do

               ! Set is now empty (all items have been deallocated in previous loop)
               set%first => null()

               ! Deallocate the category settings (no longer needed; each contained variable has its own copy of the settings)
               call item%settings%finalize()
               deallocate(item%settings)
            else
               call host%fatal_error('collect_from_categories','No variables have been registered under output category "'//trim(item%name)//'".')
            end if
         end if

         ! Move to next item and deallocate the current item (we are cleaning up the item list as we go along)
         next_item => item%next
         deallocate(item)
         item => next_item
      end do

      ! Item list is now empty (all items have been deallocated in previous loop)
      file%first_item => null()

      output_field => file%first_field
      do while (associated(output_field))
         call output_field%get_metadata(dimensions=dimensions)
         allocate(output_field%coordinates(size(dimensions)))
         do i=1,size(dimensions)
            if (.not.associated(dimensions(i)%p)) cycle
            if (.not.associated(dimensions(i)%p%coordinate)) cycle
            coordinate_field => find_field(dimensions(i)%p%coordinate%name)
            if (.not. associated(coordinate_field)) then
               coordinate_field => output_field%get_field(dimensions(i)%p%coordinate)
               if (associated(coordinate_field)) call append_field(trim(dimensions(i)%p%coordinate%name), coordinate_field, file%create_settings())
            end if
            if (associated(coordinate_field)) coordinate_field%is_coordinate = .true.
            output_field%coordinates(i)%p => coordinate_field
         end do
         output_field => output_field%next
      end do

   contains

      function find_field(output_name) result(field)
         character(len=*), intent(in)            :: output_name
         class (type_base_output_field), pointer :: field
         field => file%first_field
         do while (associated(field))
            if (field%output_name == output_name) return
            field => field%next
         end do
      end function

      subroutine create_field(output_settings, field, output_name, ignore_if_exists, category)
         class (type_output_variable_settings), target :: output_settings
         type (type_field),                     target :: field
         character(len=*), intent(in)                  :: output_name
         logical,          intent(in)                  :: ignore_if_exists
         class (type_category_node), optional,  target :: category

         class (type_base_output_field), pointer :: output_field

         output_field => find_field(output_name)
         if (associated(output_field)) then
            if (.not. ignore_if_exists) call host%fatal_error('create_field', 'A different output field with name "'//output_name//'" already exists.')
            return
         end if

         output_field => wrap_field(field, allow_missing_fields)

         if (associated(output_settings%final_operator)) output_field => output_settings%final_operator%apply_all(output_field)
         if (associated(output_field)) call append_field(output_name, output_field, output_settings, category)
      end subroutine

      subroutine append_field(output_name, output_field, output_settings, category)
         character(len=*),               intent(in)            :: output_name
         class (type_base_output_field), intent(inout), target :: output_field
         class (type_output_variable_settings),         target :: output_settings
         class (type_category_node), optional,          target :: category

         class (type_base_output_field), pointer :: last_field

         output_field%settings => output_settings
         output_field%output_name = trim(output_name)
         if (present(category)) output_field%category => category

         if (associated(file%first_field)) then
            last_field => file%first_field
            do while (associated(last_field%next))
               last_field => last_field%next
            end do
            last_field%next => output_field
         else
            file%first_field => output_field
         end if
      end subroutine

   end subroutine populate

   subroutine output_manager_start(julianday,secondsofday,microseconds,n)
      integer, intent(in) :: julianday,secondsofday,microseconds,n

      class (type_file), pointer :: file

      file => first_file
      do while (associated(file))
         call populate(file)

         ! If we do not have a start time yet, use current.
         if (file%first_julian <= 0) then
            file%first_julian = julianday
            file%first_seconds = secondsofday
         end if

         ! Create output file
         call file%initialize()

         file => file%next
      end do
      if (associated(first_file)) then
         call first_file%field_manager%collect_used(used)
      else
         allocate(used(0))
      end if
      files_initialized = .true.
   end subroutine

   subroutine set_next_output(self, julianday, secondsofday, microseconds, n)
      class (type_file), intent(inout) :: self
      integer,           intent(in)    :: julianday, secondsofday, microseconds, n

      integer                             :: yyyy,mm,dd,yyyy0,mm0
      integer(kind=selected_int_kind(12)) :: offset

      if (self%time_unit == time_unit_none) return

      ! Determine time (julian day, seconds of day) for first output.
      self%next_julian = self%first_julian
      self%next_seconds = self%first_seconds
      offset = 86400*(julianday-self%first_julian) + (secondsofday-self%first_seconds)
      if (offset > 0) then
         select case (self%time_unit)
            case (time_unit_second)
               self%next_seconds = self%next_seconds + ((offset+self%time_step-1)/self%time_step)*self%time_step
               self%next_julian = self%next_julian + self%next_seconds/86400
               self%next_seconds = mod(self%next_seconds,86400)
            case (time_unit_hour)
               self%next_seconds = self%next_seconds + ((offset+self%time_step*3600-1)/(self%time_step*3600))*self%time_step*3600
               self%next_julian = self%next_julian + self%next_seconds/86400
               self%next_seconds = mod(self%next_seconds,86400)
            case (time_unit_day)
               self%next_julian = self%next_julian + ((offset+self%time_step*86400-1)/(self%time_step*86400))*self%time_step
            case (time_unit_month)
               call host%calendar_date(julianday,yyyy,mm,dd)
               call host%calendar_date(self%first_julian,yyyy,mm0,dd)
               mm = mm0 + ((mm-mm0+self%time_step-1)/self%time_step)*self%time_step
               yyyy = yyyy + (mm-1)/12
               mm = mod(mm-1,12)+1
               call host%julian_day(yyyy,mm,dd,self%next_julian)
               if (self%next_julian == julianday .and. secondsofday > self%next_seconds) then
                  mm = mm + self%time_step
                  yyyy = yyyy + (mm-1)/12
                  mm = mod(mm-1,12)+1
                  call host%julian_day(yyyy,mm,dd,self%next_julian)
               end if
            case (time_unit_year)
               call host%calendar_date(julianday,yyyy,mm,dd)
               call host%calendar_date(self%first_julian,yyyy0,mm,dd)
               yyyy = yyyy0 + ((yyyy-yyyy0+self%time_step-1)/self%time_step)*self%time_step
               call host%julian_day(yyyy,mm,dd,self%next_julian)
               if (self%next_julian == julianday .and. secondsofday > self%next_seconds) then
                  yyyy = yyyy + self%time_step
                  call host%julian_day(yyyy,mm,dd,self%next_julian)
               end if
         end select
      end if
   end subroutine

   subroutine output_manager_save1(julianday,secondsofday,n)
      integer,intent(in) :: julianday,secondsofday,n
      call output_manager_save2(julianday,secondsofday,0,n)
   end subroutine

   subroutine output_manager_prepare_save(julianday, secondsofday, microseconds, n)
      integer,intent(in) :: julianday, secondsofday, microseconds, n

      integer                                 :: i
      class (type_file),              pointer :: file
      class (type_base_output_field), pointer :: output_field
      logical                                 :: required

      if (.not. files_initialized) call output_manager_start(julianday, secondsofday, microseconds, n)

      ! Start by marking all fields as not needing computation
      do i = 1, size(used)
         used(i)%p = .false.
      end do

      file => first_file
      do while (associated(file))
         if (in_window(file, julianday, secondsofday, microseconds, n)) then
            if (file%next_julian == -1) call set_next_output(file, julianday, secondsofday, microseconds, n)
            output_field => file%first_field
            do while (associated(output_field))
               select case (file%time_unit)
               case (time_unit_dt)
                  required = file%first_index == -1 .or. mod(n - file%first_index, file%time_step) == 0
               case default
                  required = file%next_julian == -1 .or. (julianday == file%next_julian .and. secondsofday >= file%next_seconds) .or. julianday > file%next_julian
               end select
               call output_field%flag_as_required(required)
               output_field => output_field%next
            end do
         end if
         file => file%next
      end do
   end subroutine

   logical function in_window(self, julianday, secondsofday, microseconds, n)
      class (type_file), intent(in) :: self
      integer,           intent(in) :: julianday, secondsofday, microseconds, n

      in_window = ((julianday == self%first_julian .and. secondsofday >= self%first_seconds) .or. julianday > self%first_julian) &
            .and. ((julianday == self%last_julian .and. secondsofday <= self%last_seconds)  .or. julianday < self%last_julian)
   end function

   subroutine output_manager_save2(julianday,secondsofday,microseconds,n)
      integer,intent(in) :: julianday,secondsofday,microseconds,n

      class (type_file),              pointer :: file
      class (type_base_output_field), pointer :: output_field
      integer                                 :: yyyy,mm,dd
      logical                                 :: output_required

      if (.not.files_initialized) call output_manager_start(julianday,secondsofday,microseconds,n)

      file => first_file
      do while (associated(file))
         if (in_window(file, julianday, secondsofday, microseconds, n)) then
            if (file%next_julian == -1) call set_next_output(file, julianday, secondsofday, microseconds, n)

            ! Increment time-integrated fields
            output_field => file%first_field
            do while (associated(output_field))
               call output_field%new_data()
               output_field => output_field%next
            end do

            ! Determine whether output is required
            select case (file%time_unit)
            case (time_unit_none)
               output_required = .false.
            case (time_unit_dt)
               if (file%first_index == -1) file%first_index = n
               output_required = mod(n - file%first_index, file%time_step) == 0
            case default
               output_required  = (julianday == file%next_julian .and. secondsofday >= file%next_seconds) .or. julianday > file%next_julian
            end select

            if (output_required) then
               output_field => file%first_field
               do while (associated(output_field))
                  call output_field%before_save()
                  output_field => output_field%next
               end do

               ! Do output
               call file%save(julianday,secondsofday,microseconds)

               ! Determine time (julian day, seconds of day) for next output.
               select case (file%time_unit)
               case (time_unit_second)
                  file%next_seconds = file%next_seconds + file%time_step
                  file%next_julian = file%next_julian + file%next_seconds/86400
                  file%next_seconds = mod(file%next_seconds,86400)
               case (time_unit_hour)
                  file%next_seconds = file%next_seconds + file%time_step*3600
                  file%next_julian = file%next_julian + file%next_seconds/86400
                  file%next_seconds = mod(file%next_seconds,86400)
               case (time_unit_day)
                  file%next_julian = file%next_julian + file%time_step
               case (time_unit_month)
                  call host%calendar_date(julianday,yyyy,mm,dd)
                  mm = mm + file%time_step
                  yyyy = yyyy + (mm-1)/12
                  mm = mod(mm-1,12)+1
                  call host%julian_day(yyyy,mm,dd,file%next_julian)
               case (time_unit_year)
                  call host%calendar_date(julianday,yyyy,mm,dd)
                  yyyy = yyyy + file%time_step
                  call host%julian_day(yyyy,mm,dd,file%next_julian)
               end select
            end if

         end if ! in output time window

         file => file%next
      end do
   end subroutine output_manager_save2

   subroutine configure_from_yaml(field_manager, title, postfix, settings)
      type (type_field_manager), target      :: field_manager
      character(len=*),           intent(in) :: title
      character(len=*), optional, intent(in) :: postfix
      class (type_settings), pointer, optional :: settings

      logical                        :: file_exists
      integer,parameter              :: yaml_unit = 100
      class (type_settings), pointer :: settings_
      type (type_settings)           :: ignored_settings
      type (type_file_populator)     :: populator

      inquire(file='output.yaml',exist=file_exists)

      populator%fm => field_manager
      populator%title = trim(title)
      if (present(postfix)) populator%postfix = postfix

      if (present(settings)) then
         settings_ => settings
      else
         allocate(settings_)
      end if

      if (file_exists) then
         ! Settings from output.yaml
         if (present(settings)) then
            ! yaml node also provided, but output.yaml takes priority.
            ! Read yaml node values into a dummy settings object, so everything is still properly parsed and the user receives warnings.
            populator%ignore = .true.
            ignored_settings%backing_store_node => settings%backing_store_node
            call ignored_settings%populate(populator)
            populator%ignore = .false.
         end if
         call settings_%load('output.yaml', yaml_unit)
      elseif (.not. present(settings)) then
         call host%log_message('WARNING: no output files will be written because output settings have not been provided.')
         return
      end if
      call settings_%populate(populator)
      if (file_exists) then
         if (.not. settings_%check_all_used()) call host%fatal_error('configure_from_yaml', 'Unknown setting(s) in output.yaml.')
      end if
      if (.not. present(settings)) then
         call settings_%finalize()
         deallocate(settings_)
      end if
   end subroutine configure_from_yaml

   subroutine output_manager_add_file(field_manager, file)
      type (type_field_manager), target :: field_manager
      class (type_file),         target :: file

      file%field_manager => field_manager
      file%next => first_file
      first_file => file
   end subroutine output_manager_add_file

   subroutine process_file(self, pair)
      class (type_file_populator), intent(inout) :: self
      type (type_key_value_pair),  intent(inout) :: pair

      class (type_logical_setting) ,pointer :: logical_setting
      integer                              :: fmt
      class (type_file), pointer           :: file
      character(len=:), allocatable        :: string
      class (type_settings),       pointer :: file_settings
      logical :: success
      class (type_output_variable_settings), pointer :: variable_settings

      type (type_dimension),       pointer :: dim
      integer                              :: global_start, global_stop, stride
      logical                              :: is_active
      class (type_slice_operator), pointer :: slice_operator
      integer                              :: display

      if (pair%key == 'allow_missing_fields') then
         logical_setting => type_logical_setting_create(pair, 'ignore unknown requested output fields', target=allow_missing_fields)
         return
      end if

      file_settings => type_settings_create(pair, 'path of output file, excluding extension')

      is_active = file_settings%get_logical('is_active', 'write output to this file', default=.true., display=display_hidden)
      is_active = file_settings%get_logical('use', 'write output to this file', default=.true., display=display_advanced)
      if (self%ignore) then
         call host%log_message('WARNING: '//pair%value%get_path()//' will be ignored because output.yaml is present.')
         is_active = .false.
      end if
#ifdef NETCDF_FMT
      fmt = file_settings%get_integer('format', 'format', options=(/option(1, 'text', 'text'), option(2, 'NetCDF', 'netcdf')/), default=2, display=display_advanced)
#else
      fmt = file_settings%get_integer('format', 'format', options=(/option(1, 'text', 'text')/), default=1, display=display_advanced)
#endif

      select case (fmt)
      case (1)
         allocate(type_text_file::file)
      case (2)
#ifdef NETCDF_FMT
         allocate(type_netcdf_file::file)
#endif
      end select
      call file%attributes%update(global_attributes)

      ! Create file object and prepend to list.
      file%path = pair%name
      if (allocated(self%postfix)) file%postfix = self%postfix
      file%field_manager => self%fm
      if (is_active) call output_manager_add_file(self%fm, file)

      ! Can be used for CF global attributes
      call file_settings%get(file%title, 'title', 'title', default=self%title, display=display_advanced)
      call file_settings%get(file%time_unit, 'time_unit', 'time unit', default=time_unit_day, options=(/ &
         option(time_unit_second, 'second', 'second'), option(time_unit_hour, 'hour', 'hour'), option(time_unit_day, 'day', 'day'), &
         option(time_unit_month, 'month', 'month'), option(time_unit_year, 'year', 'year'), option(time_unit_dt, 'model time step', 'dt')/))

      ! Determine time step
      call file_settings%get(file%time_step, 'time_step', 'number of time units between output', minimum=1, default=1)
      string = file_settings%get_string('time_start', 'start', 'yyyy-mm-dd HH:MM:SS', default='', display=display_advanced)
      if (string /= '') then
         call read_time_string(string, file%first_julian, file%first_seconds, success)
         if (.not. success) call host%fatal_error('process_file','Error in output configuration: invalid time_start "'//string//'" specified for file "'//pair%name//'". Required format: yyyy-mm-dd HH:MM:SS.')
      end if
      string = file_settings%get_string('time_stop', 'stop', 'yyyy-mm-dd HH:MM:SS', default='', display=display_advanced)
      if (string /= '') then
         call read_time_string(string, file%last_julian, file%last_seconds, success)
         if (.not. success) call host%fatal_error('process_file','Error in output configuration: invalid time_stop "'//string//'" specified for file "'//pair%name//'". Required format: yyyy-mm-dd HH:MM:SS.')
      end if

      ! Determine dimension ranges
      allocate(slice_operator)
      dim => self%fm%first_dimension
      do while (associated(dim))
         if (dim%iterator /= '') then
            display = display_advanced
            if (dim%global_length == 1) display = display_hidden
            global_start = file_settings%get_integer(trim(dim%iterator)//'_start', 'start index for '//trim(dim%iterator)//' dimension', default=1, minimum=0, maximum=dim%global_length, display=display)
            global_stop = file_settings%get_integer(trim(dim%iterator)//'_stop', 'stop index for '//trim(dim%iterator)//' dimension', default=dim%global_length, minimum=1, maximum=dim%global_length, display=display)
            if (global_start > global_stop) call host%fatal_error('process_file','Error parsing output.yaml: '//trim(dim%iterator)//'_stop must equal or exceed '//trim(dim%iterator)//'_start')
            stride = file_settings%get_integer(trim(dim%iterator)//'_stride', 'stride for '//trim(dim%iterator)//' dimension', default=1, minimum=1, display=display)
            call slice_operator%add(trim(dim%name), global_start, global_stop, stride)
         end if
         dim => dim%next
      end do
      variable_settings => file%create_settings()
      call variable_settings%initialize(file_settings)
      variable_settings%final_operator => slice_operator

      ! Allow specific file implementation to parse additional settings from yaml file.
      call file%configure(file_settings)

      call configure_group(file, file_settings, variable_settings)
      call variable_settings%finalize()
      deallocate(variable_settings)
   end subroutine process_file

   recursive subroutine configure_group(file, settings, variable_settings)
      class (type_file), target, intent(inout) :: file
      class (type_settings),     intent(inout) :: settings
      class (type_output_variable_settings), target :: variable_settings

      type (type_operator_populator) :: operator_populator
      type (type_group_populator)    :: group_populator
      type (type_variable_populator) :: variable_populator

      ! Get operators
      operator_populator%field_manager => file%field_manager
      operator_populator%variable_settings => variable_settings
      call settings%get_list('operators', operator_populator, display=display_advanced)

      ! Get list with groups [if any]
      group_populator%file => file
      group_populator%variable_settings => variable_settings
      call settings%get_list('groups', group_populator, display=display_advanced)

      ! Get list with variables
      variable_populator%file => file
      variable_populator%variable_settings => variable_settings
      call settings%get_list('variables', variable_populator)
   end subroutine

   recursive subroutine create_group_settings(self, index, item)
      class (type_group_populator), intent(inout) :: self
      integer,                      intent(in)    :: index
      type (type_list_item),        intent(inout) :: item

      class (type_settings),                 pointer :: group_settings
      class (type_output_variable_settings), pointer :: variable_settings

      ! Obtain dictionary with user-provided settings
      group_settings => type_settings_create(item)

      ! Create object that will contain final output settings for this group
      variable_settings => self%file%create_settings()
      call variable_settings%initialize(group_settings, self%variable_settings)

      ! Configure output settings
      ! This will also read additional user options from variable_settings and apply them to group_settings.
      call configure_group(self%file, group_settings, variable_settings)
      call variable_settings%finalize()
      deallocate(variable_settings)
   end subroutine

   recursive subroutine create_operator_settings(self, index, item)
      class (type_operator_populator), intent(inout) :: self
      integer,                         intent(in)    :: index
      type (type_list_item),           intent(inout) :: item

      class (type_settings), pointer :: settings

      settings => type_settings_create(item)
      call apply_operator(self%variable_settings%final_operator, settings, self%field_manager)
   end subroutine

   recursive subroutine create_variable_settings(self, index, item)
      class (type_variable_populator), intent(inout) :: self
      integer,                         intent(in)    :: index
      type (type_list_item),           intent(inout) :: item

      class (type_settings),              pointer :: variable_settings
      character(len=:), allocatable               :: source_name
      type (type_output_item),            pointer :: output_item
      class (type_time_average_operator), pointer :: time_filter
      integer                                     :: n

      variable_settings => type_settings_create(item)

      ! Name of source variable
      source_name = variable_settings%get_string('source', 'variable name in model')

      allocate(output_item)
      output_item%settings => self%file%create_settings()
      call output_item%settings%initialize(variable_settings, self%variable_settings)

      ! If this output is not instantaneous, add time averaging/integration to the operator stack
      if (output_item%settings%time_method /= time_method_instantaneous .and. output_item%settings%time_method /= time_method_none) then
         allocate(time_filter)
         time_filter%method = output_item%settings%time_method
         time_filter%previous => output_item%settings%final_operator
         output_item%settings%final_operator => time_filter
      end if

      ! Determine whether to create an output field or an output category
      n = len(source_name)
      if (source_name(n:n)=='*') then
         if (n==1) then
            output_item%name = ''
         else
            output_item%name = source_name(:n-2)
         end if

         ! Prefix for output name
         call variable_settings%get(output_item%prefix, 'prefix', 'name prefix used in output', default='', display=display_advanced)

         ! Postfix for output name
         call variable_settings%get(output_item%postfix, 'postfix', 'name postfix used in output', default='', display=display_advanced)

         ! Output level
         call variable_settings%get(output_item%output_level, 'output_level', 'output level', default=output_level_default, display=display_advanced)
      else
         output_item%field => self%file%field_manager%select_for_output(source_name)

         ! Name of output variable (may differ from source name)
         call variable_settings%get(output_item%name, 'name', 'name used in output', default=source_name, display=display_advanced)
      end if
      call self%file%append_item(output_item)
   end subroutine

end module
