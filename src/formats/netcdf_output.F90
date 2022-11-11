module netcdf_output
   use field_manager
   use output_manager_core
   use yaml_settings
#ifdef NETCDF_FMT
   use netcdf

   implicit none

   public type_netcdf_file, NF90_FLOAT, NF90_DOUBLE
   public type_netcdf_variable_settings

   integer, save, public :: default_xtype = NF90_FLOAT
   integer, save, public :: default_coordinate_xtype = NF90_FLOAT

   private

   type,extends(type_file) :: type_netcdf_file
      integer :: itime         = 0  ! Next time index in NetCDF file
      integer :: ncid          = -1 ! NetCDF identifier for file
      integer :: time_id       = -1 ! Identifier of time dimension
      integer :: reference_julian  = -1
      integer :: reference_seconds = -1
      integer :: sync_interval = 1  ! Number of output time step between calls to nf90_sync (-1 to disable syncing)
   contains
      procedure :: configure
      procedure :: initialize
      procedure :: save
      procedure :: finalize
      procedure :: create_settings
   end type

   type,extends(type_output_variable_settings) :: type_netcdf_variable_settings
      integer :: varid = -1
      integer,allocatable :: start(:)
      integer,allocatable :: edges(:)
      integer :: itimedim = -1
      integer :: xtype = -1
   contains
      procedure :: initialize => netcdf_variable_settings_initialize
   end type

contains

   subroutine configure(self, settings)
      class (type_netcdf_file), intent(inout) :: self
      class (type_settings),    intent(inout) :: settings

      character(len=:), allocatable :: time_reference
      logical :: success

      ! Determine time of first output (default to start of simulation)
      time_reference = settings%get_string('time_reference', 'reference date and time to use in time units', units='yyyy-mm-dd HH:MM:SS', default='', display=display_advanced)
      if (time_reference /= '') then
         call read_time_string(time_reference, self%reference_julian, self%reference_seconds, success)
         if (.not. success) call host%fatal_error('process_file','Error parsing output.yaml: invalid value "'//time_reference//'" specified for '//trim(self%path)//'/time_reference. Required format: yyyy-mm-dd HH:MM:SS.')
      end if

      ! Determine interval between calls to nf90_sync (default: after every output)
      call settings%get(self%sync_interval, 'sync_interval', 'number of output steps between sychronization to disk (<= 0: sync on close only)', default=1, display=display_advanced)
   end subroutine

   subroutine initialize(self)
      class (type_netcdf_file),intent(inout) :: self

      class (type_base_output_field), pointer :: output_field
      integer                            :: iret
      integer                            :: i
      integer,allocatable                :: current_dim_ids(:)
      integer                            :: length
      character(len=19)                  :: time_string
      character(len=256)                 :: coordinates
      type (type_dimension), pointer     :: dim
      character(len=:), allocatable :: long_name, units, standard_name, path
      type (type_dimension_pointer), allocatable :: dimensions(:)
      real(rk) :: minimum, maximum, fill_value
      type (type_attributes) :: attributes

      type type_dimension_ids
         type (type_dimension),     pointer :: dimension    => null()
         integer                            :: netcdf_dimid = -1
         type (type_dimension_ids), pointer :: next         => null()
      end type
      type (type_dimension_ids), pointer :: first_dim_id

      if (.not.associated(self%first_field)) then
         call host%log_message('NOTE: "'//trim(self%path)//trim(self%postfix)//'.nc" will not be created because it would contain no data.')
         return
      end if

      ! If no reference time is configured (to be used in time units), use time of first output.
      if (self%reference_julian==-1) then
         self%reference_julian  = self%first_julian
         self%reference_seconds = self%first_seconds
      end if

      first_dim_id => null()

      ! Create NetCDF file
      iret = nf90_create(trim(self%path)//trim(self%postfix)//'.nc',NF90_CLOBBER,self%ncid); call check_err(iret)

      ! Create recommended CF global attributes
      if ( len(trim(self%title)) .gt. 0) then
         iret = nf90_put_att(self%ncid,NF90_GLOBAL,'title',trim(self%title)); call check_err(iret)
      end if
      iret = nf90_put_att(self%ncid,NF90_GLOBAL,'comment','file created by flexout - https://github.com/BoldingBruggeman/flexout'); call check_err(iret)
      call set_attributes(NF90_GLOBAL, self%attributes)

      ! Create time coordinate
      dim => self%field_manager%find_dimension(id_dim_time)
      if (self%is_dimension_used(dim)) then
         iret = nf90_def_var(self%ncid,trim(dim%name),NF90_DOUBLE,(/get_dim_id(dim)/),self%time_id); call check_err(iret)
         call write_time_string(self%reference_julian,self%reference_seconds,time_string)
         iret = nf90_put_att(self%ncid,self%time_id,'long_name','time'); call check_err(iret)
         iret = nf90_put_att(self%ncid,self%time_id,'units','seconds since '//trim(time_string)); call check_err(iret)
         iret = nf90_put_att(self%ncid,self%time_id,'calendar','standard'); call check_err(iret)
      end if

      ! Create variables
      output_field => self%first_field
      do while (associated(output_field))
         call output_field%get_metadata(long_name=long_name, units=units, dimensions=dimensions, minimum=minimum, maximum=maximum, fill_value=fill_value, standard_name=standard_name, path=path, attributes=attributes)
         select type (settings=>output_field%settings)
         class is (type_netcdf_variable_settings)
            ! Map internal dimension indices to indices in NetCDF file.
            allocate(current_dim_ids(size(dimensions)))
            do i=1,size(dimensions)
               current_dim_ids(i) = get_dim_id(dimensions(i)%p)
            end do

            if (settings%xtype == -1) then
               if (output_field%is_coordinate) then
                  settings%xtype = default_coordinate_xtype
               else
                  settings%xtype = default_xtype
               end if
            end if

            iret = nf90_def_var(self%ncid, trim(output_field%output_name), settings%xtype, current_dim_ids, settings%varid); call check_err(iret)
            deallocate(current_dim_ids)

            iret = nf90_put_att(self%ncid,settings%varid,'units',units); call check_err(iret)
            iret = nf90_put_att(self%ncid,settings%varid,'long_name',long_name); call check_err(iret)
            if (allocated(standard_name)) iret = nf90_put_att(self%ncid,settings%varid,'standard_name',standard_name); call check_err(iret)
            if (minimum/=default_minimum) iret = put_att_typed_real(self%ncid,settings%varid,'valid_min',minimum,settings%xtype); call check_err(iret)
            if (maximum/=default_maximum) iret = put_att_typed_real(self%ncid,settings%varid,'valid_max',maximum,settings%xtype); call check_err(iret)
            if (fill_value/=default_fill_value) iret = put_att_typed_real(self%ncid,settings%varid,'_FillValue',fill_value,settings%xtype); call check_err(iret)
            if (fill_value/=default_fill_value) iret = put_att_typed_real(self%ncid,settings%varid,'missing_value',fill_value,settings%xtype); call check_err(iret)
            if (allocated(path)) iret = nf90_put_att(self%ncid,settings%varid,'path',path); call check_err(iret)
            call set_attributes(settings%varid, attributes)
            call attributes%finalize()

            coordinates = ''
            do i=1,size(output_field%coordinates)
               if (associated(output_field%coordinates(i)%p)) then
                  if (output_field%coordinates(i)%p%output_name /= dimensions(i)%p%name) &
                     coordinates = trim(coordinates) // ' ' // trim(output_field%coordinates(i)%p%output_name)
               end if
            end do
            if (coordinates /= '') then
               iret = nf90_put_att(self%ncid,settings%varid,'coordinates',trim(coordinates(2:))); call check_err(iret)
            end if

            ! Fill arrays with start index and count per dimension
            allocate(settings%start(size(dimensions)))
            allocate(settings%edges(size(dimensions)))
            do i=1,size(dimensions)
               if (dimensions(i)%p%id==id_dim_time) then
                  settings%start(i) = self%itime
                  settings%edges(i) = 1
                  settings%itimedim = i
               else
                  settings%start(i) = 1
                  settings%edges(i) = dimensions(i)%p%length
               end if
            end do
         end select
         output_field => output_field%next
      end do

      ! Exit define mode
      iret = nf90_enddef(self%ncid); call check_err(iret)

   contains

      subroutine set_attributes(varid, attributes)
         integer,                intent(in) :: varid
         type (type_attributes), intent(in) :: attributes

         class (type_attribute), pointer    :: attribute

         attribute => attributes%first
         do while (associated(attribute))
            select type (attribute)
            class is (type_real_attribute)
               iret = nf90_put_att(self%ncid, varid, trim(attribute%name), attribute%value); call check_err(iret)
            class is (type_integer_attribute)
               iret = nf90_put_att(self%ncid, varid, trim(attribute%name), attribute%value); call check_err(iret)
            class is (type_string_attribute)
               iret = nf90_put_att(self%ncid, varid, trim(attribute%name), trim(attribute%value)); call check_err(iret)
            end select
            attribute => attribute%next
         end do
      end subroutine

      integer function get_dim_id(dim)
         type (type_dimension), pointer     :: dim
         type (type_dimension_ids), pointer :: dim_id
         dim_id => first_dim_id
         do while (associated(dim_id))
            if (dim_id%dimension%name == dim%name .and. dim_id%dimension%length == dim%length) exit
            dim_id => dim_id%next
         end do
         if (.not. associated(dim_id)) then
            allocate(dim_id)
            dim_id%dimension => dim
            dim_id%next => first_dim_id
            first_dim_id => dim_id
            if (dim%id==id_dim_time) then
               length = NF90_UNLIMITED
            else
               length = dim%length
            end if
            iret = nf90_def_dim(self%ncid, trim(dim%name), length, dim_id%netcdf_dimid); call check_err(iret)
         end if
         get_dim_id = dim_id%netcdf_dimid
      end function
   end subroutine initialize

   function put_att_typed_real(ncid,varid,name,value,data_type) result(iret)
      integer,         intent(in) :: ncid,varid,data_type
      character(len=*),intent(in) :: name
      real(rk),        intent(in) :: value
      integer :: iret

      select case (data_type)
      case (NF90_FLOAT)
         iret = nf90_put_att(ncid,varid,name,real(value,kind(NF90_FILL_FLOAT)))
      case (NF90_DOUBLE)
         iret = nf90_put_att(ncid,varid,name,real(value,kind(NF90_FILL_DOUBLE)))
      case default
         call host%fatal_error('put_real_att','invalid value for data_type')
      end select
   end function put_att_typed_real

   function create_settings(self) result(settings)
      class (type_netcdf_file),intent(inout) :: self
      class (type_output_variable_settings), pointer :: settings
      allocate(type_netcdf_variable_settings::settings)
   end function create_settings

   subroutine save(self,julianday,secondsofday,microseconds)
      class (type_netcdf_file),intent(inout) :: self
      integer,                 intent(in)    :: julianday,secondsofday,microseconds

      class (type_base_output_field), pointer :: output_field
      integer                                 :: iret
      real(rk)                                :: time_value

      if (self%ncid==-1) return

      ! Increment time index
      self%itime = self%itime + 1

      ! Store time coordinate
      if (self%time_id/=-1) then
         time_value = (julianday-self%reference_julian)*real(86400,rk) + secondsofday-self%reference_seconds + microseconds*1.e-6_rk
         iret = nf90_put_var(self%ncid,self%time_id,time_value,(/self%itime/))
         if (iret/=NF90_NOERR) call host%fatal_error('netcdf_output:save','error saving variable "time" to '//trim(self%path)//trim(self%postfix)//'.nc: '//nf90_strerror(iret))
      end if

      output_field => self%first_field
      do while (associated(output_field))
         select type (settings=>output_field%settings)
         class is (type_netcdf_variable_settings)
            if (settings%itimedim/=-1) settings%start(settings%itimedim) = self%itime
            if (associated(output_field%data%p3d)) then
               iret = nf90_put_var(self%ncid,settings%varid,output_field%data%p3d,settings%start,settings%edges)
            elseif (associated(output_field%data%p2d)) then
               iret = nf90_put_var(self%ncid,settings%varid,output_field%data%p2d,settings%start,settings%edges)
            elseif (associated(output_field%data%p1d)) then
               iret = nf90_put_var(self%ncid,settings%varid,output_field%data%p1d,settings%start,settings%edges)
            elseif (associated(output_field%data%p0d)) then
               iret = nf90_put_var(self%ncid,settings%varid,output_field%data%p0d,settings%start)
            end if
            if (iret/=NF90_NOERR) call host%fatal_error('netcdf_output:save','error saving variable "'//trim(output_field%output_name)//'" to '//trim(self%path)//trim(self%postfix)//'.nc: '//nf90_strerror(iret))
         end select
         output_field => output_field%next
      end do

      if (self%sync_interval > 0) then
         if (mod(self%itime, self%sync_interval) == 0) then
            iret = nf90_sync(self%ncid)
            if (iret/=NF90_NOERR) call host%fatal_error('netcdf_output:save','error in call to nf90_sync for '//trim(self%path)//trim(self%postfix)//'.nc: '//nf90_strerror(iret))
         end if
      end if
   end subroutine save

   subroutine finalize(self)
      class (type_netcdf_file),intent(inout) :: self

      integer :: iret

      if (self%ncid/=-1) then
         iret = nf90_close(self%ncid); call check_err(iret)
      end if
      call self%type_file%finalize()
   end subroutine finalize

   subroutine check_err(iret)
      integer,intent(in) :: iret
      if (iret/=NF90_NOERR) &
         call host%fatal_error('check_err',nf90_strerror(iret))
   end subroutine

   subroutine netcdf_variable_settings_initialize(self, settings, parent)
      class (type_netcdf_variable_settings),           intent(inout) :: self
      class (type_settings),                           intent(inout) :: settings
      class (type_output_variable_settings), optional, intent(in)    :: parent

      call self%type_output_variable_settings%initialize(settings, parent)

      if (present(parent)) then
         select type (parent)
         class is (type_netcdf_variable_settings)
            self%xtype = parent%xtype
         end select
      end if
      self%xtype = settings%get_integer('xtype', 'data type', options=(/option(-1, 'default', 'default'), option(NF90_FLOAT, '32-bit float', 'single'), option(NF90_DOUBLE, '64-bit double', 'double')/), default=self%xtype, display=display_advanced)
   end subroutine netcdf_variable_settings_initialize

#endif
end module netcdf_output
