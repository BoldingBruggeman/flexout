module output_operators_library

   use field_manager
   use yaml_settings
   use output_manager_core

   use output_operators_base
   use output_operators_interp
   use output_operators_time_average
   use output_operators_slice

   implicit none

   private

   public apply_operator

contains

   subroutine apply_operator(final_operator, settings, field_manager)
      class (type_base_operator),     pointer  :: final_operator
      class (type_settings),     intent(inout) :: settings
      type (type_field_manager), intent(inout) :: field_manager

      integer                             :: operator_type
      class (type_base_operator), pointer :: op

      operator_type = settings%get_integer('type', 'operator type', options=(/option(1, 'interp')/))
      select case (operator_type)
      case (1)
         allocate(type_interp_operator::op)
      end select
      call op%configure(settings, field_manager)
      op%previous => final_operator
      final_operator => op
   end subroutine

end module