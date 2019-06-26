! -----------------------------------------------------------------------------
! This file is part of FlexOut: a flexible output manager written in
! object-oriented Fortran supporting - presently - text and NetCDF formats.
!
! Official repository: https://github.com/BoldingBruggeman/flexout
!
! Copyright 2019-2019 Bolding & Bruggeman ApS.
!
! This is free software: you can redistribute it and/or modify it under
! the terms of the GNU General Public License as published by the Free Software
! Foundation (https://www.gnu.org/licenses/gpl.html). It is distributed in the
! hope that it will be useful, but WITHOUT ANY WARRANTY; without even the
! implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
! A copy of the license is provided in the COPYING file.
! -----------------------------------------------------------------------------

program test_flexout

   use yaml_version, only: yaml_commit_id=>git_commit_id, &
                           yaml_branch_name=>git_branch_name
   use yaml_types
   use yaml
   use flexout_version, only: flexout_commit_id=>git_commit_id, &
                              flexout_branch_name=>git_branch_name
   use output_manager
   use, intrinsic :: iso_fortran_env

   character(error_length) :: error
   character(256) :: path
   class (type_node),pointer :: root

   write(*,*)
   write(*,*) 'YAML:    ',yaml_commit_id,' (',yaml_branch_name,' branch)'
   write(*,*) 'flexout: ',flexout_commit_id,' (',flexout_branch_name,' branch)'
   write(*,*)


   call get_command_argument(1, path)
   if (path=='') then
      write (*,*) 'ERROR: path to YAML file not provided.'
      stop 2
   end if
   root => parse(path,unit=100,error=error)
   if (error/='') then
      write (*,*) 'PARSE ERROR: '//trim(error)
      stop 1
   end if
   call root%dump(unit=output_unit,indent=0)

end program
