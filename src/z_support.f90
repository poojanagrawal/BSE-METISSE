module z_support
    use track_support
    implicit none

    character(len=strlen) :: format_file, key_columns_file, INPUT_FILES_DIR
    logical :: read_files_from_Z, read_eep_files

    character(len=strlen) :: Z_folder_list
    !format_specifications
    character(len=5):: file_extension
    integer :: header_location, eep_location
    character(len=256) :: column_name_file
    type(column), allocatable :: key_cols(:), temp_cols(:)
    integer :: total_cols
    integer :: extra_cols = 3

   character:: extra_char

    !used to set star_type_from_history
    ! central limits for high- / intermediate-mass stars, set these from input eep_controls nml
    real(dp) :: center_gamma_limit = 1d2
    real(dp) :: center_carbon_limit = 1d-4
    real(dp) :: log_center_T_limit = 9d0
    real(dp) :: high_mass_limit = 1d1 !Msun
    real(dp) :: very_low_mass_limit = 0.5d0 !Msun
    real(dp) :: he_core_mass_limit = 2.2
    real(dp) :: T_bgb_limit = 3.8
!    real(dp) ::  Mup_core,Mec_core
    integer, allocatable :: m_cutoff(:)

    type critical_mass
        integer :: loc
        real(dp) :: mass
    end type critical_mass

    type(critical_mass), allocatable :: Mcrit(:)
    character(len=strlen), allocatable :: track_list(:)

    real(dp) :: mass, max_age, min_mass, max_mass

    namelist /format_controls/ file_extension, read_eep_files,header_location, column_name_file, &
            PreMS_EEP, ZAMS_EEP, IAMS_EEP, TAMS_EEP, BGB_EEP, cHeIgnition_EEP, &
            cHeBurn_EEP, TA_cHeB_EEP, TPAGB_EEP, cCBurn_EEP, post_AGB_EEP, WD_EEP, &
            Initial_EEP, Final_EEP, Extra_EEP1 ,Extra_EEP2, Extra_EEP3, &
            age_colname, mass_colname, log_L_colname ,log_T_colname, &
            log_R_colname, he_core_mass, c_core_mass, &
            log_Tc, c12_mass_frac, o16_mass_frac,he4_mass_frac, &
            Lum_colname,Teff_colname,Radius_colname, total_cols,&
            extra_char, eep_location,low_mass_final_eep, high_mass_final_eep, &
            log_mdot_colname, mdot_colname, he_core_radius, co_core_radius,&
            mass_conv_envelope, radius_conv_envelope, moment_of_inertia

    namelist /SSE_input_controls/ initial_Z, max_age,read_mass_from_file,&
                            input_mass_file, number_of_tracks, max_mass, min_mass, &
                            WD_mass_scheme,BHNS_mass_scheme, max_NS_mass, &
                            use_initial_final_mass_relation, pts_1, pts_2, pts_3
                            
    namelist /extra_controls/ METISSE_DIR, INPUT_FILES_DIR, read_files_from_Z,&
                        Z_folder_list, format_file, key_columns_file, &
                        Mhook, Mhef, Mfgb, Mup, Mec, Mextra, Z_H, Z_He, fix_track, &
                        lookup_index, accuracy_limit, construct_wd_track, allow_electron_capture, &
                        verbose, write_eep_file, write_track_to_file
            
    contains

    subroutine read_metisse_input()
    
        !reading defaults option first
        open(100, file = 'defaults/evolve_metisse_defaults.in' )
            read(unit = 100, nml = SSE_input_controls)
            read(unit = 100, nml = extra_controls)
        close(100)

        !reading user input
        open(10,FILE='evolve_metisse.in',action="read")
            if (direct_call) read(unit = 10, nml = SSE_input_controls)
            read(unit = 10, nml = extra_controls)
        close(10)
         
    end subroutine
    
    subroutine get_files_from_path(path)
    character(len=strlen),intent(in) :: path
    character(LEN=strlen) :: str,eep_list
    integer ::n,i,ierr
        ierr = 0
        if (verbose) print*,"Reading input files from: ", trim(path)
        eep_list='find '//trim(path)//'/*'//trim(file_extension)//' -maxdepth 1 > .file_name.txt'
        call system(eep_list,ierr)
        if (ierr/=0) then
        print*,'Problem reading input files'
        print*,'check if INPUT_FILES_DIR is correct'
        STOP
        end if

        open(20,FILE='.file_name.txt',action="read")

        !count the number of tracks
        n=0
        do while(.true.)
            read(20,*,iostat=ierr)
            if(ierr/=0) exit
            n = n+1
        end do

        allocate(s(n))
        rewind(20)
        ierr=0
        do i = 1,n
            read(20,'(a)',iostat=ierr)str
            if (ierr/=0) exit
            s(i)% filename = trim(str)
        end do
        num_tracks = n
        close(20)
    end subroutine get_files_from_path

    !TODO: -- NEEDS to be checked
    subroutine get_folder_from_Z(INPUT_FILES_DIR,Z_value,fold_path)
    character(LEN=strlen),intent(in) :: INPUT_FILES_DIR
    real(dp),intent(in) :: Z_value
    character(LEN=strlen),intent(out) :: fold_path

    character(LEN=strlen) :: folder_list
    character(LEN=50) :: name
    integer :: ierr,flag
    real(dp):: metallicity,v_div_vcrit

        ierr = 0
        v_div_vcrit = 0d0
        flag = 1

        folder_list = trim(INPUT_FILES_DIR)//trim(Z_folder_list)
        open(50,FILE=trim(folder_list),action="read",iostat=ierr)

        do while(.true.)
            read(50,*,iostat=ierr) name, metallicity, v_div_vcrit
            if (ierr/=0) exit
            if (abs(Z_value-metallicity) < 1.0d-4) then     !TODO: relative error instead of absolute error?
                fold_path = trim(INPUT_FILES_DIR)//'/'//trim(name)
                !if(debug) print*,fold_path
                flag=0
                exit
            endif
        end do

        if (flag/=0) then
        print*, Z_value,'metallicity value not found'
        STOP
        endif

        if (ierr/=0) then
        print*,'Erorr locating folders'
        print*,'check if Z_value and Z_folder_list are correct'
        STOP
        endif
        close(50)
        
    return
    end subroutine get_folder_from_Z

    subroutine read_format(filename)
        integer :: ierr, io
        character(len=strlen), intent(in) :: filename

        io=alloc_iounit(ierr)
        !read file format specs
        open(unit=io,file=trim(filename),status='old',action='read',iostat=ierr)
            if(ierr/=0)then
                print*,'Erorr reading format file'
                print*,'check if format_file is correct'
                STOP
                return
            endif
            read(unit = io, nml = format_controls)
        close(io)
        call free_iounit(io)

    end subroutine read_format


    !locating essential columns here
    subroutine locate_column_numbers(s,cols)
        type(column), intent(in) :: cols(:)
        type(eep_track) :: s(:)
        logical :: essential

        essential = .true.

        i_age2 = locate_column(cols, age_colname, essential)
        i_age = s(1)% ncol+1
        i_mass = locate_column(cols, mass_colname, essential)

        if (log_L_colname /= '') then
            !find the log luminosity column
            i_logL = locate_column(cols, log_L_colname, essential)

        else
            !find the luminosity column and convert it into log
            i_lum = locate_column(cols, Lum_colname, essential)
            call make_logcolumn(s, i_logL)
        endif
        

        if (log_R_colname/= '') then
            i_logR = locate_column(cols, log_R_colname, essential)
        else
            i_logR = locate_column(cols, Radius_colname, essential)
            call make_logcolumn(s, i_logR)
        endif

        i_he_core = locate_column(cols, he_core_mass, essential)
        i_co_core = locate_column(cols, c_core_mass, essential)
        
        !TODO: - make log_T optional, Teff will get calculated in the code
        if (log_T_colname/= '') then
            i_logTe = locate_column(cols, log_T_colname, essential)
        else
            i_logTe = locate_column(cols, Teff_colname, essential)
            call make_logcolumn(s, i_logTe)
        endif
        
        essential  = .false.
        
        !optional columns
        !TODO: if core radius is in log units?

        i_RHe_core = -1
        i_RCO_core = -1

        if (he_core_radius/= '') i_RHe_core = locate_column(cols, he_core_radius)
        if (co_core_radius/= '') i_RCO_core = locate_column(cols, co_core_radius)

        i_mcenv = -1
        if (mass_conv_envelope/= '') i_mcenv = locate_column(cols, mass_conv_envelope)

        i_Rcenv = -1
        if (radius_conv_envelope/= '') i_rcenv = locate_column(cols, radius_conv_envelope)

        print*, 'mcenv, rcenv columns',i_mcenv, i_Rcenv

        i_MoI = -1
        if (moment_of_inertia/= '') i_MoI = locate_column(cols, moment_of_inertia)

        i_he4 = locate_column(cols, he4_mass_frac)
        i_c12 = locate_column(cols, c12_mass_frac)
        i_o16 = locate_column(cols, o16_mass_frac)
        i_Tc = locate_column(cols, log_Tc)
        !add high mass limit- , if min mass is less than that,
        !ask to provide i_Tc or Mup, mhook whatever
        !some additional ones - if ever needed
            ! i_gamma=locate_column(cols,'center_gamma')
            !i_surf=locate_column(cols,'surface_h1')
            !i_logLHe=locate_column(cols,'log_LHe')
            !i_logLH=locate_column(cols,'log_LH')
            ! i_Rhoc=locate_column(cols,'log_center_Rho')
            !i_h1=locate_column(cols,'center_h1')
            !i_he4=ilocate_column(cols,'center_he4')
            ! i_logg=ilocate_column(cols,'log_g')

!        if (log_mdot_colname/= '') then
!            i_mdot = locate_column(cols, log_mdot_colname)
!            call make_pow10column(s,i_mdot,"Mdot")
!        elseif (mdot_colname/= '') then
!             i_mdot =  locate_column(cols, mdot_colname)          !star_mdot
!        !            call make_logcolumn(s, i_mdot)
!        else
!             i_mdot = -1
!        endif


!        if (i_RHe_core > 0) number_of_core_columns = number_of_core_columns+1
!        if (i_RCO_core > 0) number_of_core_columns = number_of_core_columns+1

        number_of_core_columns = 5
        allocate(core_cols(number_of_core_columns))
        core_cols = -1
        core_cols(1) = i_logL
        core_cols(2) = i_he_core
        core_cols(3) = i_co_core

        if (i_RHe_core > 0) core_cols(4) = i_RHe_core
        if (i_RCO_core > 0) core_cols(5) = i_RCO_core
    end subroutine locate_column_numbers

    integer function locate_column(cols,colname,essential)
        character(len=col_width), intent(in) :: colname
        logical, intent(in),optional :: essential
        logical :: essential1

        type(column) :: cols(:)
        integer :: i

        !if it is not provided whether a column is essential or not,
        !then assume that it is not essential
        essential1 = .false.
        if (present(essential)) essential1 =essential

        !now find the column
        locate_column = -1
        if (trim(colname)=='') return
        do i=1,size(cols)
           if(adjustl(adjustr(cols(i)% name))==trim(colname)) then
              locate_column = i
              return
           endif
        enddo

        !check whether the column has been successfully located
        if(locate_column<0) then
            write(0,*) 'locate_column, could not find column: ', trim(colname)
            !STOP the code if cannot locate one of the essential columns
            if(essential1) STOP
        endif
        
    end function locate_column
      
    subroutine make_logcolumn(s, itemp)
        type(eep_track) :: s(:)
        integer :: itemp,k
        do k = 1, size(s)
            s(k)% tr(itemp,:) = log10(s(k)% tr(itemp,:))
            s(k)% cols(itemp)% name = "log("//trim(s(k)% cols(itemp)% name)//")"
            !what about key columns
        end do
    end subroutine make_logcolumn
    
    subroutine make_pow10column(s, itemp,newname)
        type(eep_track) :: s(:)
        integer :: itemp,k
        character(len=col_width), intent(in), optional :: newname
        do k = 1, size(s)
            s(k)% tr(itemp,:) = 10.d0**(s(k)% tr(itemp,:))
            if (present(newname)) s(k)% cols(itemp)% name = trim(newname)
        end do
    end subroutine make_pow10column

    !reading column names from file - from iso_eep_support.f90
    subroutine process_columns(filename,cols,ierr)
        character(len=strlen), intent(in) :: filename
        integer, intent(out) :: ierr
        integer :: i, io, ncols(2), nchar, column_length, pass
        character(len=strlen) :: line, column_name
        logical :: is_int,verbose1
        type(column), allocatable, intent(out) :: cols(:)
        integer :: ncol

        verbose1 =.false.
        ierr = 0
        io = alloc_iounit(ierr)
        open(io,file=trim(filename),action='read',status='old',iostat=ierr)
        if(ierr/=0) then
           write(*,*) 'failed to open columns list file: ', trim(filename)
           call free_iounit(io)
           return
        endif
        ncols=0
        do pass=1,2
           if(pass==2) allocate(cols(ncols(1)))
           inner_loop: do while(.true.)
              is_int = .false.
              read(io,'(a)',iostat=ierr) line
              if(ierr/=0) exit inner_loop

              !remove any nasty tabs
              do while(index(line,char(9))>0)
                 i=index(line,char(9))
                 line(i:i)=' '
              enddo

              nchar=len_trim(line)
              if(nchar==0) cycle inner_loop ! ignore blank line

              line=adjustl(line)
              i=index(line,'!')-1
              if(i<0) i=len_trim(line)

              if(i==0) then       !comment line
                 if(verbose1) write(*,*) ' comment: ', trim(line)
                 cycle inner_loop
              else if(index(line(1:i),'number')>0) then
                 if(verbose1) write(*,*) '****** ', trim(line)
                 if(verbose1) write(*,*) '****** index of number > 0 => integer'
                 is_int = .true.
              else if(index(line(1:i),'num_')==1)then
                 if(verbose1) write(*,*) '****** ', trim(line)
                 if(verbose1) write(*,*) '****** index of num_ == 1 => integer'
                 is_int = .true.
              endif

              column_name = line
              ncols(pass)=ncols(pass)+1
              if(i==0) then
                 column_length=len_trim(column_name)
              else
                 column_length=len_trim(column_name(1:i))
              endif
              do i=1,column_length
                 if(column_name(i:i)==' ') column_name(i:i)='_'
              enddo
              !if(verbose1) write(*,'(2i5,a32,i5)') pass, ncols(pass),trim(column_name(1:column_length)), column_length
              if(pass==2) then
                 cols(ncols(pass))% name = trim(column_name(1:column_length))
                 if(is_int) then
                    cols(ncols(pass))% type = column_int
                 else
                    cols(ncols(pass))% type = column_dbl
                 endif
                 cols(ncols(pass))% loc = ncols(pass)
              endif
           end do inner_loop
           if(pass==1) rewind(io)
           if(pass==2) close(io)
        end do
        if(ncols(1)==ncols(2)) then
           ierr=0
           ncol=ncols(1)
        endif
        call free_iounit(io)
        if(verbose1) write(*,*) 'process_columns: ncol = ', ncol

      end subroutine process_columns

    !adapted from read_history_file of iso_eep_support
    subroutine read_input_file(x)
        type(eep_track), intent(inout) :: x
        character(len=8192) :: line
        integer :: i, io, j,ierr
        real(dp), allocatable :: temp_tr(:,:)
        logical :: debug

        ierr = 0
        debug = .false.

        if (debug) print*,"in read_input_file",x% filename
        io = alloc_iounit(ierr)
        open(unit=io,file=trim(x% filename),status='old',action='read')
        !read lines of header as comments

        if (header_location >0)then
            do i = 1,header_location-1
                read(io,*) !header
            end do
            allocate(temp_cols(total_cols))
            !get column names
            read(io,'(a)') line
            do i =1, total_cols
                j = scan(line," ")
                temp_cols(i)% name = line(1:j)
                if (trim(temp_cols(i)% name)==extra_char) then
                    line = adjustl(line(j:))
                    j = scan(line," ")
                    temp_cols(i)% name = line(1:j)
                endif
                line = adjustl(line(j:))
            end do
        endif
        !figure out how many data lines
        j=0
        do while(.true.)
            read(io,*,iostat=ierr)
            if(ierr/=0) exit
            j=j+1
        enddo

        x% ntrack = j

        rewind(io)

        !ignore file header, already read it once
        do i=1,header_location
           read(io,*) !header
        enddo

        allocate(temp_tr(total_cols, x% ntrack))

        do j=1, x% ntrack
            read(io,'(a)') line
            call split(line, temp_tr(:,j), total_cols)
        enddo

        close(io)
        call free_iounit(io)

        !store only key_columns if defined
        if (size(key_cols) >1) then
            x% ncol = size(key_cols)
            allocate(x% tr(x% ncol, x% ntrack),x% cols(x% ncol))
            do j = 1, x% ncol
                i = locate_column(temp_cols, key_cols(j)% name)
                x% cols(j)% name = key_cols(j)% name
                x% tr(j,:) = temp_tr(i,:)
                !print*,x%cols(j)% name,x%tr(j,1)
            end do
        else
            x% ncol = total_cols
            allocate(x% tr(x% ncol, x% ntrack),x% cols(x% ncol))
            x% cols% name = temp_cols% name
            x% tr = temp_tr
        endif

        deallocate(temp_tr)
        if(header_location >0) deallocate(temp_cols)

        x% neep = count(key_eeps .le. x% ntrack,1)
        allocate(x% eep(x% neep))
        x% eep = pack(key_eeps,mask = key_eeps .le. x% ntrack)
        !print*,x% eep
        i = locate_column(x% cols, mass_colname)
        x% initial_mass = x% tr(i,1)
        x% initial_Z = initial_Z

        if (debug) print*,x% initial_mass, x% initial_Z, x% ncol
    end subroutine read_input_file

    !from C.Flynn's driver routine

    subroutine split(line,values,ncol)
    character(len=*) :: line
    real(dp) :: values(:)
    integer:: i,ncol, iblankpos
    line = adjustl(line)
        do i =1, ncol
            !print*,i,trim(line)
            iblankpos = scan(line," ")
            if (trim(line)/= '') read(line(1:iblankpos),*) values(i)
            !print*, values(i)
            line = adjustl(line(iblankpos:))
        end do
    end subroutine split

    subroutine read_key_eeps()
    integer :: temp(15), neep,ieep
        temp = -1

        ieep = 1
        temp(ieep) = PreMS_EEP
        if(add_eep(temp,ieep)) ieep=ieep+1

        temp(ieep) = ZAMS_EEP
        if(add_eep(temp,ieep)) ieep=ieep+1

        temp(ieep) = IAMS_EEP
        if(add_eep(temp,ieep)) ieep=ieep+1

        temp(ieep) = TAMS_EEP
        if(add_eep(temp,ieep)) ieep=ieep+1

        temp(ieep) = BGB_EEP
        if(add_eep(temp,ieep)) ieep=ieep+1

        temp(ieep) = cHeIgnition_EEP
        if(add_eep(temp,ieep)) ieep=ieep+1

        temp(ieep) = cHeBurn_EEP
        if(add_eep(temp,ieep)) ieep=ieep+1

        temp(ieep) = TA_cHeB_EEP
        if(add_eep(temp,ieep)) ieep=ieep+1

        temp(ieep) = cCBurn_EEP
        if(add_eep(temp,ieep)) ieep=ieep+1

        temp(ieep) = TPAGB_EEP
        if(add_eep(temp,ieep)) ieep=ieep+1

        temp(ieep) = post_AGB_EEP
        if(add_eep(temp,ieep)) ieep=ieep+1

        temp(ieep) = WD_EEP
        if(add_eep(temp,ieep)) ieep=ieep+1

        temp(ieep) = Extra_EEP1
        if(add_eep(temp,ieep)) ieep=ieep+1

        temp(ieep) = Extra_EEP2
        if(add_eep(temp,ieep)) ieep=ieep+1

        temp(ieep) = Extra_EEP3
        if(.not. add_eep(temp,ieep)) temp(ieep) = -1

    ! TODO -- locate if final eep is in the list of eeps

    neep = count(temp > 0,1)
    allocate(key_eeps(neep))
    key_eeps = pack(temp,temp > 0)
    
     !define initial and final eep if not already defined
    if (Initial_EEP < minval(key_eeps))  Initial_EEP = ZAMS_EEP
    if (Final_EEP < 0 .or. Final_EEP > maxval(key_eeps))  Final_EEP = maxval(key_eeps)
    
    end subroutine

    logical function add_eep(temp, i)
    integer :: last, i, temp(:)
        last = 0
        add_eep = .false.
        if (i>1 ) last = temp(i-1)
        if (temp(i) >0 .and. temp(i)<= Final_EEP .and. temp(i)/= last) then
            add_eep = .true.
        endif
    end function

  subroutine read_eep(x)      !from iso/make_track.f90
    type(eep_track), intent(inout) :: x
    !type(column), allocatable :: temp_cols(:)
    real(dp), allocatable :: temp_tr(:,:)
    integer :: ierr, io, j, i

    !logical, optional :: full_path
    logical :: read_phase !use_full_path, binfile_exists
    character(len=8) :: phase_info
    character(len=strlen) :: eepfile!, binfile
    character(len=10) :: type_label
    read_phase = .false.

    eepfile = trim(x% filename)

    io=alloc_iounit(ierr)
    open(io,file=trim(eepfile),status='old',action='read',iostat=ierr)

    !check if the file was opened successfully; if not, then fail
    if(ierr/=0) then
       x% ignore=.true.
       write(*,*) '  PROBLEM OPENING EEP FILE: ', trim(eepfile)
       close(io)
       call free_iounit(io)
       return
    endif

    read(io,'(25x,a8)') x% version_string
    read(io,'(25x,i8)') x% MESA_revision_number
    read(io,*) !comment line
    read(io,*) !comment line
    read(io,'(2x,f6.4,1p1e13.5,0p3f9.2)') x% initial_Y, x% initial_Z, x% Fe_div_H, x% alpha_div_Fe, x% v_div_vcrit
    read(io,*) !comment line
    read(io,*) !comment line
    read(io,'(2x,1p1e16.10,3i8,a8,2x,a10)') x% initial_mass, x% ntrack, x% neep, total_cols, phase_info, type_label

    call set_star_type_from_label(type_label,x)

    if(index(phase_info,'YES')/=0) then
       x% has_phase = .true.
       allocate(x% phase(x% ntrack))
       total_cols = total_cols - 1
    else
       x% has_phase = .false.
       total_cols = total_cols
    endif

    allocate(temp_tr(total_cols, x% ntrack), temp_cols(total_cols))
    allocate(x% eep(x% neep))

    read(io,'(8x,299i8)') x% eep
    read(io,*) ! comment line
    read(io,*) ! column numbers

    !to exclude pms -read from eep(2)


    read(io,'(1x,299a32)') temp_cols% name
    do j=x% eep(1), x% ntrack
        read(io,'(1x,299(1pes32.16e3))') temp_tr(:,j)
    enddo

    close(io)
    call free_iounit(io)
    
    if (size(key_cols)>1) then
    !storing selected columns only
        x% ncol = size(key_cols)
        allocate(x% tr(x% ncol, x% ntrack), x% cols(x% ncol))

        do j = 1, x% ncol
            i = locate_column(temp_cols, key_cols(j)% name)
            x% cols(j)% name = key_cols(j)% name
            x% tr(j,:) = temp_tr(i,:)
        end do
    else
        x% ncol = total_cols
        allocate(x% tr(x% ncol, x% ntrack),x% cols(x% ncol))
        x% cols% name = temp_cols% name
        x% tr = temp_tr
    endif

    deallocate(temp_tr, temp_cols)
  end subroutine read_eep

  subroutine set_star_type_from_history(t)
    type(eep_track), intent(inout) :: t
    integer :: n

    !Todo: replace t with x, make this a function

    !set the WDCS primary EEP if center_gamma < center_gamma_limit
    !center_gamma_limit = 19

    !set the CarbonBurn primary EEP if center_c12 < center_carbon_limit
    center_carbon_limit = 0.05

    !set star_type to high_mass_star if max(log_center_T) > this
    log_center_T_limit = 8.5

    !set star_type to high mass star if M_init >= this
    high_mass_limit = 10.0 !Msun

    !from Pols et al. 1998- set star_type to high mass star if He core mass>= this
    he_core_mass_limit = 2.2 !Msun

    n = t% ntrack
    
    !only reach center_gamma_limit if the star evolves to a WD
    !if( t% tr(i_gamma,n) > center_gamma_limit) then
       !t% star_type = star_low_mass
       !return
    !endif

    !simple test for high-mass stars is that central C is depleted
    if(maxval(t% tr(i_c12,:)) > 0.4d0 .and. t% tr(i_c12,n) < center_carbon_limit)then
       t% star_type = star_high_mass
       return
    endif

    !i_he_core
    if (t% tr(i_he_core,n)>= he_core_mass_limit) then
        t% star_type = star_high_mass
    else
        t% star_type = star_low_mass
    endif

    !alternative test for high-mass stars is that they reach a
    !central temperature threshhold
    if (i_Tc >0) then
        if(t% tr(i_Tc,n) > log_center_T_limit)then
            t% star_type = star_high_mass
        else
            t% star_type = star_low_mass
        endif
    endif

    !last gasp test for high-mass stars is the initial mass...
    if(t% initial_mass >= high_mass_limit) then
       t% star_type = star_high_mass
    else
       t% star_type = star_low_mass
    endif

  end subroutine set_star_type_from_history

    logical function check_mass_loss(x)
    type(eep_track), intent(in) :: x
    real(dp) :: dm

    dm = (x% tr(i_mass,1)-x% tr(i_mass,x% ntrack))/x% initial_mass
    if (dm<tiny) then
        check_mass_loss = .false.
    else
        check_mass_loss = .true.
    endif
    end function



    subroutine set_star_type_from_label(label,s)
        character(len=10), intent(in) :: label
        type(eep_track), intent(inout) :: s
        integer :: n,i
        n=size(star_label)
        do i=1,n
            if(label==star_label(i)) s% star_type = i
        enddo
    end subroutine set_star_type_from_label

    subroutine set_zparameters(zpars)
        real(dp), intent(out) :: zpars(20)
        real(dp) :: old_co_frac,co_fraction,change_frac
        real(dp) :: smass,Teff,last_val,he_diff
        real(dp), allocatable :: T_centre(:)
        integer :: len_track, i, min_index
        integer:: j_bagb, j_tagb, i_start
        real(dp), allocatable :: mass_list(:)

        logical:: debug

        debug = .true.
        old_co_frac = 0.0
        Mup_core = 0.0
        Mec_core = 0.0

        allocate(Mcrit(9))
        Mcrit% mass= -1.0
        Mcrit% loc = 0

        Mcrit(1)% mass = s(1)% initial_mass
        Mcrit(1)% loc = 1

        Mcrit(2)% mass = 0.75 !  TODO: -- MheWd ?
        Mcrit(3)% mass = Mhook
        Mcrit(4)% mass = Mhef
        Mcrit(5)% mass = Mfgb
        Mcrit(6)% mass = Mup
        Mcrit(7)% mass = Mec
        Mcrit(8)% mass = Mextra

        Mcrit(9)% mass = s(num_tracks)% initial_mass
        Mcrit(9)% loc = num_tracks+1 !TODO: explain why+1?

        if (.not. defined(Mcrit(7)% mass)) then
          do i = 1,size(s)
            call set_star_type_from_history(s(i))
          end do
        endif

        allocate(mass_list(num_tracks))
        mass_list = s% initial_mass

        !if already defined, do index search here otherwise search below
        do i = 2, size(Mcrit)-1
            if (.not. defined(Mcrit(i)% mass)) cycle
            call index_search (num_tracks, mass_list, Mcrit(i)% mass, min_index)
            Mcrit(i)% mass = s(min_index)% initial_mass
            Mcrit(i)% loc = min_index
            print*, i, Mcrit(i)% mass
        end do

        i_start = max(Mcrit(1)% loc, Mcrit(2)% loc)
        do i = i_start, num_tracks
            smass = s(i)% initial_mass
            !print*,smass, s(i)% star_type
            len_track = s(i)% ntrack
            if (smass<=3.0) then
                !determining Mhook
                if (.not. defined(Mcrit(3)% mass))then
                    if (len_track >= TAMS_EEP) then
                    !T_centre = s(i)%tr(i_Tc,IAMS_EEP:TAMS_EEP)
                    allocate(T_centre,source=s(i)% tr(i_Tc,IAMS_EEP:TAMS_EEP))

                    last_val = T_centre(size(T_centre))
                    if (maxval(T_centre)>last_val) then
                        Mcrit(3)% mass = smass
                        Mcrit(3)% loc = i
                        if (debug) print*,"Mhook",smass,i
                    endif
                    deallocate(T_centre)

                    endif
                endif

                !determining Mhef
                if (.not. defined(Mcrit(4)% mass))then
                if (len_track>=TA_cHeB_EEP) then
                    allocate(T_centre,source=s(i)% tr(i_Tc,cHeIgnition_EEP:TA_cHeB_EEP-1))
                    !T_centre = s(i)% tr(i_Tc,cHeIgnition_EEP:TA_cHeB_EEP-1)
                    if (minval(T_centre)>7.4) then
                        Mcrit(4)% mass = smass
                        Mcrit(4)% loc = i
                        if (debug) print*,"Mhef",smass,i
                    endif
                    deallocate(T_centre)
                endif
                endif

            else        !if (smass>=3.0) then
                !determining Mup
                if ((.not. defined(Mcrit(6)% mass)) .and. smass<8.0) then
!                    j_tagb = min(len_track,cCBurn_EEP,TPAGB_EEP)         !end of agb  min(cCBurn,TPAGB)
                    if (i_c12>0 .and. i_o16 >0) then
                        j_tagb = min(cCBurn_EEP,TPAGB_EEP)
                        co_fraction = s(i)% tr(i_c12,j_tagb)+s(i)% tr(i_o16,j_tagb)
                        if (old_co_frac>0.0) then
                            change_frac = abs(co_fraction-old_co_frac)
                            change_frac = change_frac/old_co_frac
                            if (change_frac>0.01) then
                                Mcrit(6)% mass = smass
                                Mcrit(6)% loc = i
                                if (debug) print*,"Mup",smass,i
                            endif
                        endif
                    old_co_frac = co_fraction
                    endif
                endif
            endif

            !determining Mfgb- all masses
            if (.not. defined(Mcrit(5)% mass))then
                if (smass<=20.0 .and. len_track>TAMS_EEP) then
                    Teff = s(i)%tr(i_logTe,cHeIgnition_EEP-1)       !temp at the end of HG/FGB
                    he_diff = abs(s(i)% tr(i_he4, cHeIgnition_EEP-1)-s(i)% tr(i_he4, TAMS_EEP))
!                    print*,"bgb",smass,Teff, he_diff
                    if (Teff> T_bgb_limit ) then !.or. he_diff >0.001
                        Mcrit(5)% mass = smass
                        Mcrit(5)% loc = i
                        if (debug) print*,"Mfgb",smass,i
                    endif
                endif
            endif

            !determining Mec- can be based on co mass in the end>1.4 maybe
            if (.not. defined(Mcrit(7)% mass))then
                if (s(i)% star_type == star_high_mass) then
                Mcrit(7)% mass = smass
                Mcrit(7)% loc = i
                if (debug) print*,"Mec",smass,i
                endif
            endif

        end do

        !if (.not. defined(Mup))then
           ! Mup = Mec-1.8
            !call index_search(num_tracks,s% initial_mass,Mup,min_index)
            !m_cutoff(7) = min_index
            !Mup = s(min_index)% initial_mass
       ! end if

        !If the tracks are beyond the zpars limits, above procedure
        !picks up the first or second track, which can lead to errors later,
        !hence those values need to be reverted

        do i = 2,size(Mcrit)-1
            if (Mcrit(i)% mass <= Mcrit(1)% mass) then
                Mcrit(i)% mass= -1.d0
                Mcrit(i)% loc = 0
            endif
        end do

        Mcrit(7)% loc = max(Mcrit(7)% loc-1,1)
        j_bagb = min(s(Mcrit(7)% loc)% ntrack,TA_cHeB_EEP)
        Mec_core = s(Mcrit(7)% loc)% tr(i_he_core,j_bagb)
        
        !if cannot locate Mup or located it beyond Mec (which is incorrect),

        if (Mcrit(6)% loc < 1 .or. Mcrit(6)% loc > Mcrit(7)% loc) then
            !modify Mup by SSE way
            if (debug) print*, 'Mcrit(6)/Mup not located, defaulting to Mup= Mec-1.8'
            Mcrit(6)% mass = Mcrit(7)% mass - 1.8d0
            call index_search (num_tracks, mass_list, Mcrit(6)% mass, Mcrit(6)% loc)
            Mcrit(6)% loc = min(Mcrit(6)% loc,Mcrit(7)% loc-1)
            Mcrit(6)% mass = s(Mcrit(6)% loc)% initial_mass
            Mcrit(6)% loc = Mcrit(6)% loc +1 !one is reduced later for normal cases
print*,Mcrit(6)% mass,Mcrit(6)% loc
        endif

        Mcrit(6)% loc = max(Mcrit(6)% loc-1,1)

        j_bagb = min(s(Mcrit(6)% loc)% ntrack,TA_cHeB_EEP)
        Mup_core = s(Mcrit(6)% loc)% tr(i_he_core,j_bagb)

!        Mup_core = 1.7816
!        Mec_core = 2.3660
        if (debug) print*,"Mup_core", Mup_core
        if (debug) print*,"Mec_core", Mec_core


        call sort_mcutoff()
        if (debug) print*, "m_cutoffs: ", m_cutoff
    
        !now redefine zpars where applicable
        do i = 3,7
        if (defined (Mcrit(i)% mass)) zpars (i-2) = Mcrit(i)% mass
        end do

        if (defined(Z_H)) zpars(11) = Z_H
        if (defined(Z_He)) zpars(12) = Z_He
        Z04 = zpars(14)

        if (debug) print*, 'zpars',  zpars(1:5)

!        call sort(Mcrit% loc, m_cutoff)
    end subroutine set_zparameters

    subroutine sort_mcutoff()
     !subroutine to sort Mcutoffs, removing the ones who are at less than 2 distance from the last one
    !making sure there are at least 2 tracks between subsequent mcutoff
    
!        integer:: mloc(:)
        integer, allocatable :: mloc(:)
        integer :: val,n
        integer :: i, k, loc,a
        
        n = size(Mcrit)
        allocate (m_cutoff(n),mloc(n))
        mloc = Mcrit% loc
        m_cutoff = 0
        m_cutoff(1) = 1
        k=1
        
        do i = 1, n
            val = minval(mloc(i:n))
            a = minloc(mloc(i:n),dim=1)
            loc = (i - 1) + a
            mloc(loc) = mloc(i)
            mloc(i) = val
            if ((val - m_cutoff(k))>=2) then
                k=k+1
                m_cutoff(k) = val
            endif
        end do
        
        !   deallocate(mloc)
        !        allocate(mloc(size(m_cutoff)))
        !        mloc = m_cutoff
        
        m_cutoff = pack(m_cutoff,mask = m_cutoff .ne. 0)
    end subroutine sort_mcutoff

    subroutine sort(mloc, list)
        integer, intent(in) :: mloc(:)
        integer, allocatable, intent(out) :: list(:)
        integer :: val,n
        integer :: i, a, loc

        n = size(mloc)
        allocate(list(n))
        list=pack(mloc,mask = mloc .ne. 0)
        
        !sort array
        do i = 0, n-1
            val = minval(list(i:n-1))
            a = minloc(list(i:n-1),dim=1)
            loc = (i - 1) + a
            list(loc) = list(i)
            list(i) = val
        end do
    end subroutine sort

    !ZPARS
    !finds critical masses and their locations
    !1; M below which hook doesn't appear on MS, Mhook. 3
    !2; M above which He ignition occurs non-degenerately, Mhef. 4
    !3; M above which He ignition occurs on the HG, Mfgb. 5
    !4; M below which C/O ignition doesn't occur, Mup. 6
    !5; M above which C ignites in the centre, Mec. 7

    subroutine calculate_sse_zpars(z,zpars)

    real(dp),intent(in) :: z
    real(dp),intent(out) :: zpars(14)
    real(dp) :: lzs,dlzs,lz,lzd

        lzs = log10(z/0.02d0)
        dlzs = 1.d0/(z*log(10.d0))
        lz = log10(z)
        lzd = lzs + 1.d0

        zpars = 0.d0

        zpars(1) = 1.0185d0 + lzs*(0.16015d0 + lzs*0.0892d0)
        zpars(2) = 1.995d0 + lzs*(0.25d0 + lzs*0.087d0)
        zpars(3) = 16.5d0*z**0.06d0/(1.d0 + (1.0d-04/z)**1.27d0)
        zpars(4) = MAX(6.11044d0 + 1.02167d0*lzs, 5.d0)
        zpars(5) = zpars(4) + 1.8d0
        zpars(6) = 5.37d0 + lzs*0.135d0
!        zpars(7) = c(1) + lzs*(c(2) + lzs*(c(3) + lzs*(c(4) + lzs*c(5))))
!        zpars(8) = MAX(0.95d0,MAX(0.95d0-(10.d0/3.d0)*(z-0.01d0),
!        &           MIN(0.99d0,0.98d0-(100.d0/7.d0)*(z-0.001d0))))
        !CALL star(kw,zpars(2),zpars(2),tm,tn,tscls,lums,GB,zpars)
        !zpars(9) = mcgbf(lums(3),GB,lums(6))
        !zpars(10) = mcgbf(lums(4),GB,lums(6))
        !* set the hydrogen and helium abundances
        zpars(11) = 0.76d0 - 3.d0*z
        zpars(12) = 0.24d0 + 2.d0*z
        !* set constant for low-mass CHeB stars
        !zpars(13) = rminf(zpars(2))/
        !&            rgbf(zpars(2),lzahbf(zpars(2),zpars(9),zpars(2)))

        zpars(14) = z**0.4d0

    end subroutine
end module z_support
